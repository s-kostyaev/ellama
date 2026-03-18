# Implementation Plan: LLM Safety Checks for DLP

## Objective

Implement the optional LLM-based safety detector described in
`docs/llm_safety_checks_spec.md` as a block-only semantic backstop for the
existing DLP pipeline.

The implementation must:

- preserve deterministic regex and exact-secret behavior
- keep redaction fully deterministic
- let the LLM detector log only in `monitor`
- let the LLM detector force `block` only in `enforce`
- fail safely without leaking reviewed payloads

## Current Integration Targets

Primary code paths:

- `ellama-tools-dlp.el`
  - `ellama-tools-dlp--validate-finding`
  - `ellama-tools-dlp--make-finding`
  - `ellama-tools-dlp--policy-action`
  - `ellama-tools-dlp--apply-enforcement`
  - `ellama-tools-dlp--log-scan-decision`
  - `ellama-tools-dlp--log-scan-error`
  - `ellama-tools-dlp--detect-findings`
  - `ellama-tools-dlp--scan-text`
- `ellama.el`
  - structured-output call patterns in `ellama-semantic-similar-p`
  - structured-output call patterns in `ellama-extract-string-list`
- tests
  - `tests/test-ellama-tools-dlp.el`
  - `tests/test-ellama-tools.el`

Recommendation: implement the feature entirely inside `ellama-tools-dlp.el`
first, then add wrapper-level tests only where tool flow coverage is required.

## Design Constraints

- Do not route the safety check through `ellama-chat`.
- Do not reuse the active chat session or conversation history.
- Do not allow tools in the checker request; pass `:tools nil`.
- Do not let LLM findings participate in redaction span calculation.
- Do not add a second rollout mode; reuse `ellama-tools-dlp-mode`.
- Do not make `risk` directly control enforcement in v1.

## Proposed Internal Shape

Keep deterministic findings as they are today.  Add a separate LLM result
object that is evaluated after deterministic policy but before final verdict
construction.

Recommended internal result shape for the LLM helper:

- `:unsafe` boolean
- `:category` string
- `:risk` symbol or string, optional
- `:reason` string
- `:raw-valid` boolean for parse/validation success

Recommended scan result flow inside `ellama-tools-dlp--scan-text`:

1. Prepare payload
2. Collect deterministic findings
3. Compute deterministic configured action
4. Decide LLM eligibility from deterministic state
5. Run LLM check when eligible
6. Compute final action with block-only override
7. Build final verdict with existing enforcement rules
8. Log deterministic and LLM metadata in sanitized form

Recommendation: keep the current `:findings` list as the deterministic source
of truth for redaction, and only append `llm` findings to the verdict/log path
after redaction is no longer relevant.

## Implementation Phases

Progress status (as of 2026-03-01):

- Done: Phase 0, Phase 1, Phase 2, Phase 3, Phase 4, Phase 5, Phase 6
- Validation note: providers without `json-response` capability now skip the
  LLM detector with visible telemetry and fail open.
- Validation note: `make test` passes in batch with 268/268 tests green.

## Phase 0: Configuration and Schema (Done)

Goal:

- add the configuration surface and extend the data model without changing
  runtime behavior when the feature is disabled

Tasks:

1. Add defcustoms in `ellama-tools-dlp.el`:
   - `ellama-tools-dlp-llm-check-enabled`
   - `ellama-tools-dlp-llm-provider`
   - `ellama-tools-dlp-llm-directions`
   - `ellama-tools-dlp-llm-max-scan-size`
   - `ellama-tools-dlp-llm-tool-allowlist`
   - `ellama-tools-dlp-llm-template`
   - `ellama-tools-dlp-llm-run-policy`
2. Extend `ellama-tools-dlp--validate-finding` to accept detector `llm`.
3. Keep span rules unchanged:
   - deterministic findings may include spans
   - `llm` findings must keep `:match-start` and `:match-end` as nil
4. Add small helpers for LLM config normalization:
   - direction membership
   - allowlist matching
   - provider fallback resolution

Acceptance criteria:

- the module loads with the new variables present
- existing tests still pass with the feature disabled
- `ellama-tools-dlp--make-finding` accepts `:detector 'llm`

## Phase 1: Request Builder and Response Parser (Done)

Goal:

- implement a strict, isolated structured-output call path

Tasks:

1. Add a prompt builder helper, for example:
   - `ellama-tools-dlp--llm-check-prompt`
2. The prompt builder should:
   - include direction, tool name, arg name, and normalized payload
   - instruct the model to classify only
   - state that no tools are available
   - request JSON only
3. Add a parser/validator helper, for example:
   - `ellama-tools-dlp--parse-llm-check-result`
4. Validate the response contract:
   - `unsafe` must be boolean
   - `category` must be a non-empty string
   - `reason` must be a string
   - `risk` may be nil or a supported value
5. Add the execution helper:
   - `ellama-tools-dlp--llm-check-text`
6. `ellama-tools-dlp--llm-check-text` should:
   - call `llm-chat` directly
   - use `llm-make-chat-prompt`
   - pass `:tools nil`
   - parse with `json-parse-string`
   - wrap all risky operations in `condition-case`

Acceptance criteria:

- the helper returns a validated internal result object
- malformed JSON and invalid fields are handled without throwing past the
  helper
- no payload text is emitted in error messages or incidents

## Phase 2: Eligibility and Deterministic Prepass (Done)

Goal:

- make LLM execution decisions from deterministic state before final verdict

Tasks:

1. Add an eligibility helper, for example:
   - `ellama-tools-dlp--llm-check-eligible-p`
2. Eligibility should require:
   - feature enabled
   - supported direction
   - string payload
   - payload not truncated by the global limit
   - payload within the LLM-specific size limit
   - tool allowed by allowlist
   - provider available and suitable
3. Add a deterministic-only policy prepass inside `ellama-tools-dlp--scan-text`.
4. Evaluate `ellama-tools-dlp-llm-run-policy` from deterministic state:
   - `clean-only`
   - `always-unless-blocked`
5. Skip the LLM call when the deterministic action is already `block`.

Acceptance criteria:

- `ellama-tools-dlp--scan-text` can decide eligibility without changing
  existing deterministic actions
- skip reasons are stable and can be logged
- no LLM call happens when deterministic policy already blocks

## Phase 3: Block-Only Override and Verdict Construction (Done)

Goal:

- apply LLM output as a separate block-only override

Tasks:

1. Add a small override helper, for example:
   - `ellama-tools-dlp--apply-llm-override`
2. Override behavior:
   - if deterministic action is `block`, keep `block`
   - if LLM result is safe, keep deterministic action
   - if LLM result is unsafe and mode is `monitor`, keep deterministic action
   - if LLM result is unsafe and mode is `enforce`, force `block`
3. Keep redaction deterministic:
   - do not feed `llm` findings into the span-merging path
   - do not let `llm` findings change `warn` vs `redact`
4. When useful, attach a synthetic `llm` finding only after the action is
   known, for logging and safe user-facing rule reporting.
5. Reuse `ellama-tools-dlp--format-safe-message` for block messages by passing
   a combined finding list only after redaction decisions are complete.

Acceptance criteria:

- unsafe LLM output can force `block` in `enforce`
- unsafe LLM output cannot trigger `warn`
- mixed deterministic plus LLM output cannot break redaction

## Phase 4: Telemetry and Incident Logging (Done)

Goal:

- add enough sanitized visibility to tune the feature safely

Tasks:

1. Add LLM-specific incident helpers, for example:
   - `ellama-tools-dlp--log-llm-check-run`
   - `ellama-tools-dlp--log-llm-check-skip`
   - `ellama-tools-dlp--log-llm-check-error`
2. Record only sanitized metadata:
   - timestamp
   - direction
   - tool name
   - arg name
   - payload length
   - provider label when safe
   - skip reason
   - parse/error type
   - returned unsafe flag
   - returned category
   - returned risk
3. Ensure current `scan-decision` logging remains coherent when an LLM override
   changes the final action.
4. Decide whether `scan-decision` should include extra fields:
   - `:llm-ran`
   - `:llm-unsafe`
   - `:llm-category`
   - `:llm-overrode`

Acceptance criteria:

- skip, run, and error paths are visible in incident logs
- no incident log stores the raw reviewed payload
- no incident log stores the raw model response

## Phase 5: Test Coverage (Done)

Goal:

- cover the new behavior at both unit and wrapper levels

Unit tests in `tests/test-ellama-tools-dlp.el`:

1. feature disabled does not call `llm-chat`
2. `llm` findings pass finding validation
3. unsupported direction is skipped
4. oversized payload is skipped
5. truncated payload is skipped
6. `clean-only` skips when deterministic findings exist
7. `always-unless-blocked` still runs when deterministic action is not `block`
8. deterministic `block` skips the LLM check
9. `:tools nil` is passed in the LLM request path
10. valid unsafe response forces `block` in `enforce`
11. valid unsafe response does not change the action in `monitor`
12. safe response preserves deterministic `allow`, `warn`, or `redact`
13. malformed JSON falls back safely
14. invalid schema fields fall back safely
15. LLM findings do not participate in redaction span calculation
16. provider errors log sanitized `llm-check-error`

Wrapper-level tests in `tests/test-ellama-tools.el`:

1. blocked input from the LLM path prevents tool execution
2. blocked output from the LLM path returns the existing safe block message
3. async tool output still applies the LLM block path correctly

Implementation note:

- Stub `llm-chat` with `cl-letf` and return JSON strings directly.
- Avoid live-provider tests in the unit suite.

Acceptance criteria:

- tests cover both `monitor` and `enforce`
- tests cover both input and output directions
- tests prove that output redaction still works for deterministic findings

## Phase 6: Rollout and Validation (Done)

Goal:

- land the feature with conservative defaults and predictable operator control

Tasks:

1. Keep `ellama-tools-dlp-llm-check-enabled` defaulting to `nil`.
2. Keep `ellama-tools-dlp-llm-run-policy` defaulting to `clean-only`.
3. Document recommended initial rollout:
   - enable only for a small tool allowlist first
   - run global DLP in `monitor` to collect incidents
   - review false positives before switching to `enforce`
4. Validate provider support:
   - disable or skip on providers without reliable structured output
5. After implementation, run:
   - `make test`
   - `make test-detailed` if needed

Acceptance criteria:

- default configuration is low risk
- unsupported providers fail open with visible telemetry
- maintainers can tune rollout from incidents before enabling enforcement

## Suggested Delivery Order

Recommended patch sequence:

1. Configuration and schema
2. Request builder and parser
3. Eligibility and deterministic prepass
4. Block-only override
5. Telemetry helpers
6. Unit tests
7. Wrapper-level tests
8. Documentation touch-up if code shape differs from this plan

This sequence keeps the highest-risk semantic change, the block-only override,
isolated until the supporting helpers and tests are already in place.
