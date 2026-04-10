# DLP Implementation Plan for Ellama Tool Input/Output

## Objective

Implement a lightweight, configurable DLP layer for `ellama` tool calls that:

- scans tool inputs and outputs
- blocks or redacts sensitive data according to policy
- logs sanitized incidents for tuning
- integrates without breaking existing confirmation flow

This plan implements the requirements in `docs/dlp_requirements.md`.

## Finalized Design Defaults (from requirements)

- DLP wrapper runs before confirmation wrapper
- output `block` returns a string (not a tool-call error)
- input `warn` requires explicit user confirmation
- max scan size = `5 MB` for input and output
- overflow behavior = scan first `5 MB` and log truncation
- exact secret scanning from environment enabled by default (opt-out)
- no file-based secret scanning in v1
- env candidate selection is automatic, heuristic-based, extendable
- DLP internal errors default to fail-open
- redaction failure fails closed to `block`
- default redaction placeholder = `[REDACTED:RULE_ID]`

## Proposed File Layout (v1)

Minimal-impact option:

- `ellama-tools.el` (wrapper integration + customization variables)
- `tests/test-ellama-tools.el` (integration tests)

Preferred maintainable option:

- `ellama-tools-dlp.el` (new module: scanner, policy, normalization, logging)
- `ellama-tools.el` (wrapper integration only)
- `tests/test-ellama-tools.el` (wrapper integration tests)
- `tests/test-ellama-tools-dlp.el` (scanner/policy unit tests)

Recommendation: use the preferred option.

## Architecture Overview

### 1. DLP Core (new module)

Responsibilities:

- normalize strings
- build scan context
- run regex and exact-secret detectors
- evaluate policy
- apply enforcement (`allow`, `warn`, `block`, `redact`)
- emit sanitized incidents

### 2. Tool Wrapper Integration

Wrap tool functions at `ellama-tools` definition time:

1. DLP input scan
2. Existing confirmation flow
3. Tool execution
4. DLP output scan

For async tools:

- wrap callback argument to inspect/redact/block outgoing result string

## Implementation Phases

Progress status (as of 2026-02-26):

- Done: Phase 0, Phase 1, Phase 2, Phase 3, Phase 4, Phase 5, Phase 6, Phase 7,
  Phase 8, Phase 9, Phase 10

## Phase 0: Scaffolding and Customization (Done)

Goal:

- add configuration surface and module skeleton with no behavior change by
  default (except loading definitions)

Tasks:

1. Create `ellama-tools-dlp.el` with package header and `provide`.
2. Add `require` in `ellama-tools.el`.
3. Define customization group, e.g. `ellama-tools-dlp`.
4. Add core toggles and defaults:
   - enable/disable DLP
   - mode: `monitor` / `enforce`
   - max scan size (`5 MB`)
   - fail-open settings (input/output)
   - env exact-secret scanning toggle (default on)
5. Add placeholder format variable (default `[REDACTED:RULE_ID]`).

Acceptance criteria:

- loading `ellama-tools.el` remains successful
- existing tests pass unchanged

## Phase 1: Data Model and Context (Done)

Goal:

- define stable internal structures for scanner results and policy decisions

Tasks:

1. Define scan context plist schema:
   - `:direction` (`input` / `output`)
   - `:tool-name`
   - `:arg-name` (optional)
   - `:payload-length`
   - `:truncated`
2. Define finding schema:
   - `:rule-id`
   - `:detector` (`regex` / `exact-secret`)
   - `:severity` (optional v1)
   - `:match-start`, `:match-end` (for redaction-capable paths)
3. Define verdict schema:
   - `:action` (`allow` / `warn` / `block` / `redact`)
   - `:message` (safe string)
   - `:findings`
   - `:redacted-text` (when applicable)

Acceptance criteria:

- helper constructors/parsers exist
- unit tests for schema helpers (if added)

## Phase 2: Normalization + Scan Size Handling (Done)

Goal:

- normalize payloads consistently and bound processing cost

Tasks:

1. Implement shared normalization function:
   - line-ending normalization
   - remove zero-width/invisible chars
   - NFKC normalization (with fail-open handling)
2. Implement payload truncation helper:
   - keep first `5 MB`
   - mark context `:truncated t`
   - log truncation event (sanitized)
3. Ensure normalization runs once per payload before detectors.

Acceptance criteria:

- unit tests for zero-width and Unicode normalization behavior
- unit tests for truncation + logging metadata

## Phase 3: Regex Detector Engine (Done)

Goal:

- implement configurable regex rules with cached compilation

Tasks:

1. Define regex rule config format:
   - `:id`
   - `:pattern`
   - `:enabled`
   - `:directions` (optional)
   - `:tools` / `:args` constraints (optional)
2. Compile and cache regexes.
3. Run regex matching over normalized payload.
4. Return match spans for redaction-capable outputs.
5. Prevent regex engine failures from leaking payloads in logs.

Acceptance criteria:

- unit tests for rule enable/disable
- unit tests for direction/tool scoping
- unit tests for match span reporting

## Phase 4: Exact Secret Detection (Environment, Heuristic and Extendable) (Done)

Goal:

- implement env-derived exact secret detection with automatic candidate
  selection and pluggable heuristics

Tasks:

1. Build env secret cache loader (default enabled):
   - read process environment
   - de-duplicate values
   - never log raw values
2. Implement heuristic candidate pipeline (extendable):
   - pipeline of predicate/scoring stages over `(env-name, env-value)`
   - configurable thresholds (length, entropy, size)
   - support future custom stages
3. Initial heuristic stages (v1):
   - minimum/maximum length
   - single-line requirement
   - token-like shape / charset check
   - known-token prefix/pattern boost
   - name+value combined signal
   - entropy threshold
   - reject obvious path/list/config values
4. Precompute exact-secret variants for selected candidates:
   - raw
   - `base64`
   - `base64url`
   - `hex`
5. Implement exact matching over normalized payload.
6. Add cache invalidation helper (manual function + optional refresh on demand).

Acceptance criteria:

- unit tests for heuristic accept/reject behavior
- unit tests for encoded variant detection
- no raw env secret values appear in error/log paths

## Phase 5: Policy Evaluation and Enforcement (Done)

Goal:

- map findings + context to actions and safe messages

Tasks:

1. Implement policy evaluator:
   - global mode (`monitor` / `enforce`)
   - per-direction defaults
   - per-tool overrides
   - per-tool + per-arg exceptions
2. Implement enforcement behavior:
   - `allow`: pass through
   - `warn`:
     - input: require explicit user confirmation
     - output: return warning + content behavior per policy (v1 can be warn-only
       message or pass-through; choose one and document)
   - `block`:
     - input: return safe denial string before execution
     - output: return safe denial string
   - `redact`:
     - replace matched spans with `[REDACTED:RULE_ID]`
     - if redaction fails safely, `block`
3. Implement safe message formatting:
   - include tool/direction/rule-id
   - never include matched content

Acceptance criteria:

- unit tests for monitor vs enforce behavior
- unit tests for per-tool/per-arg overrides
- unit tests for block/redact message safety

## Phase 6: Incident Logging / Telemetry (Done)

Goal:

- record actionable, sanitized events for tuning and debugging

Progress update (2026-02-26):

- Implemented sanitized incident recording path with control/escape-char
  sanitization.
- Implemented scan decision incidents with action, configured action, rule IDs,
  detectors, payload length, and truncation metadata.
- Added configurable logging targets (`memory`, `message`) and bounded in-memory
  retention.
- Added helper to inspect recent incidents.

Tasks:

1. Implement sanitized incident event writer:
   - timestamp
   - direction
   - tool
   - arg (optional)
   - action
   - rule IDs
   - payload length
   - truncated flag
2. Sanitize control / escape chars in log strings.
3. Add logging target options:
   - `message`
   - in-memory ring/list buffer (recommended)
4. Add helper to inspect recent DLP incidents.

Acceptance criteria:

- tests proving no raw matched text/secret is logged
- tests for truncation metadata and rule IDs

## Phase 7: Tool Wrapper Integration (Sync Tools) (Done)

Goal:

- apply DLP to all standard sync tool calls through the shared wrapper path

Touchpoints:

- `ellama-tools.el` wrapper functions around:
  - `ellama-tools-wrap-with-confirm`
  - `ellama-tools-define-tool`

Tasks:

1. Add DLP wrapper constructor:
   - wraps original tool function with input/output scanning
2. Apply wrapper order:
   - DLP wrapper first
   - confirmation wrapper second
3. Preserve tool arg metadata/types.
4. Preserve current return conventions (`string`, JSON string, `"done"`, etc.).

Acceptance criteria:

- existing non-DLP tests continue to pass when DLP disabled
- integration tests show DLP enforcement for sync tools

## Phase 8: Async Tool Output Integration (Callback Tools) (Done)

Goal:

- scan async callback results before they reach the model

Tasks:

1. Detect callback-style tool signature usage (first arg function callback).
2. Wrap callback to scan outgoing string result.
3. Apply output actions in callback wrapper:
   - allow/warn/block/redact
4. Preserve callback invocation contract.

Notes:

- v1 can focus on callback result strings.
- Non-string callback payloads may be stringified or skipped by policy.

Acceptance criteria:

- tests covering async tool callback output block/redact
- no callback double-invocation regressions

## Phase 9: ERT Test Suite (Done)

Goal:

- comprehensive coverage for behavior, safety, and compatibility

Progress update (2026-02-26):

- Added DLP core tests for monitor/enforce behavior, per-tool/per-arg policy
  overrides/exceptions, output redaction, and redaction fail-closed behavior.
- Added wrapper integration tests for input `block`, input `warn` confirmation,
- sync/async output `block` and `redact`, sync output `warn` behavior, and DLP
  disabled baseline behavior.
- Added DLP core tests for internal error fail-open/fail-closed behavior.
- Added wrapper integration test covering exact env-secret redaction path.
- Added regression test for regex cache scoping correctness.
- Re-ran full `make test` after integration changes (green, 223/223).

Test areas:

1. Regex detection on input/output.
2. Exact env secret detection:
   - raw
   - base64/base64url/hex
3. Heuristic env filtering:
   - token-like values accepted
   - path/list/noise values rejected
4. Normalization anti-bypass:
   - zero-width
   - Unicode normalization
5. Enforce actions:
   - input `block`
   - input `warn` -> explicit confirmation required
   - output `block` -> safe string returned
   - output `redact` -> placeholder replacement
6. Redaction failure -> `block`.
7. Monitor mode logs without enforcement.
8. Truncation at `5 MB`.
9. Async callback output scanning.
10. No secret leakage in logs/errors.
11. DLP disabled path preserves baseline behavior.

## Phase 10: Rollout and Safety Validation (Done)

Goal:

- enable DLP safely in real usage with minimal disruption

Progress update (2026-02-26):

- Default rollout posture already matches plan (`monitor` mode by default).
- Added runtime reset helper and incident aggregation helper to support tuning
  loops in monitor mode.
- Added rollout/tuning guide with monitor-mode workflow, helper usage, and
  scoped policy override examples (`docs/dlp_rollout_guide.md`).
- Full test suite remains green after rollout-helper additions (`make test`,
  225/225).
- Real-world tuning remains an operational activity, but implementation support
  for rollout is in place.

Steps:

1. Ship with DLP module available and mode default `monitor`.
2. Enable logging/incident inspection for tuning.
3. Tune regex rules and heuristic thresholds based on real incidents.
4. Move selected actions/tools to `enforce`.
5. Re-evaluate false positives before enabling broader enforcement.

## Implementation Order (Recommended)

1. Phase 0-2 (scaffolding, context, normalization) - Done
2. Phase 3 (regex detector) - Done
3. Phase 7 (sync wrapper integration, regex-only) - Done
4. Phase 6 (incident logging hardening) - Done
5. Phase 4 (env exact-secret + heuristics) - Done
6. Phase 5 (full policy/enforcement incl. redact) - Done
7. Phase 8 (async callback integration) - Done
8. Phase 9-10 (tests + rollout validation) - Done

This order gets a working `monitor`/`block` regex MVP integrated early.

## Risks and Mitigations

### False Positives in Tool Content

Risk:

- code snippets / configs may resemble tokens

Mitigation:

- default mode `monitor`
- per-tool/per-arg exceptions
- heuristic env filtering to keep exact-secret set precise

### Performance Regressions on Large Tool Output

Risk:

- `read_file` or shell output near cap may feel slower

Mitigation:

- `5 MB` cap with truncation
- one-pass normalization
- cached regex compilation
- precomputed exact-secret variants

### Behavioral Regressions in Async Tools

Risk:

- callback wrapping can change invocation semantics

Mitigation:

- explicit callback integration tests
- scope v1 async handling to string callback results

### Secret Exposure via Logs or Errors

Risk:

- debugging/logging accidentally leaks matched content

Mitigation:

- centralized sanitized logging helpers
- tests asserting secrets never appear in logs/errors

## Definition of Done (v1)

v1 is complete when:

1. DLP scans tool inputs and outputs via shared wrapper integration.
2. Regex + env exact-secret detection work (including encoded variants).
3. Heuristic env candidate selection is automatic and extendable.
4. `monitor` and `enforce` modes work with safe defaults.
5. Output `block` returns safe string; output `redact` uses
   `[REDACTED:RULE_ID]`.
6. Async callback result scanning works for string callbacks.
7. Incidents are logged without leaking secrets.
8. ERT coverage exists for core safety and compatibility behaviors.
