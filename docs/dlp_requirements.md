# DLP Requirements for Ellama Tool Input/Output

## Goal

Recreate a lightweight DLP layer for `ellama` to inspect and enforce policy on
tool input and output, reducing accidental secret leakage between the model and
tools.

## Scope (v1)

Inline scanning for Ellama tool calls at the tool wrapper layer:

- `tool-input`: arguments passed from LLM -> tool
- `tool-output`: values returned from tool -> LLM

Applies to:

- sync tools (direct return value)
- async/callback tools (wrap callback result before forwarding)

Decisions (current):

- output `block` returns a string (do not signal a tool-call error)
- input `warn` requires explicit user confirmation
- no per-tool/per-arg scan exclusions initially
- exact secret scanning uses environment variables by default (opt-out)
- file-based secret scanning is out of scope for v1
- env secret candidate selection is automatic and heuristic-based (extendable)
- DLP wrapper runs before confirmation wrapper
- default max scan size is `5 MB` for input and output
- default overflow behavior is scan first `5 MB` and log truncation
- DLP internal errors default to fail-open (including `enforce` mode)
- redaction failures fail-closed (`block`)

## Non-Goals (v1)

- Network-level controls (SSRF, DNS, rate limits, data budgets)
- Enterprise DLP features (OCR, EDM/fingerprinting, case management)
- Full document classification workflows

## Integration Point

Implement DLP wrapping in `ellama-tools.el` at the common tool wrapper layer so
all tools are covered consistently:

- `ellama-tools-wrap-with-confirm`
- `ellama-tools-define-tool`

Requirement:

- DLP wrapper must compose with existing confirmation wrapper.
- DLP behavior must be configurable globally and per tool.
- Default wrapper order: DLP first, confirmation second.

## Processing Pipeline (Per Direction)

For each scanned payload (`input` or `output`):

1. Normalize text (anti-bypass)
2. Extract scan targets
3. Run detectors
4. Evaluate policy with context
5. Apply enforcement
6. Emit sanitized incident event/log

## Direction-Specific Requirements

### Tool Input Scanning

Scan tool arguments before tool execution.

Must support context:

- tool name
- argument name
- argument type (string/number/etc.)
- direction = `input`

Enforcement actions (v1):

- `allow`
- `warn`
- `block`

Behavior:

- `block` must prevent tool execution.
- `warn` must require explicit user confirmation before tool execution.

### Tool Output Scanning

Scan tool result before returning it to the model.

Must support:

- sync return values
- async callback string results

Enforcement actions (v1):

- `allow`
- `warn`
- `block`
- `redact`

Behavior:

- `redact` should preserve usability by replacing sensitive fragments with
  placeholders instead of dropping the entire output.
- `block` should return a clear denial message without exposing matched data.

## Detection Requirements (v1)

### 1. Regex Patterns

Support configurable regex-based detectors for common secrets and sensitive
tokens.

Requirements:

- case-insensitive matching support
- rule naming/IDs for reporting
- enable/disable per rule

### 2. Exact Secret Match

Support exact secret leak detection from runtime sources:

- environment variables (enabled by default; opt-out)

Requirements:

- de-duplicate loaded secrets
- minimum length threshold
- never log raw loaded secrets
- configurable env scanning toggle
- automatic env candidate filtering (no allowlist/denylist required in v1)
- heuristic pipeline must be extendable (pluggable checks / scoring stages)
- heuristics should use value shape and context (env name + value), e.g.:
  token-like length/charset, entropy, known prefixes/patterns, single-line
  constraint, and exclusion of obvious config/path values
- configurable thresholds for heuristic stages (length, entropy, size)
- stable behavior when heuristics reject all env vars (scan continues without
  exact env-secret matches)

### 3. Encoded Variant Detection

For exact secrets, detect common encoded forms (v1 minimum):

- raw
- `base64`
- `base64url`
- `hex`

Optional for v1.1:

- `base32`

### 4. Entropy (Optional in v1, Recommended)

Entropy-based detection may be included as a secondary heuristic for high-risk
channels/arguments (e.g., `shell_command`, `content`), but should be disabled
by default or scoped carefully to reduce false positives.

## Normalization Requirements (Anti-Bypass)

Before detection, normalize scanned strings.

v1 minimum:

- remove zero-width/invisible characters
- Unicode normalization (NFKC)
- normalize line endings

Optional v1.1:

- confusable character folding
- combining mark stripping

Normalization must be shared across input and output paths.

## Context and Policy Model

Policy decisions must consider both content and context.

Required context fields:

- direction (`input` / `output`)
- tool name
- argument name (for input)
- payload length
- detector/rule ID

Policy controls (v1):

- global mode
- per-direction defaults
- per-tool overrides
- per-tool + per-arg exceptions/allowlist

## Modes / Rollout

Support staged rollout similar to DLP deployment modes:

- `monitor`: detect + log only (no blocking/redaction)
- `enforce`: apply configured actions

Recommended default for initial deployment:

- `monitor`

## Logging / Incident Telemetry

The system must record sanitized DLP events for tuning and debugging.

Requirements:

- include timestamp, direction, tool, arg (if any), rule ID, action
- do not log raw secrets or full sensitive payloads
- sanitize control/escape characters in logs
- support in-memory buffer and/or standard message logging

## Redaction Requirements

When action = `redact`:

- redact matched spans only (if safe to do so)
- use stable placeholders (default: `[REDACTED:RULE_ID]`)
- if safe partial redaction is not possible, fail closed to `block`

## Error/UX Requirements

Blocked messages should be explicit and actionable without leaking data.

Examples:

- `"DLP policy blocked tool input for shell_command (rule: openai_api_key)"`
- `"DLP policy redacted tool output from read_file"`

## Compatibility Requirements

- Must not break existing tool confirmation flow.
- Must preserve behavior for non-string arguments (skip or stringify by policy).
- Must support tools returning strings and JSON-encoded data.
- Must support callback-based tools that return via function argument.

## Performance Requirements

- Scanning overhead should be small for typical tool payloads.
- Configurable max scan size per payload (default `5 MB`).
- Default overflow behavior: scan only the first `5 MB` and log truncation.
- Avoid repeated recompilation of regex rules (cache compiled rules).

## Security Requirements

- Loaded secrets must not be exposed in logs, errors, or customization buffers.
- DLP failure mode must be configurable:
  - fail-open (log and continue)
  - fail-closed (block)

Recommended v1 default:

- fail-open in `monitor`
- fail-open in `enforce` (configurable per direction)
- if redaction is required and safe redaction fails, fail-closed to `block`

## Testing Requirements

Add ERT coverage for:

- input detection (regex, exact secret, encoded variants)
- output redaction/blocking
- normalization bypass cases (zero-width, Unicode variants)
- per-tool/per-arg exceptions
- monitor vs enforce modes
- async callback tool output scanning
- non-string args/returns compatibility
- no secret leakage in logs/errors

## Suggested V1 Deliverables

1. Core DLP scanner module (normalization + detectors + policy eval)
2. Tool wrapper integration in `ellama-tools.el`
3. Config variables/customization interface
4. Sanitized incident logging
5. ERT test suite for input/output enforcement and redaction

## Open Questions (To Finalize Before Implementation)

No blocking open questions for v1 requirements at this stage.
