# Implementation Plan: Irreversible Action Safety for Tool Calls

## Objective

Add low-friction safety controls for unsafe irreversible tool actions, including
MCP tools registered through `ellama-tools-define-tool`.

The implementation must:

- detect likely irreversible operations before execution
- keep rollout adoption-friendly (`monitor` first for hard blocks)
- block only a small high-confidence destructive set in enforce mode
- require stronger human intent signals only for irreversible actions
- preserve existing DLP and confirmation mechanics

## Decision Updates (2026-03-18)

Resolved decisions from review:

- Reuse the existing DLP rollout flag (`ellama-tools-dlp-mode`) for monitor vs
  enforce behavior.  Do not add a second mode flag.
- Enable DLP by default (`ellama-tools-dlp-enabled` = `t`) so irreversible
  checks are active without extra user setup.
- Keep irreversible logic in the same DLP pipeline.  Do not add a parallel
  irreversible-only enforcement engine.
- `ellama-tools-allow-all` must bypass only manual confirmation wrappers.  It
  must not bypass irreversible/DLP checks.
- MCP tool identity should derive from MCP hub metadata:
  - `mcp-hub-get-all-tool :categoryp t` sets `:category` to `mcp-<server-name>`
  - tool `:name` remains `tool-name`
  - effective identity should be `mcp-<server-name>/<tool-name>`
- Irreversible actions should be prevented by default via either:
  - hard block for high-confidence destructive patterns, or
  - explicit user confirmation for all remaining irreversible matches.
- `ellama-tools-irreversible-enabled` default is `t`.
- Monitor mode semantics for irreversible findings:
  - still run detectors
  - still require explicit confirmation for irreversible warnings
  - do not hard-block in monitor mode
- Auditability is prioritized over minimum log footprint.
- Audit sink write failures are fail-closed for irreversible actions, with a
  required interactive user confirmation to proceed.
- `srt` is optional hardening, not a hard dependency for irreversible checks:
  - when `ellama-tools-use-srt` is `nil`, irreversible/DLP controls still work
  - when `ellama-tools-use-srt` is `t` and `srt` is missing/misconfigured,
    fail closed for affected shell/file tool calls with actionable errors
- Async tool results are strings by design, so non-string async output handling
  is out of scope for this feature.
- Policy precedence for irreversible controls:
  1. enforce-mode high-confidence irreversible match => hard `block`
  2. otherwise: `session bypass` > `project override` > `global default`
  3. DLP legacy overrides must not downgrade rule (1)
- Default irreversible action policy: `warn` with explicit confirmation.
- Preferred typed confirmation phrase: `I UNDERSTAND THIS CANNOT BE UNDONE`.
- Noninteractive fallback for irreversible confirmation paths is fail-closed
  (`block`).

## Context

Current wrappers already provide a single enforcement path:

- DLP and confirmation wrapping at tool definition time in `ellama-tools.el`
- tool execution via wrapped function in enabled tool list
- optional incident logging in `ellama-tools-dlp.el`

MCP tools added with `ellama-tools-define-tool` use the same mechanics under
the hood, so this feature should extend the current wrapper path instead of
building a parallel MCP-only security path.

## Reference Map (How To Get Context)

Primary local references:

- `ellama-tools.el`
  - wrapper composition (`ellama-tools-wrap-with-confirm`)
  - tool registration (`ellama-tools-define-tool`)
  - manual confirmation path (`ellama-tools--confirm-call`)
  - shell/file tools and `srt` checks
- `ellama-tools-dlp.el`
  - scan pipeline, policy mapping, and incident logging
- `tests/test-ellama-tools.el`
  - wrapper integration tests
- `tests/test-ellama-tools-dlp.el`
  - DLP detector/policy tests
- `README.org`
  - MCP integration snippet
- MCP package source:
  - `~/.emacs.d/elpa/mcp-20260222.1058/mcp-hub.el`
  - `~/.emacs.d/elpa/mcp-20260222.1058/mcp.el`

Quick inspection commands:

```bash
rg -n "ellama-tools-wrap-with-confirm|ellama-tools-define-tool|ellama-tools--confirm-call" ellama-tools.el
rg -n "ellama-tools-dlp--scan-text|ellama-tools-dlp--policy-action|ellama-tools-dlp--log-scan-decision" ellama-tools-dlp.el
rg -n "mcp-hub-get-all-tool|mcp-make-text-tool" ~/.emacs.d/elpa/mcp-20260222.1058/*.el
```

Expected MCP identity source:

- hub assigns `:category` as `mcp-<server-name>` when `:categoryp t`
- tool `:name` is `tool-name`
- effective identity key: `mcp-<server-name>/<tool-name>`

## Design Principles

- Monitor-first rollout: start with telemetry, not hard blocks.
- High precision over broad recall for hard blocks.
- Scoped controls over global disable switches.
- Keep prompt fatigue low.
- Audit-first observability for decisions and overrides.
- Treat unknown third-party tool semantics conservatively, but do not block by
  default.

## Risk Model

Action classes (input-side):

- `read`: no persistent external state change
- `mutating`: state-changing but usually recoverable
- `irreversible`: destructive or hard-to-recover state change

Recommended defaults:

- known read-only tools: `allow`
- known mutating tools: `warn`
- known irreversible tools: `warn-strong`, with hard block for high-confidence
  destructive cases
- unknown MCP tools: `warn` (not `block`) until classified

Behavior matrix (single source of truth):

| Mode      | Rule class                                  | Decision |
|-----------|---------------------------------------------|----------|
| monitor   | no irreversible finding                     | existing DLP behavior |
| monitor   | irreversible warning-class finding          | `warn-strong` + typed confirmation |
| monitor   | irreversible high-confidence block finding  | `warn-strong` + typed confirmation |
| enforce   | irreversible warning-class finding          | `warn-strong` + typed confirmation |
| enforce   | irreversible high-confidence block finding  | `block` |

## Scope (v1)

In scope:

- input-side detection for irreversible intent
- built-in and MCP tools using shared tool wrapper mechanics
- deterministic irreversible detectors (regex/structured argument signals)
- stronger confirmation for irreversible actions
- scoped bypasses with TTL
- sanitized telemetry for tuning false positives

Out of scope:

- full policy engine for provider/network-level controls
- full transactional rollback orchestration
- deep semantic verification of arbitrary DSLs beyond high-confidence patterns

## Proposed Configuration Surface

Add new customization group (or sub-group) under tool safety:

- `ellama-tools-dlp-enabled` (default `t`)
- `ellama-tools-irreversible-enabled` (default `t`)
- `ellama-tools-irreversible-unknown-tool-action` (`allow` / `warn`)
- `ellama-tools-irreversible-high-confidence-block-rules`
- `ellama-tools-irreversible-require-typed-confirm`
- `ellama-tools-irreversible-scoped-bypass-default-ttl`
- `ellama-tools-irreversible-log-targets` (reuse existing targets when possible)
- `ellama-tools-irreversible-default-action` (`warn` / `block`)

Use `ellama-tools-dlp-mode` as the single rollout mode source for this feature.
Keep `ellama-tools-use-srt` optional (default `nil`) as operational hardening.

## Data Model Extensions

Extend scan/verdict metadata with irreversible-specific fields:

- finding tags:
  - `:risk-class` (`read` / `mutating` / `irreversible`)
  - `:confidence` (`high` / `medium`)
  - `:requires-typed-confirm` boolean
- context tags:
  - `:tool-origin` (`builtin` / `mcp`)
  - `:server-id` (derived from `:category` like `mcp-ddg`)
  - `:tool-identity` stable key (`<category>/<tool-name>` for MCP)
- incident tags:
  - `:decision-id`
  - `:policy-source` (`default` / `override` / `bypass`)
  - `:bypass-id` and expiry metadata

## Detection Strategy

### 1. High-confidence hard-block patterns

Ship a very small set for enforce mode:

- SQL destructive patterns with broad blast radius:
  - `DROP DATABASE`
  - `DROP SCHEMA`
  - `TRUNCATE` without explicit safe target allowlist
  - `DELETE FROM <table>` without `WHERE`
- shell destructive patterns:
  - recursive wipe commands over broad paths
  - destructive database admin command variants

Hard-block only when confidence is high and false-positive risk is low.

### 2. Warning-class patterns

Detect potentially destructive but ambiguous operations and return `warn`:

- SQL `DELETE`, `DROP TABLE`, `ALTER TABLE ... DROP`, bulk update patterns
- API/tool arguments indicating destructive operations (`force`, `purge`,
  `destroy`, `delete_all`, `truncate`)

### 3. Tool classification

Maintain per-tool risk classification:

- built-ins with static defaults
- MCP tools classified by:
  - MCP hub category (`mcp-<server>`) plus tool name
  - user classification prompt on first risky call
  - persisted local decision with TTL/override

## Persistence Options (Pros and Cons)

### 1. Session-only storage

Pros:

- safest by default
- no long-lived stale overrides

Cons:

- repetitive prompts across sessions
- weak for operational tuning

### 2. Project-local storage

Pros:

- contextual behavior per repository/workspace
- team-shared policy possible when committed

Cons:

- policy drift across projects
- risk of unsafe overrides being committed

### 3. User-global storage

Pros:

- lowest repetition for frequent tools
- good operator ergonomics

Cons:

- broad blast radius for mistakes
- harder to reason per-project intent

### Recommended hybrid

- store temporary bypasses in session memory
- store risk classification defaults globally
- allow project-local overrides with explicit precedence and logging

Project override trust model:

- ignore project overrides until the repository is explicitly trusted
- on first load, show a concise summary of override effects and require explicit
  approval before applying them
- trust record must bind to repo root, remote URL, and policy file hash
- if the policy file hash changes, require re-approval before using overrides
- project overrides must never relax enforce-mode high-confidence irreversible
  `block` rules
- project overrides should not change irreversible default handling from `warn`
  to `allow` unless the user explicitly approves that downgrade

Policy precedence order:

1. enforce-mode high-confidence irreversible match => hard `block`
2. session bypass
3. project override
4. global default
5. legacy DLP overrides may tune non-irreversible handling only

## Enforcement UX

### Normal warnings

- single `y/n` confirmation (existing pattern)
- allow per-tool/session approval to reduce repetition

### Irreversible warnings

- require typed confirmation phrase:
  - `I UNDERSTAND THIS CANNOT BE UNDONE`
- show concise impact summary:
  - tool identity
  - extracted target hints
  - matched rule IDs
- never include sensitive payload excerpts in prompt text
- when running noninteractive, do not prompt; return `block` with a clear
  message explaining interactive confirmation is required

### Scoped bypasses

Instead of disabling all safety:

- allow bypass by tool identity and project/session scope
- optional TTL
- explicit reason string
- visible incident log entry

TTL guidance:

- TTL defines how long a bypass remains active before automatic expiry.
- Session-scope bypasses naturally expire at session end.
- Project/global bypasses should have finite TTL by default (for example 1 hour)
  to prevent permanent silent weakening.
- Use `nil` TTL only for explicit, audited long-term policy entries.

## Unknown MCP Policy (Pros and Cons)

Global default (`warn`):

- Pros:
  - consistent safety baseline
  - catches first use of newly added servers
- Cons:
  - may be noisy for trusted read-only servers

Per-project override:

- Pros:
  - adapts to local trust assumptions
  - reduces noise in stable environments
- Cons:
  - inconsistent cross-project behavior
  - harder centralized auditing

Recommended approach:

- global default `warn`
- per-project override support, always audited

## Audit Logging and SRT Hardening

Audit is the priority.  Keep irreversible decision logs durable and separate
from normal tool output paths.

Implementation guidance:

- add file-backed incident sink for irreversible decisions
- include: timestamp, decision-id, tool-identity, action, rule-id(s), policy
  source, bypass metadata, approver signal
- never log raw sensitive payload text
- on audit sink write failure for irreversible actions:
  - fail closed by default
  - require interactive explicit user confirmation to continue
  - log fallback incident to available in-memory/message sink
  - when running noninteractive, do not prompt; return `block`

Operational hardening guidance with `srt`:

- `srt` is optional hardening and is not required for base irreversible safety
- enable `ellama-tools-use-srt`
- keep audit log path outside `filesystem.allowWrite` for tools
- add explicit `filesystem.denyRead` and `filesystem.denyWrite` entries for the
  audit directory so agent tools cannot read or tamper with logs
- keep log writer in core Emacs code path, not in tool path
- if `ellama-tools-use-srt` is enabled but `srt` is unavailable or invalid,
  fail closed for affected shell/file tool calls and emit actionable guidance

Example `srt` policy sketch:

```json
{
  "filesystem": {
    "allowWrite": [
      "/path/to/project",
      "/tmp"
    ],
    "denyRead": [
      "/var/log/ellama-audit",
      "/var/log/ellama-audit/**"
    ],
    "denyWrite": [
      "/var/log/ellama-audit",
      "/var/log/ellama-audit/**"
    ]
  }
}
```

Use a dedicated audit sink path and keep that path outside tool write
allowlists.

## Integration Plan

Primary files:

- `ellama-tools-dlp.el`
  - new detector family and irreversible policy mapping
  - safe message and incident extensions
- `ellama-tools.el`
  - typed-confirm prompt path for irreversible warnings
  - scoped bypass checks before prompting
  - tool identity extraction for MCP provenance
- `tests/test-ellama-tools-dlp.el`
  - detector/policy unit tests
- `tests/test-ellama-tools.el`
  - wrapper flow, typed confirm, bypass behavior tests

## Implementation Phases

## Phase 0: Scaffolding and Flags

Goal:

- add config and plumbing with irreversible checks enabled by default

Tasks:

- [x] Set `ellama-tools-dlp-enabled` default to `t`.
- [x] Add defcustom flags and defaults
  (`ellama-tools-irreversible-enabled` = `t`).
- [x] Add risk metadata fields to findings/verdict context helpers.
- [x] Add incident schema extensions (sanitized only).
- [x] Reuse `ellama-tools-dlp-mode` (no separate irreversible mode variable).
- [x] Implement monitor-mode exception for irreversible findings:
   - warnings still require typed confirmation
   - high-confidence irreversible rules do not hard-block in monitor
- [x] Keep `ellama-tools-use-srt` optional (`nil` default) and document behavior
   when `srt` is unavailable.

Acceptance criteria:

- DLP and irreversible checks are active by default
- monitor mode still avoids hard blocks for irreversible findings
- enabling `ellama-tools-use-srt` without a working `srt` fails closed only for
  affected tool calls with clear remediation text
- tests remain green

## Phase 1: Tool Identity and Classification

Goal:

- consistently identify tool provenance and default risk class

Tasks:

- [x] Add helper to compute stable tool identity:
   - built-in: tool name
   - MCP: derive from `:category` (`mcp-<server>`) + tool name
- [x] Add default built-in risk profile table.
- [x] Add unknown MCP default action (`warn`).

Acceptance criteria:

- calls carry stable tool identity in scan context/incidents
- unknown MCP tools are warned, not blocked

## Phase 2: Deterministic Irreversible Detectors

Goal:

- detect high-confidence destructive intents with low false positives

Tasks:

- [x] Add small high-confidence block rule set.
- [x] Add broader warning rule set for mutating/irreversible intent.
- [x] Map each rule to risk class and confidence.

Acceptance criteria:

- high-confidence test corpus blocks in enforce mode
- ambiguous corpus warns instead of blocks
- high-confidence corpus warns (not blocks) in monitor mode

## Phase 3: Typed Confirmation for Irreversible Actions

Goal:

- require stronger explicit intent only for irreversible warnings

Tasks:

- [x] Extend warn prompt path with typed-confirm branch.
- [x] Keep existing lightweight prompt for non-irreversible warnings.
- [x] Add concise impact summary in prompt text.

Acceptance criteria:

- irreversible warnings require typed phrase
- normal warnings keep existing low-friction UX

## Phase 4: Scoped Bypass with TTL

Goal:

- avoid global disable while reducing repeated friction

Tasks:

- [x] Add scoped bypass store (session/project/tool identity).
- [x] Add TTL and reason fields.
- [x] Evaluate bypass before prompting.
- [x] Ensure bypass never disables irreversible detector execution; it only
  changes final interaction policy for matching scope.
- [x] Add project-override trust gate:
  - do not apply project overrides for untrusted repositories
  - require explicit first-load approval with override summary
- [x] Bind project trust to repo root + remote URL + policy hash, and
  invalidate trust on policy changes.
- [x] Ensure project overrides cannot downgrade enforce-mode
  high-confidence irreversible `block` decisions.

Acceptance criteria:

- bypass applies only to intended scope
- expiry is enforced
- all bypass usage is logged
- untrusted repositories cannot silently apply project overrides
- policy changes trigger re-approval before overrides are used
- high-confidence enforce `block` remains non-downgradeable by project policy

## Phase 5: Durable Audit Sink and Failure Paths

Goal:

- implement mandatory file-backed audit path with explicit fail-closed behavior

Tasks:

- [x] Add file-backed incident sink for irreversible decisions.
- [x] Add stable decision IDs and policy-source metadata to each record.
- [x] Ensure sink write failures are fail-closed for irreversible actions.
- [x] Add interactive override path for sink failure.
- [x] Enforce noninteractive fallback to `block` for sink-failure override
  path.

Acceptance criteria:

- irreversible decisions are durably persisted to file sink
- sink write failure path is deterministic and fail-closed
- noninteractive runs block instead of prompting

## Phase 6: Telemetry and Tuning Surface

Goal:

- make false positives measurable and tuneable

Tasks:

- [x] Extend incident aggregation with:
  - by risk class
  - by rule
  - by tool identity
  - by decision type (`allow`, `warn`, `block`, `bypass`)
- [x] Add stats report section for irreversible controls.

Acceptance criteria:

- operators can identify top noisy rules and tools quickly

## Phase 7: Test Coverage

Goal:

- lock down behavior and prevent regressions

Unit tests (`tests/test-ellama-tools-dlp.el`):

- [x] high-confidence block patterns
- [x] warning-class patterns
- [x] unknown MCP action default
- [x] risk metadata propagation
- [x] sanitized logging (no raw sensitive payload)
- [x] monitor-mode irreversible high-confidence downgrade to `warn-strong`
- [x] enforce-mode irreversible high-confidence `block`
- [x] enforce-mode high-confidence irreversible block cannot be downgraded by
  legacy DLP override
- [x] `ellama-tools-dlp-enabled` defaults to enabled behavior
- [x] enforce-mode high-confidence irreversible `block` cannot be downgraded by
  project override

Wrapper tests (`tests/test-ellama-tools.el`):

- [x] typed confirmation required for irreversible warn
- [x] typed confirmation not required for normal warn
- [x] scoped bypass suppresses prompt in scope only
- [x] bypass expiry restores prompt behavior
- [x] async tools preserve irreversible gating behavior
- [x] `ellama-tools-allow-all` bypasses manual confirm only
- [x] precedence for non-hard-block cases:
  session bypass > project override > global default
- [x] audit sink failure is fail-closed unless interactive confirmation is
  given
- [x] noninteractive irreversible warnings are blocked (no prompt)
- [x] noninteractive audit sink failure path is blocked (no prompt)
- [x] `ellama-tools-use-srt` enabled with missing/invalid `srt` fails closed
  for affected tool calls with actionable error
- [x] untrusted repository project override is ignored
- [x] trusted repository project override is applied in-scope
- [x] project override policy hash change requires re-approval
- [x] project override cannot suppress enforce-mode high-confidence
  irreversible `block`

## Phase 8: Documentation Updates

Goal:

- ensure operator-facing and user-facing docs reflect new safety semantics

Tasks:

- [x] Update `README.org` with:
  - irreversible safety overview
  - typed confirmation behavior
  - scoped bypass behavior and precedence
  - MCP identity/classification behavior
- [x] Add/extend docs for operations:
  - where audit logs live
  - how to tune unknown MCP default action
  - how to use `srt` deny rules to protect audit logs
- [x] Add troubleshooting notes:
  - repeated warnings
  - bypass expiry behavior
  - false-positive reporting workflow

Acceptance criteria:

- README and docs describe runtime behavior accurately
- examples match implemented defaults

## Rollout Strategy

Stage 1:

- feature enabled in `monitor` only
- collect telemetry for false positives
- no hard blocks for irreversible findings
- irreversible findings still require explicit typed confirmation

Stage 2:

- enable enforce for high-confidence block rules only
- keep ambiguous irreversible rules on explicit confirmation (`warn-strong`)

Stage 3:

- tune rules and defaults from telemetry
- gradually classify commonly used MCP tools

Success criteria:

- low disable rate
- low false-positive block rate
- measurable reduction in unsafe irreversible executions
