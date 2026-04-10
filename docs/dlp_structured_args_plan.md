# DLP Plan: Secrets Inside Structured Tool Args

## Problem

Current input DLP enforcement in `ellama-tools.el` scans only top-level string
arguments in `ellama-tools--dlp-input-decision`. Secrets nested inside
structured arguments (plist/alist/list/vector/hash-table) can bypass detection.

## Goal

Extend input DLP coverage to scan string values nested inside structured tool
arguments while preserving current behavior for allow/warn/block decisions.

## Scope (Initial Patch)

- Fix input scanning in `ellama-tools.el`
- Keep `ellama-tools-dlp.el` unchanged unless required
- Preserve existing user-visible DLP actions and messages
- Add tests for nested structures and async callbacks

## Implementation Plan

1. Add a recursive structured-arg walker in `ellama-tools.el`.
   - Traverse common Elisp container types used for tool arguments:
     - plist
     - alist
     - list
     - vector
     - hash-table
   - Skip non-data objects (functions, buffers, processes, etc.)

2. Add path tracking for nested values.
   - Build a stable path string rooted at the declared tool arg name
   - Examples:
     - `payload.user.token`
     - `payload.items[0].secret`
   - Reuse DLP `:arg-name` (string) to carry this path

3. Scan string leaves only.
   - For each string leaf, call `ellama-tools-dlp--scan-text`
   - Use existing scan context with:
     - `:direction 'input`
     - tool name
     - path-aware arg name

4. Preserve current decision semantics in `ellama-tools--dlp-input-decision`.
   - First `block` result returns immediately
   - Otherwise keep first `warn` message
   - Otherwise return `allow`

5. Add traversal safety guards.
   - Cycle detection for recursive/shared structures
   - Max depth and/or max visited nodes to avoid pathological inputs
   - Ignore unsupported object types safely

## Testing Plan

Add ERT coverage in `tests/test-ellama-tools.el` for:

- nested plist secret triggers input `block`
- nested alist secret triggers input `block`
- nested list/vector secret triggers input `block`
- hash-table nested secret triggers input `block`
- async tool + structured arg returns blocked message via callback
- structured arg with no string secrets does not block
- warn path still prompts once and can deny execution

## Design Notes

- Prefer recursive leaf scanning over `(format "%S" value)` serialization.
  - Better path precision in incident logs/messages
  - Lower false-positive risk
  - Easier to scope future overrides by arg path

- Initial patch should scan values, not keys.
  - Key scanning can be evaluated later as a separate change

## Rollout Notes

- Keep DLP mode in `monitor` for initial validation if enabling broadly
- Review incidents for path formatting and false positives before tightening
  enforcement

