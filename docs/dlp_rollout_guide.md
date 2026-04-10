# DLP Rollout Guide for Ellama Tools

## Goal

Enable and tune `ellama` tool DLP safely with minimal disruption.

This guide assumes the DLP implementation is present in `ellama-tools-dlp.el`
and integrated through `ellama-tools.el`.

## Recommended Initial Rollout (Monitor Mode)

Start in monitor mode and collect incidents before enabling enforcement:

```elisp
(setq ellama-tools-dlp-enabled t)
(setq ellama-tools-dlp-mode 'monitor)
(setq ellama-tools-dlp-log-targets '(memory))
```

Optional: also send incidents to `*Messages*` while tuning.

```elisp
(setq ellama-tools-dlp-log-targets '(memory message))
```

## Tuning Workflow

1. Reset runtime state before a tuning session.
2. Exercise common tool workflows (read/write files, shell, grep, task, etc.).
3. Inspect recent incidents.
4. Review aggregated stats by rule/tool/action.
5. Add scoped overrides for noisy tools/args.
6. Re-run and compare incident stats.
7. Move selected paths to `enforce`.

Helpers:

- `M-x ellama-tools-dlp-reset-runtime-state`
- `M-x ellama-tools-dlp-show-incident-stats`
- `(ellama-tools-dlp-recent-incidents)`
- `(ellama-tools-dlp-incident-stats)`
- `(ellama-tools-dlp-incident-stats-report)`

## Example Scoped Overrides

Structured input arguments are scanned recursively. Nested string values use
path-like arg names (for example `content.items[0].token`) in DLP context and
incidents. Override `:arg` matches exact arg names and nested path prefixes, so
`"content"` matches `content.items[0].token`.

Allow a noisy input argument for a specific tool:

```elisp
(setq ellama-tools-dlp-policy-overrides
      '((:tool "shell_command"
         :direction input
         :arg "cmd"
         :except t)))
```

Warn on a sensitive tool input while leaving global defaults unchanged:

```elisp
(setq ellama-tools-dlp-policy-overrides
      '((:tool "write_file"
         :direction input
         :arg "content"
         :action warn)))
```

Target a specific nested structured path (exact match):

```elisp
(setq ellama-tools-dlp-policy-overrides
      '((:tool "write_file"
         :direction input
         :arg "content.items[0].token"
         :action block)))
```

Block output for a specific tool in enforce mode:

```elisp
(setq ellama-tools-dlp-policy-overrides
      '((:tool "read_file"
         :direction output
         :action block)))
```

## Suggested Enforcement Progression

1. `monitor` globally with logging enabled.
2. Tune regex rules and overrides until false positives are manageable.
3. Switch to `enforce` for low-risk, high-confidence paths first.
4. Prefer output `redact` over `block` when usability matters.
5. Keep `input warn` for ambiguous cases that need human confirmation.

## Autonomous Agent Configuration

Autonomous agents need a low-friction policy.  Prompting on every ordinary
tool call trains users to approve blindly, which is worse than a smaller set of
high-signal prompts.  The recommended shape is:

- clean read/write operations inside the approved workspace run automatically
- secret leaks and prompt-injection-like output are redacted or blocked
- ambiguous dangerous actions require typed user confirmation or fail closed
- high-confidence destructive actions are blocked, not prompted
- unknown MCP tools warn until they are classified

`ellama-tools-allow-all` only bypasses the normal confirmation wrapper.  It
does not bypass DLP, irreversible-action checks, output scanning, or `srt`
filesystem checks because DLP wraps the confirmation layer.  This makes
`allow-all` suitable only when DLP enforcement is enabled and tool filesystem
access is constrained.

Recommended baseline for autonomous coding:

```elisp
(with-eval-after-load 'ellama-tools
  ;; Let clean calls run without repetitive prompts.
  (setq ellama-tools-allow-all t)

  ;; Required safety layer.
  (setq ellama-tools-dlp-enabled t)
  (setq ellama-tools-dlp-mode 'enforce)

  ;; Prefer fail-closed behavior for autonomous operation.
  (setq ellama-tools-dlp-input-fail-open nil)
  (setq ellama-tools-dlp-output-fail-open nil)

  ;; Keep secret and output protections active.
  (setq ellama-tools-dlp-scan-env-exact-secrets t)
  (setq ellama-tools-dlp-input-default-action 'warn)
  (setq ellama-tools-dlp-output-default-action 'redact)

  ;; Irreversible actions require stronger intent.
  (setq ellama-tools-irreversible-enabled t)
  (setq ellama-tools-irreversible-default-action 'warn)
  (setq ellama-tools-irreversible-require-typed-confirm t)

  ;; Keep telemetry durable enough to tune.
  (setq ellama-tools-dlp-log-targets '(memory file))

  ;; Strongly recommended when `ellama-tools-allow-all' is enabled.
  (setq ellama-tools-use-srt t)
  (setq ellama-tools-srt-args
        '("--settings" "~/.config/ellama/srt-autonomous.json")))
```

Use an `srt` policy that allows writes only inside the current project and a
scratch area.  Add explicit read/write denials for secrets and audit logs:

```json
{
  "filesystem": {
    "allowWrite": [
      "/path/to/project",
      "/tmp/ellama-agent"
    ],
    "denyRead": [
      "~/.ssh",
      "~/.gnupg",
      "~/.authinfo",
      "~/.aws",
      "~/.config/gh",
      "~/.emacs.d/ellama-dlp-audit.jsonl"
    ],
    "denyWrite": [
      "~/.ssh",
      "~/.gnupg",
      "~/.authinfo",
      "~/.aws",
      "~/.config/gh",
      "~/.emacs.d/ellama-dlp-audit.jsonl"
    ]
  }
}
```

There are two reasonable policies for dangerous cases:

```elisp
;; User-handled dangerous cases.
(setq ellama-tools-dlp-input-default-action 'warn)
(setq ellama-tools-dlp-output-warn-behavior 'confirm)
(setq ellama-tools-irreversible-default-action 'warn)
(setq ellama-tools-irreversible-require-typed-confirm t)
```

This asks only for DLP warnings and irreversible actions.  Medium-risk
irreversible actions require the typed confirmation phrase
`I UNDERSTAND THIS CANNOT BE UNDONE`.  High-confidence destructive actions are
still blocked in `enforce` mode before session or project overrides apply.

```elisp
;; Secure fallback for unattended runs.
(setq ellama-tools-dlp-input-default-action 'block)
(setq ellama-tools-dlp-output-warn-behavior 'block)
(setq ellama-tools-irreversible-default-action 'block)
(setq ellama-tools-irreversible-require-typed-confirm t)
```

This is better for unattended agents.  The agent receives a denial string and
must find a safer path.  It avoids turning user confirmation into a routine
approval habit.

Reduce prompt fatigue by tightening tool roles instead of weakening safety.
Avoid `:tools :all` for autonomous subagents.  Prefer separate roles such as:

- read-only explorer without `shell_command`
- coder with file tools and sandboxed `shell_command`
- bash role only when a task explicitly needs shell access
- no `ask_user` tool unless user interruption is intentional

For MCP tools, keep `ellama-tools-irreversible-unknown-tool-action` as `warn`
initially.  After observing common safe tools, classify trusted tool identities
with `ellama-tools-irreversible-tool-risk-overrides` or trusted project
overrides.  Use `ellama-tools-dlp-add-session-bypass` only for narrow
tool identities and short TTLs; do not disable DLP globally to reduce prompts.

If `srt` is not available, do not use global `ellama-tools-allow-all` for
autonomous coding.  Use a small `ellama-tools-allowed` list of safe read-only
tools instead, and keep mutating tools behind confirmation or DLP fallback.

## Tool Output Line Budget Guard

Ellama tools also apply a per-tool-output line budget before output is sent back
to the main model. This is separate from DLP detector decisions.

Defaults:

- `ellama-tools-output-line-budget-enabled` = `t`
- `ellama-tools-output-line-budget-max-lines` = `200`
- `ellama-tools-output-line-budget-max-line-length` = `4000`
- `ellama-tools-output-line-budget-save-overflow-file` = `t`

Behavior:

- output beyond the line budget is truncated and replaced with a notice block
- hyper-long lines are truncated and marked with `...[line truncated]`
- notice tells the agent that content was truncated and how to continue
  (`lines_range`, `grep_in_file`, `grep`)
- if source path is known (for example `read_file`, `lines_range`,
  `grep_in_file`), the notice references that path
- if source path is unknown, full output is saved to a temp file and the notice
  includes this filename (when overflow-file saving is enabled)

## Safety Notes

- DLP internal errors default to fail-open unless configured otherwise.
- Redaction failures fall back to `block`.
- Output `warn` currently passes content through in v1 and relies on telemetry.
- Large payloads are truncated to the configured scan size for detection and
  logging.
