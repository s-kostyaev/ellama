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

## Safety Notes

- DLP internal errors default to fail-open unless configured otherwise.
- Redaction failures fall back to `block`.
- Output `warn` currently passes content through in v1 and relies on telemetry.
- Large payloads are truncated to the configured scan size for detection and
  logging.
