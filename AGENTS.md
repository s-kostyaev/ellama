# AGENTS.md – Agentic Coding Quick Reference

## Automated Edit Hooks

Project-local edit hooks are configured in `.dir-locals.el` and implemented in
`.codex/hooks/`.

- The pre-edit hook blocks edits on `main`; create and switch to a feature
  branch before retrying.
- The post-edit hook runs the relevant project checks for changed Elisp and
  Org files.  Follow the hook output when a check fails.

## Git Workflow

After making changes and before committing, use the project-local
`commit-message` skill to write the commit message from the final diff:
`.codex/skills/commit-message/SKILL.md`.

For release work, use the project-local `changelog` skill, update the version in
`ellama.el`, commit generated release changes with the exact commit message
`Bump version`, and push.

## Code Style Guidelines

- Keep `require`s grouped at the file top.
- Use 2-space indentation, no trailing whitespace, line length <= 80 chars.
- Prefer symbols (`'my-symbol`) and strings; keyword arguments for optional
  params.
- Use `kebab-case`; symbols should be prefixed with `ellama-`, or
  `ellama-{subpackage}-` in subpackages.  In `ellama-tools.el` tool specs use
  `snake_case`.
- Wrap risky calls in `(condition-case err ...)` and signal with `(error "msg")`
  when appropriate.
- Function docstrings use base verb form, for example `contain` and `return`.
- Comments use `;;` for buffer comments.

## Operation Guidelines

1. ALWAYS use available tools to make user requested action. Carefully fill all
   required fields.
2. Double quotes escaping: use one backslash (`\`) before quotes.
3. ALWAYS use the `oq` skill to inspect `.org` files such as `README.org` and
   `NEWS.org`; they are large enough that structure-first queries are required.
