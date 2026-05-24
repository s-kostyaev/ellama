# AGENTS.md – Agentic Coding Quick Reference

## Git Workflow

Before committing, use the project-local `commit-message` skill.

Never merge, rebase, cherry-pick, or otherwise integrate code into `main`.
Never push directly to `main`; always use a feature branch and a pull request.

For release work, use the project-local `changelog` skill.

## Code Style Guidelines

- Keep `require`s grouped at the file top.
- Use `kebab-case`; symbols should be prefixed with `ellama-`, or
  `ellama-{subpackage}-` in subpackages.  In `ellama-tools.el` tool specs use
  `snake_case`.
- Wrap risky calls in `(condition-case err ...)` and signal with `(error "msg")`
  when appropriate.
- Comments use `;;` for buffer comments.

## Operation Guidelines

1. ALWAYS use available tools to make user requested action. Carefully fill all
   required fields.
2. Double quotes escaping: use one backslash (`\`) before quotes.
3. ALWAYS use the `oq` skill to inspect `.org` files such as `README.org` and
   `NEWS.org`; they are large enough that structure-first queries are required.
