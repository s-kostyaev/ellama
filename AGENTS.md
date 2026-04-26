# AGENTS.md – Agentic Coding Quick Reference

## Build / Lint / Test

1. **Build**: `make build`
2. **Run unit tests** (ERT): `make test`
		If it fails, run `make test-detailed` to see fail reasons.
3. **Check native compilation warnings**: `make check-compile-warnings`
4. **Check docstrings and style docs**: `make checkdocs`
5. **Format Elisp files**: `make format-elisp`
6. **Export manual**: `make manual`
7. **Refill readme**: `make refill-readme`

Before committing changes, run all make targets appropriate for the changed
files:

- Always run `make build`, `make test`, `make check-compile-warnings`, and
  `make checkdocs` for Elisp changes.
- Run `make format-elisp` when formatting changed Elisp files.
- Run `make refill-readme` after changing `README.org`.
- Run `make manual` after changing `README.org` or manual generation code.
- Run `make refill-news` after changing `NEWS.org`.
- Run specialized integration targets when touching their area, for example
  `make test-srt-integration` or `make test-srt-integration-linux` for SRT
  integration changes.

After making changes and before committing, use the project-local
`commit-message` skill to write the commit message from the final diff:
`.codex/skills/commit-message/SKILL.md`.

## Git Workflow

1. Check the current branch before starting work. If it is `main`, create a
   new branch before making changes.
2. Observe the code and plan the change. Ask the user for explanations when the
   requested behavior, release scope, or expected workflow is unclear.
3. Make the requested changes.
4. Run all appropriate checks from the Build / Lint / Test section.
5. Fix any findings and rerun the relevant checks. Repeat until the checks pass.
6. Commit the implementation using the project-local `commit-message` skill,
   then push the branch.
7. Continue with additional implementation/check/fix/commit/push iterations
   when the task requires more work.
8. Update documentation after the implementation is stable.
9. Generate the changelog using the project-local `changelog` skill, update the
   version in `ellama.el`, commit those generated release changes with the
   exact commit message `Bump version`, and push.

## Code Style Guidelines

- **Imports**: Keep `require`s grouped at the file top.
- **Formatting**: Use 2‑space indentation, no trailing whitespace, line length ≤80 chars.
- **Types**: Prefer symbols (`'my-symbol`) and strings; keyword arguments for optional params.
- **Naming**: Use `kebab-case`, all symbols in a package should be prefixed with
  `ellama-`, in subpackages `ellama-{subpackage}-`, e.g. `ellama-blueprint-`.
	  - In `ellama-tools.el` in tool spec use `snake_case`.
- **Error handling**: Wrap risky calls in `(condition-case err ...)` and signal with `(error "msg")` when appropriate.
- **Docstrings**: One‑line summary, then optional details; keep under 80 chars
  per line. Do not add empty lines. If there are multiple sentences on one line,
  sentences should be separated by two spaces.
  Use base verb form in function docstrings (no `-s`), e.g. `contain` not
  `contains`, `return` not `returns`.
- **Comments**: Prefix with `;;` for buffer comments; avoid inline `#` clutter.

## Operation Guidelines

1. ALWAYS use available tools to make user requested action. Carefully fill all
   required fields.
2. Double quotes escaping:

"maingame": {
  "day1": {
    "text1": "Tag 1",
     "text2": "Heute startet unsere Rundreise \" Example text\". Jeden Tag wird ein neues Reiseziel angesteuert bis wir.</strong> "
  }
}

Just one backslash (\) in front of quotes.

3. ALWAYS use `oq` skill to see content of `.org` files (for example
   ./README.org and ./NEWS.org) isntead of `read_file` tool. README.org and
   NEWS.org are big enough, you NEED to use `oq` skill with it.
