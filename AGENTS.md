# AGENTS.md – Agentic Coding Quick Reference

## Build / Lint / Test

1. **Build**: `make build`
2. **Run unit tests** (ERT): `make test`
		If it fails, run `make test-detailed` to see fail reasons.
3. **Check native compilation warnings**: `make check-compile-warnings`
4. **Format Elisp files**: `make format-elisp`
5. **Export manual**: `make manual`
6. **Refill readme**: `make refill-readme`

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

3. Do not use git, util requested by user explicitly.
4. ALWAYS use `oq` skill to see content of `.org` files (for example
   ./README.org and ./NEWS.org) isntead of `read_file` tool. README.org and
   NEWS.org are big enough, you NEED to use `oq` skill with it.
