#!/bin/sh
set -eu

branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null \
	|| git rev-parse --short HEAD 2>/dev/null \
	|| printf '%s' unknown)

if [ "$branch" = "main" ]; then
	cat <<'EOF'
Edit blocked: current branch is main.

Instruction: create and switch to a feature branch first, then retry the edit.
Suggested command: git switch -c <branch-name>
EOF
	exit 1
fi
