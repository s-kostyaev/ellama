#!/bin/sh
set -eu

file=${ELLAMA_FILE_NAME:-}

if [ -z "$file" ]; then
	printf '%s\n' "Missing ELLAMA_FILE_NAME for edit hook."
	exit 1
fi

sh .codex/hooks/check-files.sh "$file"
