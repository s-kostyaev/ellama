#!/bin/sh
set -eu

file=${ELLAMA_FILE_NAME:-}

if [ -z "$file" ]; then
	printf '%s\n' "Missing ELLAMA_FILE_NAME for edit hook."
	exit 1
fi

case "$file" in
	"$PWD"/*) rel=${file#"$PWD"/} ;;
	*) rel=$file ;;
esac

run_check() {
	target=$1
	output=$(mktemp)
	if make "$target" >"$output" 2>&1; then
		rm -f "$output"
		printf '%s\n' "$target passed for $rel."
	else
		status=$?
		printf '%s\n\n' "$target failed for $rel."
		cat "$output"
		rm -f "$output"
		exit "$status"
	fi
}

case "$rel" in
	*.el)
		run_check check-elisp
		;;
	README.org)
		run_check check-readme
		;;
	NEWS.org)
		run_check check-news
		;;
esac
