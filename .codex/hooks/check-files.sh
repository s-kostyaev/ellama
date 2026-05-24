#!/bin/sh
set -eu

check_elisp=false
check_readme=false
check_news=false
elisp_files=

append_file() {
	files=$1
	file=$2
	if [ -n "$files" ]; then
		printf '%s %s' "$files" "$file"
	else
		printf '%s' "$file"
	fi
}

for file in "$@"
do
	case "$file" in
		"$PWD"/*) rel=${file#"$PWD"/} ;;
		*) rel=$file ;;
	esac

	case "$rel" in
		*.el)
			check_elisp=true
			elisp_files=$(append_file "$elisp_files" "$rel")
			;;
		README.org)
			check_readme=true
			;;
		NEWS.org)
			check_news=true
			;;
	esac
done

ensure_clean() {
	target=$1
	shift

	if [ "${ELLAMA_BLOCK_ON_CHECK_CHANGES:-}" != 1 ]; then
		return
	fi

	if ! git diff --quiet -- "$@"; then
		cat >&2 <<EOF
Push blocked: make $target changed files.

Review, stage, and commit those changes before pushing.
EOF
		exit 1
	fi
}

run_check() {
	target=$1
	label=$2
	shift 2
	output=$(mktemp)
	printf '%s\n' "Running make $target for $label..."
	if make "$target" >"$output" 2>&1; then
		rm -f "$output"
		printf '%s\n' "$target passed for $label."
	else
		status=$?
		printf '%s\n\n' "$target failed for $label."
		cat "$output"
		rm -f "$output"
		exit "$status"
	fi
	ensure_clean "$target" "$@"
}

if [ "$check_elisp" = true ]; then
	run_check check-elisp "$elisp_files" '*.el' 'tests/*.el'
fi

if [ "$check_readme" = true ]; then
	run_check check-readme README.org README.org ellama.texi ellama.info
fi

if [ "$check_news" = true ]; then
	run_check check-news NEWS.org NEWS.org
fi
