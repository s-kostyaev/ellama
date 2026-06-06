#!/bin/bash
# check-transient-dup-keys.sh - Check transient menus for duplicate keys
#
# Usage: ./scripts/check-transient-dup-keys.sh [project-dir]
# If project-dir is omitted, uses the current directory.
#
# Exit code 0 = no duplicates found, 1 = duplicates found or error

set -euo pipefail

# Determine project root (resolve to absolute path)
PROJECT_DIR="${1:-$(pwd)}"
PROJECT_DIR="$(cd "$PROJECT_DIR" && pwd)"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRANSIENT_FILE="$PROJECT_DIR/ellama-transient.el"

echo "=== Ellama Transient Key Duplication Checker ==="
echo "Project directory: $PROJECT_DIR"
echo "Transient file: $TRANSIENT_FILE"
echo ""

# Validate ellama-transient.el exists
if [ ! -f "$TRANSIENT_FILE" ]; then
    echo "ERROR: ellama-transient.el not found at $TRANSIENT_FILE"
    exit 1
fi

# Validate Elisp script exists
if [ ! -f "$SCRIPT_DIR/check-transient-dup-keys.el" ]; then
    echo "ERROR: Elisp script not found at $SCRIPT_DIR/check-transient-dup-keys.el"
    exit 1
fi

# Create temp file for output
OUTPUT_FILE=$(mktemp)

trap "rm -f '$OUTPUT_FILE'" EXIT

echo "--- Checking for duplicate keys in transient menus ---"
echo ""

# Run Emacs in batch mode to check for duplicate keys
if ELLAMA_DIR="$PROJECT_DIR" emacs -Q -batch \
	     -l "$SCRIPT_DIR/check-transient-dup-keys.el" \
	     > "$OUTPUT_FILE" 2>&1; then
    cat "$OUTPUT_FILE"
else
    echo "ERROR: Emacs execution failed."
    exit 1
fi

# Check if any output means duplicates were found
if [ -s "$OUTPUT_FILE" ] && grep -q "===" "$OUTPUT_FILE"; then
    echo ""
    echo "FAIL: Duplicate keys found in transient menus."
    exit 1
fi

echo "PASS: No duplicate keys found in transient menus."
exit 0
