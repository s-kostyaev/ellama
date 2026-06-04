#!/bin/bash
# check-commands.sh - Verify interactive command documentation coverage
#
# Usage: ./scripts/check-commands.sh [project-dir]
# If project-dir is omitted, uses the current directory.
#
# Performs two checks:
#   1. Verify all public commands are documented in README.org
#   2. Verify all documented commands in README.org correspond to real commands
#
# Exit code 0 = all checks pass, 1 = discrepancies found

set -euo pipefail

# Determine project root (resolve to absolute path)
PROJECT_DIR="${1:-$(pwd)}"
PROJECT_DIR="$(cd "$PROJECT_DIR" && pwd)"
README_FILE="$PROJECT_DIR/README.org"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=== Ellama Interactive Command Documentation Checker ==="
echo "Project directory: $PROJECT_DIR"
echo "README file: $README_FILE"
echo ""

# Validate README.org exists
if [ ! -f "$README_FILE" ]; then
    echo "ERROR: README.org not found at $README_FILE"
    exit 1
fi

# Validate Elisp extraction script exists
if [ ! -f "$SCRIPT_DIR/extract-commands.el" ]; then
    echo "ERROR: Elisp extraction script not found at $SCRIPT_DIR/extract-commands.el"
    exit 1
fi

# Create temp files
CMD_FILE=$(mktemp)
DOCUMENTED_FILE=$(mktemp)
TMP_RAW_DOCS=$(mktemp)

trap "rm -f '$CMD_FILE' '$DOCUMENTED_FILE' '$TMP_RAW_DOCS'" EXIT

# --- Step 1: Extract public commands using Emacs batch mode ---
echo "--- Extracting public commands from .el files ---"

# Use safe Emacs-based extraction to avoid false positives from regex.
# Pass directory via environment variable to avoid command-line processing issues
if ELLAMA_DIR="$PROJECT_DIR" emacs -Q -batch -l "$SCRIPT_DIR/extract-commands.el" 2>/dev/null > "$CMD_FILE"; then
    CMD_COUNT=$(wc -l < "$CMD_FILE")
    echo "Found $CMD_COUNT public commands in .el files."
else
    echo "ERROR: Failed to extract public commands."
    exit 1
fi
echo ""

# --- Step 2: Extract documented commands from README.org Commands section ---
echo "--- Extracting documented commands from README.org Commands section ---"

# Use oq to get Commands section range
COMMANDS_SECTION_RANGE=$(oq "$README_FILE" ".section('Commands')" 2>&1 | head -1 || true)

# Extract lines from Commands section
if [ -n "$COMMANDS_SECTION_RANGE" ]; then
    # Parse line range
    START_LINE=$(echo "$COMMANDS_SECTION_RANGE" | sed -n 's/.*lines \([0-9]*\):.*/\1/p')
    END_LINE=$(echo "$COMMANDS_SECTION_RANGE" | sed -n 's/.*lines [0-9]*:\([0-9]*\)).*/\1/p')

    # Extract documented commands
    sed -n "${START_LINE},${END_LINE}p" "$README_FILE" \
        | grep -oE '~ellama-[a-zA-Z0-9_-]+~' \
        | sed 's/~//g' \
        | sort -u > "$TMP_RAW_DOCS" || true
else
    echo "WARNING: Could not locate Commands section with oq, using fallback."
    sed -n '/^\* Commands$/,/^\* /p' "$README_FILE" \
        | grep -oE '~ellama-[a-zA-Z0-9_-]*~' \
        | sed 's/~//g' \
        | sort -u > "$TMP_RAW_DOCS" || true
fi

sort -u "$TMP_RAW_DOCS" > "$DOCUMENTED_FILE"

DOCUMENTED_COUNT=$(wc -l < "$DOCUMENTED_FILE")
echo "Found $DOCUMENTED_COUNT documented commands in README.org."
echo ""

# --- Step 3: Compare and report ---
echo "--- Checking coverage ---"
echo ""

# Check 1: Public commands not documented
UNDOCUMENTED=$(comm -23 "$CMD_FILE" "$DOCUMENTED_FILE")
if [ -n "$UNDOCUMENTED" ]; then
    echo "ERROR: Public commands NOT documented in README.org:"
    echo "$UNDOCUMENTED"
    UNDOCUMENTED_FLAG=1
else
    echo "OK: All public commands are documented."
    UNDOCUMENTED_FLAG=0
fi
echo ""

# Check 2: Documented commands that don't exist
EXTRA=$(comm -13 "$CMD_FILE" "$DOCUMENTED_FILE")
if [ -n "$EXTRA" ]; then
    echo "ERROR: Commands documented in README.org that do not exist:"
    echo "$EXTRA"
    EXTRA_FLAG=1
else
    echo "OK: All documented commands correspond to real functions."
    EXTRA_FLAG=0
fi
echo ""

# Exit with error if discrepancies
if [ "$UNDOCUMENTED_FLAG" -eq 1 ] || [ "$EXTRA_FLAG" -eq 1 ]; then
    echo "FAIL: Documentation coverage issues detected."
    exit 1
fi

echo "PASS: All public commands are properly documented."
exit 0
