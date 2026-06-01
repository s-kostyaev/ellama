#!/bin/bash
# check-custom-variables.sh - Verify defcustom documentation coverage
#
# Usage: ./scripts/check-custom-variables.sh [project-dir]
# If project-dir is omitted, uses the current directory.
#
# Performs two checks:
#   1. Verify all defcustom symbols are documented in README.org
#   2. Verify all documented variables in README.org correspond to defcustoms
#
# Exit code 0 = all checks pass, 1 = discrepancies found

set -euo pipefail

# Determine project root
PROJECT_DIR="${1:-$(pwd)}"
README_FILE="$PROJECT_DIR/README.org"

echo "=== Ellama Custom Variable Documentation Checker ==="
echo "Project directory: $PROJECT_DIR"
echo "README file: $README_FILE"
echo ""

# Validate README.org exists
if [ ! -f "$README_FILE" ]; then
    echo "ERROR: README.org not found at $README_FILE"
    exit 1
fi

# Create temp files for symbol lists
DEF_CUSTOM_FILE=$(mktemp)
DOCUMENTED_FILE=$(mktemp)
TMP_RAW_DOCS=$(mktemp)

trap "rm -f '$DEF_CUSTOM_FILE' '$DOCUMENTED_FILE' '$TMP_RAW_DOCS'" EXIT

# --- Step 1: Extract defcustom symbols from .el files ---
echo "--- Extracting defcustom symbols from .el files ---"
# Find all .el files (not .elc, .eln, etc.) and extract defcustom symbols
# Use grep -oh to extract just the matching part: (defcustom SYMBOL_NAME
# Then strip the (defcustom prefix
find "$PROJECT_DIR" -name "*.el" -type f \
     ! -name "*.elc" \
     ! -name "*.eln" \
     -exec grep -oh '(defcustom [a-zA-Z_-]*' {} + 2>/dev/null \
    | sed 's/(defcustom //' \
    | grep -v '\-template$' \
    | sort -u > "$DEF_CUSTOM_FILE"

DEF_COUNT=$(wc -l < "$DEF_CUSTOM_FILE")
echo "Found $DEF_COUNT defcustom symbols in .el files."
echo ""

# --- Step 2: Extract documented variables from README.org Configuration section ---
echo "--- Extracting documented variables from README.org Configuration section ---"

# Use oq to get Configuration section range
CONFIG_SECTION_RANGE=$(oq "$README_FILE" ".section('Configuration')" 2>&1 | head -1 || true)

# Extract lines from Configuration section (format: "* Configuration (lines XXX:YYY)")
if [ -n "$CONFIG_SECTION_RANGE" ]; then
    # Parse the line range from oq output using sed (macOS compatible)
    # Format: "* Configuration (lines 310:1300)"
    START_LINE=$(echo "$CONFIG_SECTION_RANGE" | sed -n 's/.*lines \([0-9]*\):.*/\1/p')
    END_LINE=$(echo "$CONFIG_SECTION_RANGE" | sed -n 's/.*lines [0-9]*:\([0-9]*\)).*/\1/p')
    
    # Extract all ~ellama-variablename~ patterns from the Configuration section
    # Filter out non-ellama variables mentioned in descriptions (like ~ollama~, ~vertex~, etc.)
    # Only keep lines that are actual list items (start with "- ~")
    sed -n "${START_LINE},${END_LINE}p" "$README_FILE" \
        | grep -oE '~ellama-[a-zA-Z0-9_-]+~' \
        | sed 's/~//g' \
        | grep -v '\-template$' \
        | sort -u > "$TMP_RAW_DOCS"
else
    # Fallback: use the old method if oq fails
    CONFIG_SECTION=$(sed -n '/^* Configuration$/,/^* /p' "$README_FILE")
    echo "$CONFIG_SECTION" \
        | grep -oE '~ellama-[a-zA-Z0-9_-]+~' \
        | sed 's/~//g' \
        | grep -v '\-template$' \
        | sort -u > "$TMP_RAW_DOCS"
fi

# Filter: only keep variables that are actual defcustoms
# This eliminates false positives like ellama-ask-about, ellama-chat, etc.
while IFS= read -r var; do
    if grep -qx "$var" "$DEF_CUSTOM_FILE"; then
        echo "$var" >> "$DOCUMENTED_FILE"
    fi
done < "$TMP_RAW_DOCS"
sort -u -o "$DOCUMENTED_FILE" "$DOCUMENTED_FILE"

DOC_COUNT=$(wc -l < "$DOCUMENTED_FILE")
echo "Found $DOC_COUNT documented defcustom variables in README.org Configuration section."
echo ""

# --- Step 3: Check 1 - Are all defcustoms documented? ---
echo "--- Check 1: Are all defcustoms documented in README.org? ---"

# Find defcustoms that are NOT in the documented list
UNDOCUMENTED=$(comm -23 "$DEF_CUSTOM_FILE" "$DOCUMENTED_FILE")

if [ -z "$UNDOCUMENTED" ]; then
    echo "PASS: All defcustom symbols are documented."
else
    UNDOCUMENTED_COUNT=$(echo "$UNDOCUMENTED" | wc -l)
    echo "FAIL: $UNDOCUMENTED_COUNT defcustom symbol(s) are NOT documented in README.org:"
    echo "$UNDOCUMENTED" | sed 's/^/  - /'
fi
echo ""

# --- Step 4: Check 2 - Are all documented variables actual defcustoms? ---
echo "--- Check 2: Are all documented variables actual defcustoms? ---"

# Find documented variables that are NOT actual defcustoms
NO_DEF_CUSTOM=$(comm -23 "$DOCUMENTED_FILE" "$DEF_CUSTOM_FILE")

if [ -z "$NO_DEF_CUSTOM" ]; then
    echo "PASS: All documented variables correspond to defcustom symbols."
else
    NO_DEF_CUSTOM_COUNT=$(echo "$NO_DEF_CUSTOM" | wc -l)
    echo "FAIL: $NO_DEF_CUSTOM_COUNT documented variable(s) do NOT correspond to defcustoms:"
    echo "$NO_DEF_CUSTOM" | sed 's/^/  - /'
fi
echo ""

# --- Summary ---
echo "=== Summary ==="

if [ -z "$UNDOCUMENTED" ]; then
    UNDOCUMENTED_COUNT=0
else
    UNDOCUMENTED_COUNT=$(echo "$UNDOCUMENTED" | wc -l)
fi

if [ -z "$NO_DEF_CUSTOM" ]; then
    NO_DEF_CUSTOM_COUNT=0
else
    NO_DEF_CUSTOM_COUNT=$(echo "$NO_DEF_CUSTOM" | wc -l)
fi

if [ "$UNDOCUMENTED_COUNT" -eq 0 ] && [ "$NO_DEF_CUSTOM_COUNT" -eq 0 ]; then
    echo "All checks passed! ✓"
    echo ""
    echo "Statistics:"
    echo "  Total defcustom symbols: $DEF_COUNT"
    echo "  Total documented variables: $DOC_COUNT"
    exit 0
else
    echo "Issues found:"
    if [ "$UNDOCUMENTED_COUNT" -gt 0 ]; then
        echo "  - $UNDOCUMENTED_COUNT undocumented defcustom(s)"
    fi
    if [ "$NO_DEF_CUSTOM_COUNT" -gt 0 ]; then
        echo "  - $NO_DEF_CUSTOM_COUNT documented variable(s) without corresponding defcustom"
    fi
    exit 1
fi
