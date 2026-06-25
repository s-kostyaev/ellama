#\!/bin/bash
# check-copyright.sh - Verify FSF copyright headers in Elisp files
#
# Usage: ./scripts/check-copyright.sh [project-dir]
# If project-dir is omitted, uses the current directory.
#
# Exit code 0 = all files have FSF copyright, 1 = some files missing it

set -euo pipefail

# Determine project root (resolve to absolute path)
PROJECT_DIR="${1:-$(pwd)}"
PROJECT_DIR="$(cd "$PROJECT_DIR" && pwd)"

echo "=== Ellama FSF Copyright Checker ==="
echo "Project directory: $PROJECT_DIR"
echo ""

# Find all .el files in the project (excluding compiled and vendor files)
# Using -not instead of \! to avoid shell history expansion issues
EL_FILES=$(find "$PROJECT_DIR" -name "*.el" -type f \
     -not -name "*.elc" \
     -not -name "*.eln" \
     -not -name ".dir-locals.el" \
     -not -path "*/deps/*" \
     -not -path "*/30_*" \
     -not -path "*/elpa/*" \
     -not -path "*/tests/fixtures/*/workspace/*" \
     | sort)

echo "--- Checking FSF copyright headers ---"
echo ""

MISSING_FILES=()

for file in $EL_FILES; do
    # Check if file contains Free Software Foundation copyright
    HAS_COPYRIGHT=0
    if head -20 "$file" | grep -q "Free Software Foundation"; then
        HAS_COPYRIGHT=1
    fi
    if [ "$HAS_COPYRIGHT" -eq 0 ]; then
        MISSING_FILES+=("$file")
    fi
done

FILE_COUNT=$(echo "$EL_FILES" | wc -l)
MISSING_COUNT=${#MISSING_FILES[@]}

echo "Scanned $FILE_COUNT Elisp files."
echo ""

if [ "$MISSING_COUNT" -eq 0 ]; then
    echo "PASS: All files have FSF copyright headers."
    exit 0
else
    echo "FAIL: $MISSING_COUNT file(s) missing FSF copyright headers:"
    echo ""
    for file in "${MISSING_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
    exit 1
fi
