#!/bin/bash
# test_csv.sh - CSV benchmark test for Bash implementation
#
# Usage: ./test/test_csv.sh <csv_file>

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../src/mpc_designation.sh"

if [ $# -ne 1 ]; then
    echo "Usage: $0 <csv_file>"
    exit 1
fi

CSV_FILE="$1"

if [ ! -f "$CSV_FILE" ]; then
    echo "Error: File not found: $CSV_FILE"
    exit 1
fi

PASSED=0
FAILED=0

# Get start time in milliseconds
START_MS=$(($(date +%s) * 1000 + $(date +%N 2>/dev/null | cut -c1-3 || echo 0)))

# Use a more portable approach for milliseconds on macOS
if command -v gdate >/dev/null 2>&1; then
    START_MS=$(gdate +%s%3N)
elif command -v python3 >/dev/null 2>&1; then
    START_MS=$(python3 -c 'import time; print(int(time.time() * 1000))')
else
    START_MS=$(($(date +%s) * 1000))
fi

# Read CSV and test each line (skip header)
FIRST=true
while IFS=, read -r unpacked packed || [ -n "$unpacked" ]; do
    if [ "$FIRST" = true ]; then
        FIRST=false
        continue
    fi

    # Pack test: unpacked -> packed
    result=$(convert_simple "$unpacked")
    if [ "$result" = "$packed" ]; then
        ((PASSED++))
    else
        ((FAILED++))
    fi
done < "$CSV_FILE"

# Get end time
if command -v gdate >/dev/null 2>&1; then
    END_MS=$(gdate +%s%3N)
elif command -v python3 >/dev/null 2>&1; then
    END_MS=$(python3 -c 'import time; print(int(time.time() * 1000))')
else
    END_MS=$(($(date +%s) * 1000))
fi

ELAPSED=$((END_MS - START_MS))
TOTAL=$((PASSED + FAILED))

if [ "$ELAPSED" -gt 0 ]; then
    RATE=$(echo "scale=1; $TOTAL * 1000 / $ELAPSED" | bc)
else
    RATE=0
fi

echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Time: ${ELAPSED}ms ($RATE entries/sec)"
