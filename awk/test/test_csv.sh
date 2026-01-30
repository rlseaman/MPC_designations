#!/bin/bash
# test_csv.sh - CSV benchmark test wrapper for AWK implementation
#
# Usage: ./test/test_csv.sh <csv_file>

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AWK_LIB="$SCRIPT_DIR/../src/mpc_designation.awk"
AWK_TEST="$SCRIPT_DIR/test_csv.awk"

if [ $# -ne 1 ]; then
    echo "Usage: $0 <csv_file>"
    exit 1
fi

CSV_FILE="$1"

if [ ! -f "$CSV_FILE" ]; then
    echo "Error: File not found: $CSV_FILE"
    exit 1
fi

# Get timing
get_ms() {
    if command -v gdate >/dev/null 2>&1; then
        gdate +%s%3N
    elif command -v python3 >/dev/null 2>&1; then
        python3 -c 'import time; print(int(time.time() * 1000))'
    else
        echo $(($(date +%s) * 1000))
    fi
}

START_MS=$(get_ms)

# Run the test
OUTPUT=$(awk -f "$AWK_LIB" -f "$AWK_TEST" "$CSV_FILE")

END_MS=$(get_ms)
ELAPSED=$((END_MS - START_MS))

# Extract counts
PASSED=$(echo "$OUTPUT" | grep "Passed:" | awk '{print $2}')
FAILED=$(echo "$OUTPUT" | grep "Failed:" | awk '{print $2}')
TOTAL=$((PASSED + FAILED))

if [ "$ELAPSED" -gt 0 ]; then
    RATE=$(echo "scale=1; $TOTAL * 1000 / $ELAPSED" | bc)
else
    RATE=0
fi

echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Time: ${ELAPSED}ms ($RATE entries/sec)"
