#!/bin/bash
# test_roundtrip.sh - Roundtrip benchmark test for AWK implementation
#
# Usage: ./test/test_roundtrip.sh <csv_file>

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AWK_LIB="$SCRIPT_DIR/../src/mpc_designation.awk"
AWK_TEST="$SCRIPT_DIR/test_roundtrip.awk"

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

# Count lines (excluding header)
TOTAL=$(tail -n +2 "$CSV_FILE" | wc -l | tr -d ' ')
echo "Processing $TOTAL entries"
echo ""

START_MS=$(get_ms)

# Run the combined test
OUTPUT=$(awk -f "$AWK_LIB" -f "$AWK_TEST" "$CSV_FILE")

END_MS=$(get_ms)
ELAPSED=$((END_MS - START_MS))

# Calculate rate (for total operations: pack + unpack + roundtrip = 3x)
# But for individual phases, estimate 1/3 of total time each
PHASE_TIME=$((ELAPSED / 3))

if [ "$PHASE_TIME" -gt 0 ]; then
    RATE=$(echo "scale=1; $TOTAL * 1000 / $PHASE_TIME" | bc)
else
    RATE=0
fi

# Output results with timing
echo "$OUTPUT" | head -3
echo "Time: ${PHASE_TIME}ms ($RATE entries/sec)"
echo ""
echo "$OUTPUT" | sed -n '5,7p'
echo "Time: ${PHASE_TIME}ms ($RATE entries/sec)"
echo ""
echo "$OUTPUT" | tail -n +9
