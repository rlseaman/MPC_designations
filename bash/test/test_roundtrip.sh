#!/bin/bash
# test_roundtrip.sh - Roundtrip benchmark test for Bash implementation
#
# Usage: ./test/test_roundtrip.sh <csv_file>

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

# Function to get current time in milliseconds
get_ms() {
    if command -v gdate >/dev/null 2>&1; then
        gdate +%s%3N
    elif command -v python3 >/dev/null 2>&1; then
        python3 -c 'import time; print(int(time.time() * 1000))'
    else
        echo $(($(date +%s) * 1000))
    fi
}

# Read all data into arrays first
declare -a UNPACKED_DATA
declare -a PACKED_DATA

echo "Loading data..."
FIRST=true
while IFS=, read -r unpacked packed || [ -n "$unpacked" ]; do
    if [ "$FIRST" = true ]; then
        FIRST=false
        continue
    fi
    UNPACKED_DATA+=("$unpacked")
    PACKED_DATA+=("$packed")
done < "$CSV_FILE"

TOTAL=${#UNPACKED_DATA[@]}
echo "Loaded $TOTAL entries"
echo ""

# ========== Phase 1: Pack (unpacked -> packed) ==========
echo "=== Phase 1: Pack ==="
PACK_PASSED=0
PACK_FAILED=0

START_MS=$(get_ms)

for i in "${!UNPACKED_DATA[@]}"; do
    result=$(convert_simple "${UNPACKED_DATA[$i]}")
    if [ "$result" = "${PACKED_DATA[$i]}" ]; then
        ((PACK_PASSED++))
    else
        ((PACK_FAILED++))
    fi
done

END_MS=$(get_ms)
PACK_ELAPSED=$((END_MS - START_MS))

if [ "$PACK_ELAPSED" -gt 0 ]; then
    PACK_RATE=$(echo "scale=1; $TOTAL * 1000 / $PACK_ELAPSED" | bc)
else
    PACK_RATE=0
fi

echo "Passed: $PACK_PASSED"
echo "Failed: $PACK_FAILED"
echo "Time: ${PACK_ELAPSED}ms ($PACK_RATE entries/sec)"
echo ""

# ========== Phase 2: Unpack (packed -> unpacked) ==========
echo "=== Phase 2: Unpack ==="
UNPACK_PASSED=0
UNPACK_FAILED=0

START_MS=$(get_ms)

for i in "${!PACKED_DATA[@]}"; do
    result=$(convert_simple "${PACKED_DATA[$i]}")
    if [ "$result" = "${UNPACKED_DATA[$i]}" ]; then
        ((UNPACK_PASSED++))
    else
        ((UNPACK_FAILED++))
    fi
done

END_MS=$(get_ms)
UNPACK_ELAPSED=$((END_MS - START_MS))

if [ "$UNPACK_ELAPSED" -gt 0 ]; then
    UNPACK_RATE=$(echo "scale=1; $TOTAL * 1000 / $UNPACK_ELAPSED" | bc)
else
    UNPACK_RATE=0
fi

echo "Passed: $UNPACK_PASSED"
echo "Failed: $UNPACK_FAILED"
echo "Time: ${UNPACK_ELAPSED}ms ($UNPACK_RATE entries/sec)"
echo ""

# ========== Round-trip verification: pack(unpack(y)) = y ==========
echo "=== Round-trip Verification ==="
RT_PACKED_PASS=0
RT_PACKED_FAIL=0

for i in "${!PACKED_DATA[@]}"; do
    packed="${PACKED_DATA[$i]}"
    unpacked=$(convert_simple "$packed")
    repacked=$(convert_simple "$unpacked")
    if [ "$repacked" = "$packed" ]; then
        ((RT_PACKED_PASS++))
    else
        ((RT_PACKED_FAIL++))
    fi
done

if [ "$RT_PACKED_FAIL" -eq 0 ]; then
    echo "Packed RT: PASS ($RT_PACKED_PASS/$TOTAL)"
else
    echo "Packed RT: FAIL ($RT_PACKED_FAIL failures)"
fi
