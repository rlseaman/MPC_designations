#!/bin/bash
#
# benchmark.sh - Compare performance across language implementations
#
# Usage: ./scripts/benchmark.sh [iterations]
#
# Runs conversion tests for each language and displays a comparison table.
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$ROOT_DIR"

ITERATIONS=${1:-1}
CSV_FILE="test-data/prov_unpack_to_pack.csv"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "=== MPC Designation Benchmark ==="
echo ""

# Ensure test data exists
if [ ! -f "$CSV_FILE" ]; then
    echo "Decompressing test data..."
    gunzip -k "${CSV_FILE}.gz"
fi

# Build compiled languages (including test executables)
echo "Building compiled implementations..."
make -C c test_csv >/dev/null 2>&1
make -C swift test_csv >/dev/null 2>&1
make -C go test_csv >/dev/null 2>&1
echo ""

# Arrays to store results
declare -a LANGUAGES
declare -a TIMES_MS
declare -a RATES

# Function to extract timing from output
extract_timing() {
    local output="$1"
    # Extract: Time:   XXXms (YYY.Z entries/sec)
    local time_ms=$(echo "$output" | grep "Time:" | sed -E 's/.*Time:[[:space:]]*([0-9]+)ms.*/\1/')
    local rate=$(echo "$output" | grep "Time:" | sed -E 's/.*\(([0-9.]+) entries\/sec\).*/\1/')
    echo "$time_ms $rate"
}

# Function to run benchmark for a language
run_benchmark() {
    local lang="$1"
    local cmd="$2"

    printf "Testing %-8s " "$lang..."

    local total_ms=0
    local total_rate=0
    local output

    for ((i=1; i<=ITERATIONS; i++)); do
        output=$(eval "$cmd" 2>&1)

        # Check for failures
        if echo "$output" | grep -q "Failed: [1-9]"; then
            echo -e "${RED}FAILED${NC}"
            return 1
        fi

        read time_ms rate <<< $(extract_timing "$output")
        total_ms=$((total_ms + time_ms))
        total_rate=$(echo "$total_rate + $rate" | bc)
    done

    local avg_ms=$((total_ms / ITERATIONS))
    local avg_rate=$(echo "scale=1; $total_rate / $ITERATIONS" | bc)

    LANGUAGES+=("$lang")
    TIMES_MS+=("$avg_ms")
    RATES+=("$avg_rate")

    printf "%6dms  %12.1f entries/sec\n" "$avg_ms" "$avg_rate"
}

echo "Running benchmarks (${ITERATIONS} iteration(s) each)..."
echo ""

# Run each language
run_benchmark "C" "cd c && ./test_csv ../test-data/prov_unpack_to_pack.csv"
run_benchmark "Go" "cd go && ./test_csv ../test-data/prov_unpack_to_pack.csv"
run_benchmark "Swift" "cd swift && ./test_csv ../test-data/prov_unpack_to_pack.csv"
run_benchmark "Perl" "cd perl && perl test/test_csv.pl ../test-data/prov_unpack_to_pack.csv"
run_benchmark "Python" "cd python && python3 test/test_csv.py ../test-data/prov_unpack_to_pack.csv"
run_benchmark "Tcl" "cd tcl && tclsh test/test_csv.tcl ../test-data/prov_unpack_to_pack.csv"

echo ""
echo "=== Summary ==="
echo ""

# Sort by rate (descending) and display
# Create indexed array for sorting
declare -a INDEXED
for i in "${!LANGUAGES[@]}"; do
    INDEXED+=("${RATES[$i]}:${LANGUAGES[$i]}:${TIMES_MS[$i]}")
done

# Sort numerically by rate (descending)
IFS=$'\n' SORTED=($(sort -t: -k1 -rn <<<"${INDEXED[*]}"))
unset IFS

printf "%-10s %10s %18s %11s\n" "Language" "Time (ms)" "Rate (entries/sec)" "Relative"
printf "%-10s %10s %18s %11s\n" "--------" "---------" "------------------" "--------"

# Get fastest rate for relative comparison
FASTEST_RATE=$(echo "${SORTED[0]}" | cut -d: -f1)

for item in "${SORTED[@]}"; do
    rate=$(echo "$item" | cut -d: -f1)
    lang=$(echo "$item" | cut -d: -f2)
    time_ms=$(echo "$item" | cut -d: -f3)
    relative=$(echo "scale=4; $rate / $FASTEST_RATE" | bc)
    printf "%-10s %10d %18.1f %10.4fx\n" "$lang" "$time_ms" "$rate" "$relative"
done

echo ""
echo "Test data: 2,021,090 designation conversions"
