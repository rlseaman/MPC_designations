#!/bin/bash
#
# benchmark.sh - Compare performance across language implementations
#
# Usage: ./scripts/benchmark.sh [--roundtrip]
#
# By default, runs pack-only timing (fastest).
# With --roundtrip, runs bidirectional tests showing pack, unpack, and round-trip timing.
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$ROOT_DIR"

ROUNDTRIP=false
if [ "$1" = "--roundtrip" ]; then
    ROUNDTRIP=true
fi

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

# Build compiled languages
echo "Building compiled implementations..."
make -C c test_csv test_roundtrip >/dev/null 2>&1 || true
make -C cpp test_csv test_roundtrip >/dev/null 2>&1 || true
make -C swift test_csv test_roundtrip >/dev/null 2>&1 || true
make -C go test_csv test_roundtrip >/dev/null 2>&1 || true
make -C fortran build/test_csv build/test_roundtrip >/dev/null 2>&1 || true
if command -v cargo >/dev/null 2>&1; then
    (cd rust && cargo build --release -q 2>/dev/null) || true
fi
if command -v javac >/dev/null 2>&1; then
    make -C java build >/dev/null 2>&1 || true
fi
if command -v kotlinc >/dev/null 2>&1; then
    make -C kotlin build >/dev/null 2>&1 || true
fi
if command -v dotnet >/dev/null 2>&1; then
    (cd csharp && dotnet build -c Release -v q 2>/dev/null) || true
fi
if command -v npm >/dev/null 2>&1; then
    (cd typescript && npm run build 2>/dev/null) || true
fi
echo ""

if [ "$ROUNDTRIP" = true ]; then
    # ========== Roundtrip mode: Show bidirectional timing ==========

    # Arrays for each phase
    declare -a LANGUAGES
    declare -a PACK_TIMES
    declare -a PACK_RATES
    declare -a UNPACK_TIMES
    declare -a UNPACK_RATES
    declare -a RT_PACKED_STATUS

    extract_phase_timing() {
        local output="$1"
        local phase="$2"

        # Extract timing from specific phase section
        local section=$(echo "$output" | grep -A3 "=== $phase")
        local time_ms=$(echo "$section" | grep "Time:" | sed -E 's/.*Time:[[:space:]]*([0-9]+)ms.*/\1/')
        local rate=$(echo "$section" | grep "Time:" | sed -E 's/.*\([[:space:]]*([0-9.]+) entries\/sec\).*/\1/')
        echo "$time_ms $rate"
    }

    run_roundtrip_benchmark() {
        local lang="$1"
        local cmd="$2"

        printf "Testing %-12s " "$lang..."

        local output
        output=$(eval "$cmd" 2>&1 || true)

        # Extract pack timing
        read pack_time pack_rate <<< $(extract_phase_timing "$output" "Phase 1: Pack")

        # Extract unpack timing
        read unpack_time unpack_rate <<< $(extract_phase_timing "$output" "Phase 2: Unpack")

        # Check packed RT status
        if echo "$output" | grep -q "Packed RT:.*PASS"; then
            rt_status="PASS"
        else
            rt_status="FAIL"
        fi

        if [ -n "$pack_time" ] && [ -n "$pack_rate" ]; then
            LANGUAGES+=("$lang")
            PACK_TIMES+=("$pack_time")
            PACK_RATES+=("$pack_rate")
            UNPACK_TIMES+=("$unpack_time")
            UNPACK_RATES+=("$unpack_rate")
            RT_PACKED_STATUS+=("$rt_status")

            printf "Pack: %5dms (%8.0f/s)  Unpack: %5dms (%8.0f/s)  RT: %s\n" \
                "$pack_time" "$pack_rate" "$unpack_time" "$unpack_rate" "$rt_status"
        else
            printf "SKIPPED (not available)\n"
        fi
    }

    echo "Running bidirectional benchmarks..."
    echo ""

    run_roundtrip_benchmark "C" "cd c && ./test_roundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    run_roundtrip_benchmark "C++" "cd cpp && ./test_roundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    run_roundtrip_benchmark "Go" "cd go && ./test_roundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    run_roundtrip_benchmark "Swift" "cd swift && ./test_roundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    run_roundtrip_benchmark "Fortran" "cd fortran && ./build/test_roundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    if command -v cargo >/dev/null 2>&1; then
        run_roundtrip_benchmark "Rust" "cd rust && ./target/release/test_roundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v java >/dev/null 2>&1 && [ -f java/classes/mpc/TestRoundtrip.class ]; then
        run_roundtrip_benchmark "Java" "cd java && java -cp classes mpc.TestRoundtrip ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v kotlin >/dev/null 2>&1 && [ -f kotlin/build/mpc_designation.jar ]; then
        run_roundtrip_benchmark "Kotlin" "cd kotlin && kotlin -cp build/mpc_designation.jar mpc.TestRoundtripKt ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v dotnet >/dev/null 2>&1; then
        run_roundtrip_benchmark "C#" "cd csharp/test && dotnet run -c Release --project TestRoundtrip.csproj -- ../../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v node >/dev/null 2>&1; then
        run_roundtrip_benchmark "JavaScript" "cd js && node test/test_roundtrip.js ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v node >/dev/null 2>&1 && [ -f typescript/dist/test/test_roundtrip.js ]; then
        run_roundtrip_benchmark "TypeScript" "cd typescript && node dist/test/test_roundtrip.js ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v julia >/dev/null 2>&1; then
        run_roundtrip_benchmark "Julia" "cd julia && julia test/test_roundtrip.jl ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    run_roundtrip_benchmark "Python" "cd python && python3 test/test_roundtrip.py ../test-data/prov_unpack_to_pack.csv 2>&1"
    if command -v php >/dev/null 2>&1; then
        run_roundtrip_benchmark "PHP" "cd php && php test/test_roundtrip.php ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    if command -v ruby >/dev/null 2>&1; then
        run_roundtrip_benchmark "Ruby" "cd ruby && ruby test/test_roundtrip.rb ../test-data/prov_unpack_to_pack.csv 2>&1"
    fi
    run_roundtrip_benchmark "Perl" "cd perl && perl test/test_roundtrip.pl ../test-data/prov_unpack_to_pack.csv 2>&1"
    run_roundtrip_benchmark "Tcl" "cd tcl && tclsh test/test_roundtrip.tcl ../test-data/prov_unpack_to_pack.csv 2>&1"

    echo ""
    echo "=== Summary: Pack Direction (unpacked → packed) ==="
    echo ""

    # Sort by pack rate and display
    declare -a INDEXED
    for i in "${!LANGUAGES[@]}"; do
        INDEXED+=("${PACK_RATES[$i]}:${LANGUAGES[$i]}:${PACK_TIMES[$i]}")
    done
    IFS=$'\n' SORTED=($(sort -t: -k1 -rn <<<"${INDEXED[*]}"))
    unset IFS

    printf "%-12s %10s %18s %11s\n" "Language" "Time (ms)" "Rate (entries/sec)" "Relative"
    printf "%-12s %10s %18s %11s\n" "----------" "---------" "------------------" "--------"

    FASTEST_RATE=$(echo "${SORTED[0]}" | cut -d: -f1)
    for item in "${SORTED[@]}"; do
        rate=$(echo "$item" | cut -d: -f1)
        lang=$(echo "$item" | cut -d: -f2)
        time_ms=$(echo "$item" | cut -d: -f3)
        relative=$(echo "scale=2; $rate / $FASTEST_RATE" | bc)
        printf "%-12s %10d %18.0f %10.2fx\n" "$lang" "$time_ms" "$rate" "$relative"
    done

    echo ""
    echo "=== Summary: Unpack Direction (packed → unpacked) ==="
    echo ""

    # Sort by unpack rate and display
    declare -a INDEXED_U
    for i in "${!LANGUAGES[@]}"; do
        INDEXED_U+=("${UNPACK_RATES[$i]}:${LANGUAGES[$i]}:${UNPACK_TIMES[$i]}")
    done
    IFS=$'\n' SORTED_U=($(sort -t: -k1 -rn <<<"${INDEXED_U[*]}"))
    unset IFS

    printf "%-12s %10s %18s %11s\n" "Language" "Time (ms)" "Rate (entries/sec)" "Relative"
    printf "%-12s %10s %18s %11s\n" "----------" "---------" "------------------" "--------"

    FASTEST_RATE_U=$(echo "${SORTED_U[0]}" | cut -d: -f1)
    for item in "${SORTED_U[@]}"; do
        rate=$(echo "$item" | cut -d: -f1)
        lang=$(echo "$item" | cut -d: -f2)
        time_ms=$(echo "$item" | cut -d: -f3)
        relative=$(echo "scale=2; $rate / $FASTEST_RATE_U" | bc)
        printf "%-12s %10d %18.0f %10.2fx\n" "$lang" "$time_ms" "$rate" "$relative"
    done

    echo ""
    echo "=== Round-trip Verification: pack(unpack(y)) = y ==="
    echo ""
    for i in "${!LANGUAGES[@]}"; do
        printf "%-12s %s\n" "${LANGUAGES[$i]}" "${RT_PACKED_STATUS[$i]}"
    done

else
    # ========== Simple mode: Pack-only timing ==========

    declare -a LANGUAGES
    declare -a TIMES_MS
    declare -a RATES

    extract_timing() {
        local output="$1"
        local time_ms=$(echo "$output" | grep "Time:" | head -1 | sed -E 's/.*Time:[[:space:]]*([0-9]+)ms.*/\1/')
        local rate=$(echo "$output" | grep "Time:" | head -1 | sed -E 's/.*\([[:space:]]*([0-9.]+) entries\/sec\).*/\1/')
        echo "$time_ms $rate"
    }

    run_benchmark() {
        local lang="$1"
        local cmd="$2"

        printf "Testing %-12s " "$lang..."

        local output
        output=$(eval "$cmd" 2>&1 || true)

        if echo "$output" | grep -q "Failed: [1-9]"; then
            echo -e "${RED}FAILED${NC}"
            return 1
        fi

        read time_ms rate <<< $(extract_timing "$output")

        if [ -n "$time_ms" ] && [ -n "$rate" ]; then
            LANGUAGES+=("$lang")
            TIMES_MS+=("$time_ms")
            RATES+=("$rate")
            printf "%6dms  %12.0f entries/sec\n" "$time_ms" "$rate"
        else
            printf "SKIPPED (not available)\n"
        fi
    }

    echo "Running pack benchmarks..."
    echo ""

    run_benchmark "C" "cd c && ./test_csv ../test-data/prov_unpack_to_pack.csv"
    run_benchmark "C++" "cd cpp && ./test_csv ../test-data/prov_unpack_to_pack.csv"
    run_benchmark "Go" "cd go && ./test_csv ../test-data/prov_unpack_to_pack.csv"
    run_benchmark "Swift" "cd swift && ./test_csv ../test-data/prov_unpack_to_pack.csv"
    run_benchmark "Fortran" "cd fortran && ./build/test_csv ../test-data/prov_unpack_to_pack.csv"
    if command -v cargo >/dev/null 2>&1; then
        run_benchmark "Rust" "cd rust && ./target/release/test_csv ../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v java >/dev/null 2>&1 && [ -f java/classes/mpc/TestCSV.class ]; then
        run_benchmark "Java" "cd java && java -cp classes mpc.TestCSV ../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v kotlin >/dev/null 2>&1 && [ -f kotlin/build/mpc_designation.jar ]; then
        run_benchmark "Kotlin" "cd kotlin && kotlin -cp build/mpc_designation.jar mpc.TestCsvKt ../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v dotnet >/dev/null 2>&1; then
        run_benchmark "C#" "cd csharp/test && dotnet run -c Release --project TestCsv.csproj -- ../../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v node >/dev/null 2>&1; then
        run_benchmark "JavaScript" "cd js && node test/test_csv.js ../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v node >/dev/null 2>&1 && [ -f typescript/dist/test/test_csv.js ]; then
        run_benchmark "TypeScript" "cd typescript && node dist/test/test_csv.js ../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v julia >/dev/null 2>&1; then
        run_benchmark "Julia" "cd julia && julia test/test_csv.jl ../test-data/prov_unpack_to_pack.csv"
    fi
    run_benchmark "Python" "cd python && python3 test/test_csv.py ../test-data/prov_unpack_to_pack.csv"
    if command -v php >/dev/null 2>&1; then
        run_benchmark "PHP" "cd php && php test/test_csv.php ../test-data/prov_unpack_to_pack.csv"
    fi
    if command -v ruby >/dev/null 2>&1; then
        run_benchmark "Ruby" "cd ruby && ruby test/test_csv.rb ../test-data/prov_unpack_to_pack.csv"
    fi
    run_benchmark "Perl" "cd perl && perl test/test_csv.pl ../test-data/prov_unpack_to_pack.csv"
    run_benchmark "Tcl" "cd tcl && tclsh test/test_csv.tcl ../test-data/prov_unpack_to_pack.csv"

    echo ""
    echo "=== Summary ==="
    echo ""

    declare -a INDEXED
    for i in "${!LANGUAGES[@]}"; do
        INDEXED+=("${RATES[$i]}:${LANGUAGES[$i]}:${TIMES_MS[$i]}")
    done
    IFS=$'\n' SORTED=($(sort -t: -k1 -rn <<<"${INDEXED[*]}"))
    unset IFS

    printf "%-12s %10s %18s %11s\n" "Language" "Time (ms)" "Rate (entries/sec)" "Relative"
    printf "%-12s %10s %18s %11s\n" "----------" "---------" "------------------" "--------"

    FASTEST_RATE=$(echo "${SORTED[0]}" | cut -d: -f1)
    for item in "${SORTED[@]}"; do
        rate=$(echo "$item" | cut -d: -f1)
        lang=$(echo "$item" | cut -d: -f2)
        time_ms=$(echo "$item" | cut -d: -f3)
        relative=$(echo "scale=2; $rate / $FASTEST_RATE" | bc)
        printf "%-12s %10d %18.0f %10.2fx\n" "$lang" "$time_ms" "$rate" "$relative"
    done
fi

echo ""
echo "Test data: 2,021,090 designation conversions"
echo ""
echo "Note: Unpack has ~2,625 'failures' which are old-style format normalizations (expected)."
echo "      pack(unpack(y)) = y should PASS for all implementations (confirms lossless packed round-trip)."
