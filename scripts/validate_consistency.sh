#!/bin/bash
#
# validate_consistency.sh - Verify all implementations produce identical output
#
# Usage: ./scripts/validate_consistency.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Test designations covering different types
TEST_CASES=(
    "1"
    "99999"
    "100001"
    "620000"
    "1995 XA"
    "2024 AB"
    "2040 P-L"
    "1P"
    "C/1995 O1"
    "D/1993 F2-B"
    "S/2019 S 22"
    "J95X00A"
    "00001"
    "A0001"
    "CJ95O010"
)

echo "=== Cross-Language Consistency Validation ==="
echo ""

# Build C if needed
if [ ! -f "$ROOT_DIR/c/mpc_designation" ]; then
    echo "Building C implementation..."
    (cd "$ROOT_DIR/c" && make mpc_designation)
fi

FAILED=0

for input in "${TEST_CASES[@]}"; do
    # Get outputs from each implementation
    C_OUTPUT=$("$ROOT_DIR/c/mpc_designation" "$input" 2>&1) || true
    PY_OUTPUT=$(python3 "$ROOT_DIR/python/src/mpc_designation/mpc_designation.py" "$input" 2>&1) || true
    TCL_OUTPUT=$(tclsh "$ROOT_DIR/tcl/src/mpc_designation_cli.tcl" "$input" 2>&1) || true

    # Compare outputs
    if [ "$C_OUTPUT" = "$PY_OUTPUT" ] && [ "$PY_OUTPUT" = "$TCL_OUTPUT" ]; then
        echo "  PASS: '$input' -> '$C_OUTPUT'"
    else
        echo "  FAIL: '$input'"
        echo "    C:      '$C_OUTPUT'"
        echo "    Python: '$PY_OUTPUT'"
        echo "    TCL:    '$TCL_OUTPUT'"
        FAILED=$((FAILED + 1))
    fi
done

echo ""
echo "=== Results ==="
echo "Total:  ${#TEST_CASES[@]}"
echo "Passed: $((${#TEST_CASES[@]} - FAILED))"
echo "Failed: $FAILED"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "VALIDATION FAILED: Implementations produce inconsistent output"
    exit 1
else
    echo ""
    echo "All implementations produce consistent output."
    exit 0
fi
