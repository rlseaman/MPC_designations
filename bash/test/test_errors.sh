#!/bin/bash
# test_errors.sh - Test suite for MPC Designation Converter (Bash)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../src/mpc_designation.sh"

PASS=0
FAIL=0

test_convert() {
    local input="$1"
    local expected="$2"
    local result
    result=$(convert_simple "$input")

    if [ "$result" = "$expected" ]; then
        ((PASS++))
    else
        ((FAIL++))
        echo "FAIL: '$input' -> '$result' (expected '$expected')"
    fi
}

echo "Running Bash MPC Designation tests..."
echo ""

# Permanent asteroids - packed to unpacked
test_convert "00001" "1"
test_convert "00123" "123"
test_convert "99999" "99999"
test_convert "A0001" "100001"
test_convert "Z9999" "359999"
test_convert "a0001" "360001"
test_convert "z9999" "619999"
test_convert "~0000" "620000"
test_convert "~000z" "620061"

# Permanent asteroids - unpacked to packed
test_convert "1" "00001"
test_convert "99999" "99999"
test_convert "100001" "A0001"
test_convert "359999" "Z9999"
test_convert "360001" "a0001"
test_convert "619999" "z9999"
test_convert "620000" "~0000"
test_convert "620061" "~000z"

# Provisional designations
test_convert "J95X00A" "1995 XA"
test_convert "K24AB3B" "2024 AB113"
test_convert "1995 XA" "J95X00A"
test_convert "2024 AB113" "K24AB3B"

# Survey designations
test_convert "PLS2040" "2040 P-L"
test_convert "T1S1234" "1234 T-1"
test_convert "T2S0001" "1 T-2"
test_convert "2040 P-L" "PLS2040"
test_convert "1234 T-1" "T1S1234"

# Numbered comets
test_convert "0001P" "1P"
test_convert "0002P" "2P"
test_convert "1P" "0001P"
test_convert "2P" "0002P"

# Provisional comets
test_convert "CJ95O010" "C/1995 O1"
test_convert "C/1995 O1" "CJ95O010"
test_convert "DJ93F02b" "D/1993 F2-B"
test_convert "D/1993 F2-B" "DJ93F02b"

# Natural satellites
test_convert "SK19S220" "S/2019 S 22"
test_convert "S/2019 S 22" "SK19S220"

echo ""
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [ "$FAIL" -eq 0 ]; then
    echo "All tests passed!"
    exit 0
else
    exit 1
fi
