#!/bin/bash
# test_helpers.sh - Test helper functions in MPC designation converter
#
# Tests format conversion (minimal <-> 12-char report format),
# fragment extraction, and designation comparison functions.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../src/mpc_designation.sh"

PASS=0
FAIL=0

test_to_report() {
    local input="$1"
    local expected="$2"
    local desc="$3"
    local result
    result=$(to_report_format "$input")

    if [ "$result" = "$expected" ]; then
        ((PASS++))
        echo "  PASS: to_report_format(\"$input\") -> \"$result\""
    else
        ((FAIL++))
        echo "  FAIL: to_report_format(\"$input\"): expected \"$expected\", got \"$result\""
    fi
}

test_from_report() {
    local input="$1"
    local expected="$2"
    local desc="$3"
    local result
    result=$(from_report_format "$input")

    if [ "$result" = "$expected" ]; then
        ((PASS++))
        echo "  PASS: from_report_format(\"$input\") -> \"$result\""
    else
        ((FAIL++))
        echo "  FAIL: from_report_format(\"$input\"): expected \"$expected\", got \"$result\""
    fi
}

test_has_fragment() {
    local input="$1"
    local expected="$2"
    local desc="$3"

    if has_fragment "$input"; then
        if [ "$expected" = "true" ]; then
            ((PASS++))
            echo "  PASS: has_fragment(\"$input\") -> 1"
        else
            ((FAIL++))
            echo "  FAIL: has_fragment(\"$input\"): expected 0, got 1"
        fi
    else
        if [ "$expected" = "false" ]; then
            ((PASS++))
            echo "  PASS: has_fragment(\"$input\") -> 0"
        else
            ((FAIL++))
            echo "  FAIL: has_fragment(\"$input\"): expected 1, got 0"
        fi
    fi
}

test_get_fragment() {
    local input="$1"
    local expected="$2"
    local desc="$3"
    local result
    result=$(get_fragment "$input")

    if [ "$result" = "$expected" ]; then
        ((PASS++))
        echo "  PASS: get_fragment(\"$input\") -> \"$result\""
    else
        ((FAIL++))
        echo "  FAIL: get_fragment(\"$input\"): expected \"$expected\", got \"$result\""
    fi
}

test_get_parent() {
    local input="$1"
    local expected="$2"
    local desc="$3"
    local result
    result=$(get_parent "$input")

    if [ "$result" = "$expected" ]; then
        ((PASS++))
        echo "  PASS: get_parent(\"$input\") -> \"$result\""
    else
        ((FAIL++))
        echo "  FAIL: get_parent(\"$input\"): expected \"$expected\", got \"$result\""
    fi
}

test_equal() {
    local d1="$1"
    local d2="$2"
    local expected="$3"
    local desc="$4"

    if designations_equal "$d1" "$d2"; then
        if [ "$expected" = "true" ]; then
            ((PASS++))
            echo "  PASS: designations_equal(\"$d1\", \"$d2\") -> 1"
        else
            ((FAIL++))
            echo "  FAIL: designations_equal(\"$d1\", \"$d2\"): expected 0, got 1"
        fi
    else
        if [ "$expected" = "false" ]; then
            ((PASS++))
            echo "  PASS: designations_equal(\"$d1\", \"$d2\") -> 0"
        else
            ((FAIL++))
            echo "  FAIL: designations_equal(\"$d1\", \"$d2\"): expected 1, got 0"
        fi
    fi
}

echo "=== MPC Designation Helper Function Tests (Bash) ==="
echo ""

# Test to_report_format
echo "--- to_report_format ---"

# Numbered asteroids
test_to_report "00001" "       00001" "Numbered asteroid 1"
test_to_report "00433" "       00433" "Numbered asteroid 433"
test_to_report "99999" "       99999" "Numbered asteroid 99999"
test_to_report "A0000" "       A0000" "Numbered asteroid 100000"
test_to_report "~0000" "       ~0000" "Numbered asteroid 620000"

# Provisional asteroids
test_to_report "J95X00A" "     J95X00A" "Provisional 1995 XA"
test_to_report "K24A12B" "     K24A12B" "Provisional 2024 AB12"

# Survey designations
test_to_report "PLS2040" "     PLS2040" "Survey P-L"
test_to_report "T3S3141" "     T3S3141" "Survey T-3"

# Numbered comets
test_to_report "0001P" "0001P       " "Comet 1P"
test_to_report "0073P" "0073P       " "Comet 73P"

# Numbered comets with fragments
test_to_report "0073Pa" "0073P      a" "Comet 73P-A"
test_to_report "0073Pb" "0073P      b" "Comet 73P-B"
test_to_report "0073Paa" "0073P     aa" "Comet 73P-AA"
test_to_report "0073Paz" "0073P     az" "Comet 73P-AZ"
test_to_report "0073Pzz" "0073P     zz" "Comet 73P-ZZ"

# Provisional comets
test_to_report "CJ95O010" "    CJ95O010" "Comet C/1995 O1"
test_to_report "DJ93F020" "    DJ93F020" "Comet D/1993 F2"
test_to_report "DJ93F02a" "    DJ93F02a" "Comet D/1993 F2-A"

# Test from_report_format
echo ""
echo "--- from_report_format ---"

# Numbered asteroids
test_from_report "       00001" "00001" "Numbered asteroid 1"
test_from_report "       00433" "00433" "Numbered asteroid 433"
test_from_report "       A0000" "A0000" "Numbered asteroid 100000"

# Provisional asteroids
test_from_report "     J95X00A" "J95X00A" "Provisional 1995 XA"

# Numbered comets
test_from_report "0073P       " "0073P" "Comet 73P"

# Numbered comets with fragments
test_from_report "0073P      a" "0073Pa" "Comet 73P-A"
test_from_report "0073P     aa" "0073Paa" "Comet 73P-AA"
test_from_report "0073P     az" "0073Paz" "Comet 73P-AZ"

# Provisional comets
test_from_report "    CJ95O010" "CJ95O010" "Comet C/1995 O1"

# Test has_fragment
echo ""
echo "--- has_fragment ---"

# Unpacked with fragments
test_has_fragment "73P-A" "true" "Unpacked numbered comet with fragment"
test_has_fragment "73P-AA" "true" "Unpacked numbered comet with 2-letter fragment"
test_has_fragment "D/1993 F2-A" "true" "Unpacked provisional comet with fragment"
test_has_fragment "P/1930 J1-AA" "true" "Unpacked provisional comet with 2-letter fragment"

# Unpacked without fragments
test_has_fragment "73P" "false" "Unpacked numbered comet no fragment"
test_has_fragment "C/1995 O1" "false" "Unpacked provisional comet no fragment"

# Packed with fragments
test_has_fragment "0073Pa" "true" "Packed numbered comet with fragment"
test_has_fragment "0073Paa" "true" "Packed numbered comet with 2-letter fragment"
test_has_fragment "DJ93F02a" "true" "Packed provisional comet with fragment"

# Packed without fragments
test_has_fragment "0073P" "false" "Packed numbered comet no fragment"
test_has_fragment "CJ95O010" "false" "Packed provisional comet no fragment"

# Non-comets
test_has_fragment "1995 XA" "false" "Asteroid no fragment"
test_has_fragment "00001" "false" "Numbered asteroid"

# Test get_fragment
echo ""
echo "--- get_fragment ---"

# Unpacked with fragments
test_get_fragment "73P-A" "A" "Unpacked single fragment"
test_get_fragment "73P-AA" "AA" "Unpacked 2-letter fragment"
test_get_fragment "73P-I" "I" "Unpacked fragment I"
test_get_fragment "D/1993 F2-B" "B" "Unpacked provisional fragment"
test_get_fragment "P/1930 J1-AZ" "AZ" "Unpacked provisional 2-letter"

# Unpacked without fragments
test_get_fragment "73P" "" "Unpacked no fragment"
test_get_fragment "C/1995 O1" "" "Unpacked provisional no fragment"

# Packed with fragments
test_get_fragment "0073Pa" "A" "Packed single fragment"
test_get_fragment "0073Paa" "AA" "Packed 2-letter fragment"
test_get_fragment "0073Pi" "I" "Packed fragment I"
test_get_fragment "DJ93F02b" "B" "Packed provisional fragment"

# Packed without fragments
test_get_fragment "0073P" "" "Packed no fragment"
test_get_fragment "CJ95O010" "" "Packed provisional no fragment"

# Test get_parent
echo ""
echo "--- get_parent ---"

# Unpacked with fragments
test_get_parent "73P-A" "73P" "Unpacked single fragment"
test_get_parent "73P-AA" "73P" "Unpacked 2-letter fragment"
test_get_parent "D/1993 F2-B" "D/1993 F2" "Unpacked provisional fragment"
test_get_parent "P/1930 J1-AA" "P/1930 J1" "Unpacked provisional 2-letter"

# Unpacked without fragments
test_get_parent "73P" "73P" "Unpacked no fragment"
test_get_parent "C/1995 O1" "C/1995 O1" "Unpacked provisional no fragment"

# Packed with fragments
test_get_parent "0073Pa" "0073P" "Packed single fragment"
test_get_parent "0073Paa" "0073P" "Packed 2-letter fragment"

# Packed without fragments
test_get_parent "0073P" "0073P" "Packed no fragment"

# Non-comets (should return as-is)
test_get_parent "1995 XA" "1995 XA" "Asteroid"
test_get_parent "00001" "00001" "Numbered asteroid"

# Test designations_equal
echo ""
echo "--- designations_equal ---"

# Same designation, different formats
test_equal "1995 XA" "J95X00A" "true" "Provisional packed/unpacked"
test_equal "73P" "0073P" "true" "Numbered comet packed/unpacked"
test_equal "73P-A" "0073Pa" "true" "Comet with fragment packed/unpacked"
test_equal "73P-AA" "0073Paa" "true" "Comet with 2-letter fragment"
test_equal "1" "00001" "true" "Numbered asteroid"
test_equal "C/1995 O1" "CJ95O010" "true" "Provisional comet"

# Different designations
test_equal "1995 XA" "1995 XB" "false" "Different provisional"
test_equal "73P-A" "73P-B" "false" "Different fragments"
test_equal "73P" "74P" "false" "Different comet numbers"
test_equal "1" "2" "false" "Different asteroid numbers"

# Same designation (both packed or both unpacked)
test_equal "1995 XA" "1995 XA" "true" "Same unpacked"
test_equal "J95X00A" "J95X00A" "true" "Same packed"

# Summary
echo ""
echo "=================================================="
echo "Total: $((PASS + FAIL)), Passed: $PASS, Failed: $FAIL"

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
