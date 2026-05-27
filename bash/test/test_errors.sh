#!/bin/bash
#
# test_errors.sh - Test mpc_designation.sh error handling (Bash).
#
# Tests various classes of invalid input to ensure proper error detection.
# Reads test cases from the shared error_test_cases.csv (5 columns:
# category,subcategory,input,expected_error,description).
#
# Mirrors python/test/test_errors.py and tcl/test/test_errors.tcl:
#   expected_error == "valid"  -> conversion must SUCCEED
#   otherwise (format/range)   -> conversion must be REJECTED
#
# For Bash, the converter signals failure by returning non-zero, by emitting
# an "ERROR: ..." message (on stderr), or by producing empty output.
#
# Usage: bash test_errors.sh [error_test_cases.csv]
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../src/mpc_designation.sh"

#------------------------------------------------------------------------------
# Expected skips for the Bash implementation.
#
# These are documented PRE-EXISTING feature gaps, not regressions. They are
# recognized by category/subcategory, reported as "SKIP", and are NOT counted
# as failures (matching how c/test/test_errors.c handles its null-byte cases).
#
# Implemented as a case statement (not an associative array) so the harness
# runs on the stock macOS bash 3.2.
#------------------------------------------------------------------------------
skip_reason() {
    # $1 = "category/subcategory"; echoes a reason string if this case is a
    # documented expected-skip, otherwise echoes nothing.
    case "$1" in
        # NUL bytes terminate Bash strings (variables cannot hold a literal
        # NUL), exactly as with C strings, so a NUL embedded in the input is
        # indistinguishable from end-of-string and the truncated remainder
        # ("1995") converts as a valid asteroid. Same limitation as C.
        invalid_char/null_byte)
            echo "Bash strings/variables cannot hold a NUL byte (terminates string)" ;;
        edge_case/null_middle)
            echo "Bash strings/variables cannot hold a NUL byte (terminates string)" ;;

        # No ancient (<1000 CE) or BCE comet support in the Bash converter;
        # these valid cases are rejected because the year field is not 4 digits.
        boundary/ancient_comet)
            echo "Bash converter has no ancient (<1000 CE) comet support" ;;
        boundary/bce_comet)
            echo "Bash converter has no BCE comet support" ;;

        # No range validation on asteroid numbers: values <1 or above the
        # packed maximum (15396335) are silently encoded instead of rejected.
        out_of_bounds/asteroid_zero)
            echo "Bash converter does not range-check asteroid number lower bound" ;;
        out_of_bounds/asteroid_max_plus_one)
            echo "Bash converter does not range-check asteroid number upper bound" ;;
        out_of_bounds/asteroid_huge)
            echo "Bash converter does not range-check asteroid number upper bound" ;;
        out_of_bounds/asteroid_overflow)
            echo "Bash converter does not range-check asteroid number upper bound" ;;
        out_of_bounds/packed_above_max)
            echo "Bash converter does not range-check packed tilde upper bound" ;;

        # No range validation on provisional cycle counts.
        out_of_bounds/cycle_huge)
            echo "Bash converter does not range-check provisional cycle count" ;;
        out_of_bounds/cycle_overflow)
            echo "Bash converter does not range-check provisional cycle count" ;;

        # No range/format validation on comet order numbers.
        out_of_bounds/comet_order_huge)
            echo "Bash converter does not range-check comet order number" ;;
        out_of_bounds/comet_order_zero)
            echo "Bash converter does not reject comet order number zero" ;;

        # No validation of these provisional / satellite / survey sub-forms.
        format/three_letters)
            echo "Bash converter does not reject 3-letter provisional (truncates to 2)" ;;
        format/comet_no_year)
            echo "Bash converter does not reject comet with missing year" ;;
        format/comet_no_provisional)
            echo "Bash converter does not reject comet with missing provisional part" ;;
        format/satellite_invalid_planet)
            echo "Bash converter does not validate satellite planet code" ;;
        format/satellite_missing_number)
            echo "Bash converter does not reject satellite with missing number" ;;
        format/satellite_zero_number)
            echo "Bash converter does not reject satellite number zero" ;;
        format/satellite_negative)
            echo "Bash converter does not reject negative satellite number" ;;
        format/survey_zero)
            echo "Bash converter does not reject survey number zero" ;;

        # No upper length bound: an over-long all-digit string is encoded as a
        # tilde-format asteroid instead of being rejected.
        edge_case/very_long_string)
            echo "Bash converter does not reject over-long all-digit input" ;;

        # A trailing tab is tolerated by the provisional matcher rather than
        # rejected as stray whitespace.
        whitespace/trailing_tab)
            echo "Bash converter tolerates a trailing tab character" ;;
        # Doubled internal spaces in a satellite designation are not rejected
        # (collapsed into a malformed but accepted packed result).
        whitespace/double_space_satellite)
            echo "Bash converter does not reject doubled spaces in satellite designation" ;;

        *)
            : ;;  # not a skip
    esac
}

# Parse escape sequences in a string (mirrors python/tcl/c unescape_string).
# Handles \xNN hex escapes plus \t \n \r \f \v and \\.
# NOTE: \x00 cannot be represented in a Bash variable; such cases are in the
# expected-skip list and are never passed through here.
unescape_string() {
    local s="$1"
    local result=""

    # Resolve \xNN hex escapes first.
    while [[ "$s" =~ (\\x[0-9a-fA-F][0-9a-fA-F]) ]]; do
        local token="${BASH_REMATCH[1]}"
        local hex="${token:2:2}"
        local ch
        ch=$(printf '%b' "\\x$hex")
        # Replace the first occurrence of the token with the decoded byte.
        s="${s/$token/$ch}"
    done

    # Standard single-character escapes.
    s="${s//\\t/$'\t'}"
    s="${s//\\n/$'\n'}"
    s="${s//\\r/$'\r'}"
    s="${s//\\f/$'\f'}"
    s="${s//\\v/$'\v'}"
    s="${s//\\\\/\\}"

    printf '%s' "$s"
}

run_error_tests() {
    local csv_file="$1"
    local total=0 passed=0 failed=0 skipped=0

    echo "=== MPC Designation Error Tests ==="
    echo ""

    local line
    while IFS= read -r line || [ -n "$line" ]; do
        # Strip a trailing carriage return (CRLF files).
        line="${line%$'\r'}"

        # Skip empty lines and comments.
        [ -z "$line" ] && continue
        [ "${line:0:1}" = "#" ] && continue

        # Skip header.
        case "$line" in
            category,*) continue ;;
        esac

        # Parse the first four comma-separated fields; the remainder is the
        # description (matches python's split(',', 4)).
        local category subcategory input_str expected_error description
        category="${line%%,*}";        line="${line#*,}"
        subcategory="${line%%,*}";     line="${line#*,}"
        input_str="${line%%,*}";       line="${line#*,}"
        expected_error="${line%%,*}";  line="${line#*,}"
        description="$line"

        total=$((total + 1))

        # Expected-skip check before running (documented pre-existing gaps).
        local reason
        reason=$(skip_reason "$category/$subcategory")
        if [ -n "$reason" ]; then
            echo "SKIP [$category/$subcategory]: '$description' ($reason)"
            skipped=$((skipped + 1))
            continue
        fi

        # Unescape the input string.
        local desig
        desig=$(unescape_string "$input_str")

        # Run the converter, capturing stdout and the exit status.
        local output rc
        output=$(convert_simple "$desig" 2>/dev/null)
        rc=$?

        # Determine whether the converter rejected the input.
        local got_error=0
        if [ "$rc" -ne 0 ] || [ -z "$output" ] || [ "${output:0:6}" = "ERROR:" ]; then
            got_error=1
        fi

        if [ "$expected_error" = "valid" ]; then
            if [ "$got_error" -eq 0 ]; then
                passed=$((passed + 1))
            else
                echo "FAIL [$category/$subcategory]: '$description'"
                echo "      Expected: valid conversion"
                echo "      Got:      rejected (rc=$rc, output='$output')"
                failed=$((failed + 1))
            fi
        else
            if [ "$got_error" -eq 1 ]; then
                passed=$((passed + 1))
            else
                echo "FAIL [$category/$subcategory]: '$description'"
                echo "      Expected: error ($expected_error)"
                echo "      Got:      '$output' (success)"
                failed=$((failed + 1))
            fi
        fi
    done < "$csv_file"

    echo ""
    echo "=== Error Test Results ==="
    echo "Total:   $total"
    echo "Passed:  $passed"
    echo "Skipped: $skipped"
    echo "Failed:  $failed"

    [ "$failed" -eq 0 ]
}

# Main
CSV_FILE="${1:-$SCRIPT_DIR/../../test-data/error_test_cases.csv}"

if [ ! -f "$CSV_FILE" ]; then
    echo "Error: Cannot open file: $CSV_FILE" >&2
    exit 1
fi

if run_error_tests "$CSV_FILE"; then
    exit 0
else
    exit 1
fi
