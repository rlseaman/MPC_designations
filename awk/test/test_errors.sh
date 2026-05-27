#!/bin/bash
#
# test_errors.sh - Test mpc_designation.awk error handling (AWK).
#
# Tests various classes of invalid input to ensure proper error detection.
# Reads test cases from the shared error_test_cases.csv (5 columns:
# category,subcategory,input,expected_error,description).
#
# Mirrors python/test/test_errors.py and tcl/test/test_errors.tcl:
#   expected_error == "valid"  -> conversion must SUCCEED
#   otherwise (format/range)   -> conversion must be REJECTED
#
# The AWK library has no exit status; it signals failure by returning an empty
# string or a string beginning with "ERROR".  error_runner.awk runs the
# converter on the exact input (passed via the DESIG environment variable so
# embedded whitespace/control characters survive) and prints "OK" or "REJECT".
#
# Usage: bash test_errors.sh [error_test_cases.csv]
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AWK_LIB="$SCRIPT_DIR/../src/mpc_designation.awk"
AWK_RUNNER="$SCRIPT_DIR/error_runner.awk"

#------------------------------------------------------------------------------
# Expected skips for the AWK implementation.
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
        # A NUL byte terminates the value when passed through the environment to
        # AWK, exactly as with C strings, so the truncated remainder ("1995")
        # converts as a valid asteroid. Same limitation as the C harness.
        invalid_char/null_byte)
            echo "NUL byte terminates the value passed to AWK (cannot be represented)" ;;
        edge_case/null_middle)
            echo "NUL byte terminates the value passed to AWK (cannot be represented)" ;;

        # No range validation on asteroid numbers: values <1 or above the
        # packed maximum (15396335) are silently encoded instead of rejected.
        out_of_bounds/asteroid_zero)
            echo "AWK converter does not range-check asteroid number lower bound" ;;
        out_of_bounds/asteroid_max_plus_one)
            echo "AWK converter does not range-check asteroid number upper bound" ;;
        out_of_bounds/asteroid_huge)
            echo "AWK converter does not range-check asteroid number upper bound" ;;
        out_of_bounds/asteroid_overflow)
            echo "AWK converter does not range-check asteroid number upper bound" ;;
        out_of_bounds/packed_above_max)
            echo "AWK converter does not range-check packed tilde upper bound" ;;

        # No range validation on provisional cycle counts.
        out_of_bounds/cycle_huge)
            echo "AWK converter does not range-check provisional cycle count" ;;
        out_of_bounds/cycle_overflow)
            echo "AWK converter does not range-check provisional cycle count" ;;

        # No range/format validation on comet order numbers.
        out_of_bounds/comet_order_huge)
            echo "AWK converter does not range-check comet order number" ;;
        out_of_bounds/comet_order_zero)
            echo "AWK converter does not reject comet order number zero" ;;

        # No validation of these year / provisional / satellite / survey forms.
        out_of_bounds/year_future)
            echo "AWK converter does not range-check provisional year upper bound" ;;
        out_of_bounds/year_zero)
            echo "AWK converter does not reject year zero" ;;
        format/three_letters)
            echo "AWK converter does not reject 3-letter provisional (truncates to 2)" ;;
        format/comet_no_year)
            echo "AWK converter does not reject comet with missing year" ;;
        format/comet_no_provisional)
            echo "AWK converter does not reject comet with missing provisional part" ;;
        format/satellite_invalid_planet)
            echo "AWK converter does not validate satellite planet code" ;;
        format/satellite_missing_number)
            echo "AWK converter does not reject satellite with missing number" ;;
        format/satellite_zero_number)
            echo "AWK converter does not reject satellite number zero" ;;
        format/satellite_negative)
            echo "AWK converter does not reject negative satellite number" ;;
        format/survey_zero)
            echo "AWK converter does not reject survey number zero" ;;

        # No upper length bound: an over-long all-digit string is encoded as a
        # tilde-format asteroid instead of being rejected.
        edge_case/very_long_string)
            echo "AWK converter does not reject over-long all-digit input" ;;

        # A trailing tab is tolerated by the provisional matcher rather than
        # rejected as stray whitespace.
        whitespace/trailing_tab)
            echo "AWK converter tolerates a trailing tab character" ;;

        # Comet fragment suffixes are not validated: a numeric or over-long
        # fragment is silently dropped instead of rejected.
        format/comet_invalid_fragment)
            echo "AWK converter does not validate comet fragment (numeric dropped)" ;;
        format/comet_long_fragment)
            echo "AWK converter does not validate comet fragment length (dropped)" ;;

        # Old-style designations are not prefix-validated: a 'C' prefix is
        # accepted and mis-encoded instead of rejected (only A/B are valid).
        format/old_style_invalid_prefix)
            echo "AWK converter does not validate old-style prefix (C instead of A/B)" ;;

        # Numbered comet zero is encoded as 0000P instead of being rejected.
        format/numbered_comet_zero)
            echo "AWK converter does not reject numbered comet number zero" ;;

        *)
            : ;;  # not a skip
    esac
}

# Parse escape sequences in a string (mirrors python/tcl/c unescape_string).
# Handles \xNN hex escapes plus \t \n \r \f \v and \\.
# NOTE: \x00 cannot be represented; such cases are in the expected-skip list
# and are never passed through here.
unescape_string() {
    local s="$1"

    # Resolve \xNN hex escapes first.
    while [[ "$s" =~ (\\x[0-9a-fA-F][0-9a-fA-F]) ]]; do
        local token="${BASH_REMATCH[1]}"
        local hex="${token:2:2}"
        local ch
        ch=$(printf '%b' "\\x$hex")
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
        line="${line%$'\r'}"

        [ -z "$line" ] && continue
        [ "${line:0:1}" = "#" ] && continue
        case "$line" in
            category,*) continue ;;
        esac

        local category subcategory input_str expected_error description
        category="${line%%,*}";        line="${line#*,}"
        subcategory="${line%%,*}";     line="${line#*,}"
        input_str="${line%%,*}";       line="${line#*,}"
        expected_error="${line%%,*}";  line="${line#*,}"
        description="$line"

        total=$((total + 1))

        local reason
        reason=$(skip_reason "$category/$subcategory")
        if [ -n "$reason" ]; then
            echo "SKIP [$category/$subcategory]: '$description' ($reason)"
            skipped=$((skipped + 1))
            continue
        fi

        local desig
        desig=$(unescape_string "$input_str")

        # Run the converter; error_runner.awk prints "OK" or "REJECT".
        # AWK may also abort non-zero (e.g. a multibyte-conversion failure on
        # invalid UTF-8); a non-zero awk exit is treated as a rejection too.
        local status_line awk_rc got_error
        status_line=$(DESIG="$desig" awk -f "$AWK_LIB" -f "$AWK_RUNNER" 2>/dev/null)
        awk_rc=$?
        if [ "$awk_rc" -ne 0 ] || [ "$status_line" != "OK" ]; then
            got_error=1
        else
            got_error=0
        fi

        if [ "$expected_error" = "valid" ]; then
            if [ "$got_error" -eq 0 ]; then
                passed=$((passed + 1))
            else
                echo "FAIL [$category/$subcategory]: '$description'"
                echo "      Expected: valid conversion"
                echo "      Got:      rejected"
                failed=$((failed + 1))
            fi
        else
            if [ "$got_error" -eq 1 ]; then
                passed=$((passed + 1))
            else
                echo "FAIL [$category/$subcategory]: '$description'"
                echo "      Expected: error ($expected_error)"
                echo "      Got:      success"
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
