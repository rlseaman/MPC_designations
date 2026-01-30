#!/usr/bin/awk -f
# test_csv.awk - CSV benchmark test for AWK implementation
#
# Usage: awk -f test_csv.awk -f src/mpc_designation.awk <csv_file>

BEGIN {
    FS = ","
    passed = 0
    failed = 0
    first = 1
}

{
    if (first) {
        first = 0
        next  # Skip header
    }

    unpacked = $1
    expected_packed = $2

    result = convert_simple(unpacked)

    if (result == expected_packed) {
        passed++
    } else {
        failed++
    }
}

END {
    print "Passed: " passed
    print "Failed: " failed
}
