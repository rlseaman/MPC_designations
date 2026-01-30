#!/usr/bin/awk -f
# test_roundtrip.awk - Roundtrip test for AWK implementation
#
# Usage: awk -f src/mpc_designation.awk -f test/test_roundtrip.awk <csv_file>

BEGIN {
    FS = ","
    pack_passed = 0
    pack_failed = 0
    unpack_passed = 0
    unpack_failed = 0
    rt_passed = 0
    rt_failed = 0
    first = 1
}

{
    if (first) {
        first = 0
        next  # Skip header
    }

    unpacked = $1
    packed = $2

    # Phase 1: Pack test
    result = convert_simple(unpacked)
    if (result == packed) {
        pack_passed++
    } else {
        pack_failed++
    }

    # Phase 2: Unpack test
    result = convert_simple(packed)
    if (result == unpacked) {
        unpack_passed++
    } else {
        unpack_failed++
    }

    # Round-trip: pack(unpack(packed)) == packed
    unpacked_result = convert_simple(packed)
    repacked_result = convert_simple(unpacked_result)
    if (repacked_result == packed) {
        rt_passed++
    } else {
        rt_failed++
    }
}

END {
    total = pack_passed + pack_failed

    print "=== Phase 1: Pack ==="
    print "Passed: " pack_passed
    print "Failed: " pack_failed
    print ""

    print "=== Phase 2: Unpack ==="
    print "Passed: " unpack_passed
    print "Failed: " unpack_failed
    print ""

    print "=== Round-trip Verification ==="
    if (rt_failed == 0) {
        print "Packed RT: PASS (" rt_passed "/" total ")"
    } else {
        print "Packed RT: FAIL (" rt_failed " failures)"
    }
}
