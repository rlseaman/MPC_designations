#!/usr/bin/env tclsh
#
# Test MPC designation handling of comet fragments
#
# Tests:
# 1. Numbered comets with single-letter fragments (73P-A)
# 2. Numbered comets with two-letter fragments (73P-AA)
# 3. Provisional comets with single-letter fragments
# 4. Provisional comets with two-letter fragments
# 5. Round-trip verification
#

source [file join [file dirname [info script]] .. src mpc_designation.tcl]

set passed 0
set failed 0

proc test {description expected actual} {
    global passed failed
    if {$expected eq $actual} {
        puts "PASS: $description"
        incr passed
    } else {
        puts "FAIL: $description"
        puts "      Expected: $expected"
        puts "      Got:      $actual"
        incr failed
    }
}

proc test_pack {input expected} {
    set result [MPCDesignation::convertSimple $input]
    test "pack $input" $expected $result
}

proc test_unpack {input expected} {
    set result [MPCDesignation::convertSimple $input]
    test "unpack $input" $expected $result
}

proc test_roundtrip {unpacked packed} {
    set pack_result [MPCDesignation::convertSimple $unpacked]
    set unpack_result [MPCDesignation::convertSimple $packed]
    set repack_result [MPCDesignation::convertSimple $unpack_result]

    test "pack $unpacked" $packed $pack_result
    test "unpack $packed" $unpacked $unpack_result
    test "roundtrip $unpacked" $packed $repack_result
}

puts "=== Numbered Comet Fragment Tests ==="
puts ""

# Numbered comets without fragments
puts "--- Numbered comets without fragments ---"
test_roundtrip "1P" "0001P"
test_roundtrip "73P" "0073P"
test_roundtrip "354D" "0354D"
test_roundtrip "9999P" "9999P"

puts ""

# Numbered comets with single-letter fragments
puts "--- Numbered comets with single-letter fragments ---"
test_roundtrip "73P-A" "0073Pa"
test_roundtrip "73P-B" "0073Pb"
test_roundtrip "73P-H" "0073Ph"
test_roundtrip "73P-I" "0073Pi"
test_roundtrip "73P-J" "0073Pj"
test_roundtrip "73P-Z" "0073Pz"
test_roundtrip "1P-A" "0001Pa"
test_roundtrip "9999D-Z" "9999Dz"

puts ""

# Numbered comets with two-letter fragments
puts "--- Numbered comets with two-letter fragments ---"
test_roundtrip "73P-AA" "0073Paa"
test_roundtrip "73P-AB" "0073Pab"
test_roundtrip "73P-AI" "0073Pai"
test_roundtrip "73P-AZ" "0073Paz"
test_roundtrip "73P-BA" "0073Pba"
test_roundtrip "73P-BI" "0073Pbi"
test_roundtrip "73P-BZ" "0073Pbz"
test_roundtrip "73P-ZZ" "0073Pzz"
test_roundtrip "1P-AA" "0001Paa"
test_roundtrip "9999P-ZZ" "9999Pzz"

puts ""

# Provisional comets with single-letter fragments
puts "--- Provisional comets with single-letter fragments ---"
test_roundtrip "P/1930 J1-A" "PJ30J01a"
test_roundtrip "D/1993 F2-B" "DJ93F02b"
test_roundtrip "C/1995 O1-C" "CJ95O01c"

puts ""

# Provisional comets with two-letter fragments
puts "--- Provisional comets with two-letter fragments ---"
test_roundtrip "P/1930 J1-AA" "PJ30J01aa"
test_roundtrip "P/1930 J1-AI" "PJ30J01ai"
test_roundtrip "P/1930 J1-AZ" "PJ30J01az"
test_roundtrip "P/1930 J1-BA" "PJ30J01ba"
test_roundtrip "P/1930 J1-BI" "PJ30J01bi"

puts ""

# Edge cases
puts "--- Edge cases ---"
test_roundtrip "1P-A" "0001Pa"
test_roundtrip "9999P-ZZ" "9999Pzz"
test_pack "73P/Schwassmann-Wachmann" "0073P"

puts ""
puts "=== Summary ==="
puts "Passed: $passed"
puts "Failed: $failed"

exit [expr {$failed > 0 ? 1 : 0}]
