#!/usr/bin/env tclsh
#
# Regression tests for packed-provisional detection collisions.
#
# Motivating case: the Catalina Sky Survey assigns temporary tracklet IDs such
# as "C03UYWZ" that resemble a packed MPC provisional designation. A too-loose
# detection regex routed these into the asteroid-provisional branch. The
# canonical packed provisional is:
#
#     [I-L] [0-9]{2} [A-HJ-Y] [0-9A-Za-z] [0-9] [A-HJ-Z]
#     century  year   half-mon  cycle-hi  cycle-lo  order
#
# Invariants that reject the collisions:
#   1. Century code is I-L (1800-2199); asteroids have no provisional
#      designation before 1892, so A-H (years < 1800) is impossible.
#   2. The cycle's units character (position 6) is always a literal digit.
#   3. Half-month and order letters skip I (and half-month tops out at Y).
#
# Usage: tclsh test/test_detection_collision.tcl
#

source [file join [file dirname [info script]] .. src mpc_designation.tcl]

# Strings that must be REJECTED (not parsed as asteroid provisionals).
set reject {
    {C03UYWZ {Catalina survey temp ID: century C = 1200s, impossible for asteroid}}
    {K03UYWZ {valid century but letter W in digits-only cycle-units position}}
    {A95X00A {century A = 1000s, before any asteroid}}
    {H95X00A {century H = 1700s, before first asteroid provisional (1892)}}
    {K95I00A {invalid half-month I in packed provisional}}
    {K95X0IA {invalid order letter I}}
    {K95Z00A {invalid half-month Z}}
    {{1995 XI} {unpacked: order letter I (pack must reject, not emit J95X00I)}}
    {{A908 CI} {unpacked old-style: order letter I}}
}

# Valid controls that must still convert correctly.
set accept {
    {J95X00A {1995 XA}}
    {I92X00A {A892 XA}}
    {K14A00A {2014 AA}}
    {SK19S220 {S/2019 S 22}}
    {CJ95O010 {C/1995 O1}}
}

set total 0
set passed 0
set failed 0

puts "=== Detection Collision Regression Tests ===\n"

foreach pair $reject {
    lassign $pair s why
    incr total
    if {[catch {MPCDesignation::convertSimple $s} out]} {
        incr passed
    } else {
        puts "FAIL \[reject\] '$s': expected rejection ($why)"
        puts "      got: '$out'"
        incr failed
    }
}

foreach pair $accept {
    lassign $pair s expected
    incr total
    if {[catch {MPCDesignation::convertSimple $s} out]} {
        puts "FAIL \[accept\] '$s': unexpected error $out"
        incr failed
    } elseif {$out eq $expected} {
        incr passed
    } else {
        puts "FAIL \[accept\] '$s': expected '$expected', got '$out'"
        incr failed
    }
}

puts "\nTotal: $total, Passed: $passed, Failed: $failed"
exit [expr {$failed == 0 ? 0 : 1}]
