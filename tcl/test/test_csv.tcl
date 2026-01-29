#!/usr/bin/env tclsh
#
# Test mpc_designation.tcl against CSV file of known conversions
#

source [file join [file dirname [info script]] .. src mpc_designation.tcl]

proc runTests {csvFile {maxErrors 100}} {
    set fp [open $csvFile r]
    gets $fp header  ;# skip header line

    set total 0
    set passed 0
    set failed 0
    set errors {}

    set startTime [clock milliseconds]

    while {[gets $fp line] >= 0} {
        incr total
        set parts [split $line ,]
        set unpacked [lindex $parts 0]
        set expectedPacked [lindex $parts 1]

        # Test unpacked -> packed
        if {[catch {set gotPacked [MPCDesignation::convertSimple $unpacked]} err]} {
            incr failed
            if {[llength $errors] < $maxErrors} {
                lappend errors [list $unpacked "ERROR: $err" $expectedPacked]
            }
            continue
        }

        if {$gotPacked ne $expectedPacked} {
            incr failed
            if {[llength $errors] < $maxErrors} {
                lappend errors [list $unpacked $gotPacked $expectedPacked]
            }
        } else {
            incr passed
        }

        # Progress indicator every 100,000 entries
        if {$total % 100000 == 0} {
            puts "Processed $total entries..."
        }
    }
    close $fp

    set elapsed [expr {[clock milliseconds] - $startTime}]

    puts ""
    puts "=== Test Results ==="
    puts "Total:  $total"
    puts "Passed: $passed"
    puts "Failed: $failed"
    puts "Time:   ${elapsed}ms ([format %.1f [expr {$total * 1000.0 / $elapsed}]] entries/sec)"
    puts ""

    if {$failed > 0} {
        puts "=== First [llength $errors] failures ==="
        puts [format "%-25s %-15s %-15s" "Input" "Got" "Expected"]
        puts [string repeat "-" 60]
        foreach err $errors {
            lassign $err input got expected
            puts [format "%-25s %-15s %-15s" $input $got $expected]
        }
    }

    return [expr {$failed == 0}]
}

# Main
if {$argc < 1} {
    puts "Usage: test_csv.tcl <csv_file> \[max_errors\]"
    exit 1
}

set csvFile [lindex $argv 0]
set maxErrors [expr {$argc > 1 ? [lindex $argv 1] : 100}]

if {![file exists $csvFile]} {
    puts "Error: File not found: $csvFile"
    exit 1
}

set success [runTests $csvFile $maxErrors]
exit [expr {$success ? 0 : 1}]
