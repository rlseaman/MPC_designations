#!/usr/bin/env tclsh
#
# Test mpc_designation.tcl error handling.
#
# Tests various classes of invalid input to ensure proper error detection.
# Reads test cases from error_test_cases.csv.
#
# Usage: tclsh test_errors.tcl [error_test_cases.csv]
#

source [file join [file dirname [info script]] .. src mpc_designation.tcl]

# Parse escape sequences in a string
proc unescapeString {s} {
    # Handle \xNN hex escapes
    while {[regexp {\\x([0-9a-fA-F]{2})} $s match hex]} {
        set char [format %c [scan $hex %x]]
        set s [string map [list $match $char] $s]
    }

    # Handle standard escapes
    set s [string map {
        \\n \n
        \\r \r
        \\t \t
        \\f \f
        \\v \v
        \\0 \x00
        \\\\ \\
    } $s]

    return $s
}

proc runErrorTests {csvFile} {
    set fp [open $csvFile r]

    set total 0
    set passed 0
    set failed 0

    puts "=== MPC Designation Error Tests ===\n"

    while {[gets $fp line] >= 0} {
        # Skip empty lines and comments
        set line [string trim $line]
        if {$line eq "" || [string index $line 0] eq "#"} {
            continue
        }

        # Skip header
        if {[string match "category,*" $line]} {
            continue
        }

        # Parse CSV (simple split)
        set parts [split $line ,]
        if {[llength $parts] < 5} {
            continue
        }

        set category [lindex $parts 0]
        set subcategory [lindex $parts 1]
        set inputStr [lindex $parts 2]
        set expectedError [lindex $parts 3]
        set description [lindex $parts 4]

        # Unescape the input string
        set inputStr [unescapeString $inputStr]

        incr total

        # Run the test
        set gotError 0
        set errorMsg ""
        set output ""

        if {[catch {set output [MPCDesignation::convertSimple $inputStr]} err]} {
            set gotError 1
            set errorMsg $err
        }

        set testPassed 0

        if {$expectedError eq "valid"} {
            # Expect success
            if {!$gotError} {
                set testPassed 1
            } else {
                puts "FAIL \[$category/$subcategory\]: '$description'"
                puts "      Expected: valid conversion"
                puts "      Got:      $errorMsg"
                incr failed
            }
        } else {
            # Expect error
            if {$gotError} {
                set testPassed 1
            } else {
                puts "FAIL \[$category/$subcategory\]: '$description'"
                puts "      Expected: error ($expectedError)"
                puts "      Got:      '$output' (success)"
                incr failed
            }
        }

        if {$testPassed} {
            incr passed
        }
    }

    close $fp

    puts "\n=== Error Test Results ==="
    puts "Total:  $total"
    puts "Passed: $passed"
    puts "Failed: $failed"

    return [expr {$failed == 0}]
}

# Main
set csvFile [expr {$argc > 0 ? [lindex $argv 0] : "error_test_cases.csv"}]

if {![file exists $csvFile]} {
    puts "Error: Cannot open file: $csvFile"
    exit 1
}

set success [runErrorTests $csvFile]
exit [expr {$success ? 0 : 1}]
