#!/usr/bin/env tclsh
#
# Test MPC designation conversion with bidirectional timing and round-trip verification
#
# Tests:
# 1. Pack direction (unpacked -> packed) with timing
# 2. Unpack direction (packed -> unpacked) with timing
# 3. Unpacked round-trip: unpack(pack(x)) = x
# 4. Packed round-trip: pack(unpack(y)) = y
#
# Usage: tclsh test_roundtrip.tcl <csv_file>
#

source [file join [file dirname [info script]] .. src mpc_designation.tcl]

set MAX_ERRORS 20

proc runRoundtripTests {csvFile} {
    global MAX_ERRORS

    # Load test data
    set fp [open $csvFile r]
    gets $fp header  ;# skip header

    set unpackedList {}
    set packedList {}

    while {[gets $fp line] >= 0} {
        if {$line eq ""} continue
        set parts [split $line ,]
        if {[llength $parts] >= 2} {
            lappend unpackedList [lindex $parts 0]
            lappend packedList [lindex $parts 1]
        }
    }
    close $fp

    set total [llength $unpackedList]
    puts "Loaded $total test cases\n"

    set errors {}

    # ========== Phase 1: Pack (unpacked -> packed) ==========
    puts "=== Phase 1: Pack (unpacked -> packed) ==="
    set packPassed 0
    set packFailed 0
    set startTime [clock milliseconds]

    for {set i 0} {$i < $total} {incr i} {
        set unpacked [lindex $unpackedList $i]
        set expected [lindex $packedList $i]

        if {[catch {set got [MPCDesignation::convertSimple $unpacked]} err]} {
            incr packFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list pack $unpacked "ERROR: $err" $expected]
            }
        } elseif {$got ne $expected} {
            incr packFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list pack $unpacked $got $expected]
            }
        } else {
            incr packPassed
        }
    }

    set packTime [expr {[clock milliseconds] - $startTime}]
    set packRate [expr {$total * 1000.0 / $packTime}]
    puts "Passed: $packPassed"
    puts "Failed: $packFailed"
    puts [format "Time:   %dms (%.1f entries/sec)\n" $packTime $packRate]

    # ========== Phase 2: Unpack (packed -> unpacked) ==========
    puts "=== Phase 2: Unpack (packed -> unpacked) ==="
    set unpackPassed 0
    set unpackFailed 0
    set startTime [clock milliseconds]

    for {set i 0} {$i < $total} {incr i} {
        set packed [lindex $packedList $i]
        set expected [lindex $unpackedList $i]

        if {[catch {set got [MPCDesignation::convertSimple $packed]} err]} {
            incr unpackFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list unpack $packed "ERROR: $err" $expected]
            }
        } elseif {$got ne $expected} {
            incr unpackFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list unpack $packed $got $expected]
            }
        } else {
            incr unpackPassed
        }
    }

    set unpackTime [expr {[clock milliseconds] - $startTime}]
    set unpackRate [expr {$total * 1000.0 / $unpackTime}]
    puts "Passed: $unpackPassed"
    puts "Failed: $unpackFailed"
    puts [format "Time:   %dms (%.1f entries/sec)\n" $unpackTime $unpackRate]

    # ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    puts "=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==="
    set rtUnpackedPassed 0
    set rtUnpackedFailed 0
    set startTime [clock milliseconds]

    for {set i 0} {$i < $total} {incr i} {
        set original [lindex $unpackedList $i]

        if {[catch {
            set packed [MPCDesignation::convertSimple $original]
            set unpacked [MPCDesignation::convertSimple $packed]
        } err]} {
            incr rtUnpackedFailed
            continue
        }

        if {$unpacked ne $original} {
            incr rtUnpackedFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list rt-unp $original "$packed -> $unpacked" $original]
            }
        } else {
            incr rtUnpackedPassed
        }
    }

    set rtUnpackedTime [expr {[clock milliseconds] - $startTime}]
    set rtUnpackedRate [expr {$total * 1000.0 / $rtUnpackedTime}]
    puts "Passed: $rtUnpackedPassed"
    puts "Failed: $rtUnpackedFailed"
    puts [format "Time:   %dms (%.1f entries/sec)\n" $rtUnpackedTime $rtUnpackedRate]

    # ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    puts "=== Phase 4: Packed round-trip: pack(unpack(y)) = y ==="
    set errors {}  ;# Reset to show phase 4 errors
    set rtPackedPassed 0
    set rtPackedFailed 0
    set startTime [clock milliseconds]

    for {set i 0} {$i < $total} {incr i} {
        set original [lindex $packedList $i]

        if {[catch {
            set unpacked [MPCDesignation::convertSimple $original]
            set repacked [MPCDesignation::convertSimple $unpacked]
        } err]} {
            incr rtPackedFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list rt-pak $original "ERROR: $err" $original]
            }
            continue
        }

        if {$repacked ne $original} {
            incr rtPackedFailed
            if {[llength $errors] < $MAX_ERRORS} {
                lappend errors [list rt-pak $original "$unpacked -> $repacked" $original]
            }
        } else {
            incr rtPackedPassed
        }
    }

    set rtPackedTime [expr {[clock milliseconds] - $startTime}]
    set rtPackedRate [expr {$total * 1000.0 / $rtPackedTime}]
    puts "Passed: $rtPackedPassed"
    puts "Failed: $rtPackedFailed"
    puts [format "Time:   %dms (%.1f entries/sec)\n" $rtPackedTime $rtPackedRate]

    # ========== Summary ==========
    puts "=== Summary ==="
    puts [format "%-30s %10s %18s %12s" "Phase" "Time (ms)" "Rate (entries/sec)" "Status"]
    puts [format "%-30s %10s %18s %12s" [string repeat "-" 30] [string repeat "-" 10] [string repeat "-" 18] [string repeat "-" 12]]

    proc status {failed} {
        if {$failed == 0} {
            return "PASS"
        }
        return "FAIL ($failed)"
    }

    puts [format "%-30s %10d %18.1f %12s" "Pack" $packTime $packRate [status $packFailed]]
    puts [format "%-30s %10d %18.1f %12s" "Unpack" $unpackTime $unpackRate [status $unpackFailed]]
    puts [format "%-30s %10d %18.1f %12s" "Unpacked RT: unpack(pack(x))=x" $rtUnpackedTime $rtUnpackedRate [status $rtUnpackedFailed]]
    puts [format "%-30s %10d %18.1f %12s" "Packed RT: pack(unpack(y))=y" $rtPackedTime $rtPackedRate [status $rtPackedFailed]]
    puts ""

    # Show errors
    if {[llength $errors] > 0} {
        puts "=== First [llength $errors] errors ==="
        puts [format "%-8s %-25s %-20s %-20s" "Phase" "Input" "Got" "Expected"]
        puts [format "%-8s %-25s %-20s %-20s" [string repeat "-" 8] [string repeat "-" 25] [string repeat "-" 20] [string repeat "-" 20]]
        foreach err $errors {
            lassign $err phase input got expected
            puts [format "%-8s %-25s %-20s %-20s" $phase $input $got $expected]
        }
    }

    # Exit with error only if pack or packed RT failed
    set totalFailed [expr {$packFailed + $rtPackedFailed}]
    return [expr {$totalFailed == 0}]
}

# Main
if {$argc < 1} {
    puts stderr "Usage: test_roundtrip.tcl <csv_file>"
    exit 1
}

set csvFile [lindex $argv 0]

if {![file exists $csvFile]} {
    puts stderr "Error: File not found: $csvFile"
    exit 1
}

set success [runRoundtripTests $csvFile]
exit [expr {$success ? 0 : 1}]
