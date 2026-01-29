#!/usr/bin/env tclsh
#
# example_usage.tcl - Example usage of the MPC designation library
#
# Run: tclsh example_usage.tcl
#

source [file join [file dirname [info script]] .. src mpc_designation.tcl]

# Example 1: Simple conversion
proc exampleSimpleConversion {} {
    puts "=== Simple Conversion ==="

    set designations {
        "1995 XA"
        "J95X00A"
        "1"
        "C/1995 O1"
        "S/2019 S 22"
    }

    foreach des $designations {
        if {[catch {set result [MPCDesignation::convertSimple $des]} err]} {
            puts "  $des -> Error: $err"
        } else {
            puts "  $des -> $result"
        }
    }
    puts ""
}

# Example 2: Conversion with format information
proc exampleWithInfo {} {
    puts "=== Conversion with Info ==="

    set result [MPCDesignation::convert "1995 XA"]
    puts "  Input:   [dict get $result input]"
    puts "  Output:  [dict get $result output]"
    puts "  Format:  [dict get $result format]"
    puts "  Type:    [dict get $result subtype]"
    puts ""
}

# Example 3: Error handling
proc exampleErrorHandling {} {
    puts "=== Error Handling ==="

    set invalid {
        "invalid"
        ""
        "99999999999"
    }

    foreach inp $invalid {
        if {[catch {MPCDesignation::convertSimple $inp} err]} {
            puts "  '$inp' -> $err"
        }
    }
    puts ""
}

# Example 4: Bidirectional conversion
proc exampleBidirectional {} {
    puts "=== Bidirectional Conversion ==="

    # Pack
    set packed [MPCDesignation::convertSimple "2024 AB"]
    puts "  Pack:   '2024 AB' -> '$packed'"

    # Unpack
    set unpacked [MPCDesignation::convertSimple $packed]
    puts "  Unpack: '$packed' -> '$unpacked'"
    puts ""
}

# Example 5: Format detection
proc exampleFormatDetection {} {
    puts "=== Format Detection ==="

    set examples {"1995 XA" "J95X00A" "1P" "00001"}

    foreach des $examples {
        set info [MPCDesignation::detectFormat $des]
        puts [format "  %-12s -> format=%s, type=%s" \
            $des [dict get $info format] [dict get $info type]]
    }
    puts ""
}

# Example 6: Different designation types
proc exampleDesignationTypes {} {
    puts "=== Designation Types ==="

    set examples {
        {"1" "Numbered asteroid (Ceres)"}
        {"100001" "High numbered asteroid"}
        {"1995 XA" "Provisional asteroid"}
        {"2040 P-L" "Survey asteroid (Palomar-Leiden)"}
        {"1P" "Numbered comet (Halley)"}
        {"C/1995 O1" "Provisional comet (Hale-Bopp)"}
        {"D/1993 F2-B" "Comet fragment (Shoemaker-Levy 9)"}
        {"S/2019 S 22" "Natural satellite (Saturn moon)"}
    }

    foreach example $examples {
        lassign $example inp description
        set result [MPCDesignation::convertSimple $inp]
        puts [format "  %-15s -> %-12s (%s)" $inp $result $description]
    }
    puts ""
}

# Example 7: Batch processing
proc exampleBatchProcessing {} {
    puts "=== Batch Processing ==="

    set batch {
        "1"
        "1995 XA"
        "C/1995 O1"
        "invalid"
        "S/2019 S 22"
    }

    set success 0
    set failure 0

    foreach des $batch {
        if {[catch {MPCDesignation::convertSimple $des}]} {
            incr failure
        } else {
            incr success
        }
    }

    puts "  Processed [expr {$success + $failure}] designations"
    puts "  Success: $success"
    puts "  Failure: $failure"
    puts ""
}

# Main
puts "MPC Designation Library - Example Usage"
puts "========================================\n"

exampleSimpleConversion
exampleWithInfo
exampleErrorHandling
exampleBidirectional
exampleFormatDetection
exampleDesignationTypes
exampleBatchProcessing
