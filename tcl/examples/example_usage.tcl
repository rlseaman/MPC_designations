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
    set info [dict get $result info]
    puts "  Input:   [dict get $result input]"
    puts "  Output:  [dict get $result output]"
    puts "  Format:  [dict get $info format]"
    puts "  Type:    [dict get $info subtype]"
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

# Example 8: Unpacking mixed input (valid MPC + temporary identifiers)
# Returns empty string for invalid/temporary identifiers
proc exampleUnpackMixed {} {
    puts "=== Unpack Mixed Input (MPC + Temporary IDs) ==="
    puts "  (Empty result = not a valid MPC designation)\n"

    # Mix of valid packed MPC designations and temporary/internal identifiers
    set identifiers {
        CE4AXE2  CE4JLW2  K22C02Q  K25Y08W  K25Y12R
        K26A01L  K26A02E  K26A03Z  K26A04M  K26B02V
        K26B04M  K26B05X  P12kY0u  P22l0lE  P22l2fg
        P22l2g7  P22l2g8  P22l8O0  P22l8OS  P22l8Rk
        P22lbY0  ST26AC0 z2342
    }

    foreach id $identifiers {
        if {[catch {set unpacked [MPCDesignation::unpack $id]} err]} {
            # Invalid identifier - return empty string
            puts [format "  %-10s -> (invalid)" $id]
        } else {
            puts [format "  %-10s -> %s" $id $unpacked]
        }
    }
    puts ""
}

# Example 9: Packing unpacked designations
# Demonstrates converting human-readable designations to packed format
proc examplePackDesignations {} {
    puts "=== Pack Unpacked Designations ==="

    # Mix of valid unpacked designations
    set designations {
        "2022 CQ2"
        "2025 YW8"
        "2026 AL1"
        "2026 BX5"
        "1"
        "433"
        "100000"
        "1995 XA"
        "1P"
        "C/1995 O1"
    }

    foreach des $designations {
        if {[catch {set packed [MPCDesignation::pack $des]} err]} {
            puts [format "  %-15s -> (invalid)" $des]
        } else {
            puts [format "  %-15s -> %s" $des $packed]
        }
    }
    puts ""
}

# Example 10: Check if packed or unpacked
proc exampleCheckPackedUnpacked {} {
    puts "=== Check Packed vs Unpacked ==="

    set designations {"K26A01L" "2026 AL1" "00433" "433" "CJ95O010" "C/1995 O1"}

    foreach des $designations {
        if {[catch {set info [MPCDesignation::detectFormat $des]} err]} {
            puts [format "  %-12s -> (invalid)" $des]
        } else {
            set format [dict get $info format]
            puts [format "  %-12s -> %s" $des $format]
        }
    }
    puts ""
}

# Example 11: Check if provisional or permanent (numbered)
proc exampleCheckProvisionalPermanent {} {
    puts "=== Check Provisional vs Permanent ==="

    set designations {"2026 AL1" "433" "K26A01L" "00433" "1995 XA" "1P" "C/1995 O1"}

    foreach des $designations {
        if {[catch {set info [MPCDesignation::detectFormat $des]} err]} {
            puts [format "  %-12s -> (invalid)" $des]
        } else {
            set type [dict get $info type]
            # Simplify: "permanent" or "provisional" (or other for comets/satellites)
            if {$type eq "permanent"} {
                set category "permanent (numbered)"
            } elseif {$type in {provisional provisional_extended survey}} {
                set category "provisional"
            } else {
                set category $type  ;# comet_numbered, comet_full, satellite, etc.
            }
            puts [format "  %-12s -> %s" $des $category]
        }
    }
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
exampleUnpackMixed
examplePackDesignations
exampleCheckPackedUnpacked
exampleCheckProvisionalPermanent
