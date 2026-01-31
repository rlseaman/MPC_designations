#!/usr/bin/env tclsh
#
# mpc_designation_cli.tcl - Command-line interface for MPC designation converter
#
# Usage: mpc_designation_cli.tcl [-v|--verbose] <designation> [designation ...]
#
# This is the CLI wrapper. For library usage, source mpc_designation.tcl directly.
#

source [file join [file dirname [info script]] mpc_designation.tcl]

#
# Print usage information
#
proc printUsage {} {
    puts stderr "Usage: mpc_designation_cli.tcl \[-v|--verbose\] <designation> \[designation ...\]"
    puts stderr ""
    puts stderr "Convert between packed and unpacked asteroid/comet designations."
    puts stderr "The program auto-detects the input format and converts to the other."
    puts stderr ""
    puts stderr "Options:"
    puts stderr "  -v, --verbose   Show detailed information about each conversion"
    puts stderr "  --version       Show version information"
    puts stderr ""
    puts stderr "Multiple designations can be provided; output will be labeled."
    puts stderr ""
    puts stderr "ASTEROID Examples:"
    puts stderr "  Permanent designations:"
    puts stderr "    mpc_designation_cli.tcl 03202      -> 3202"
    puts stderr "    mpc_designation_cli.tcl 3202       -> 03202"
    puts stderr "    mpc_designation_cli.tcl A0345      -> 100345"
    puts stderr "    mpc_designation_cli.tcl 100345     -> A0345"
    puts stderr "    mpc_designation_cli.tcl '~0000'    -> 620000"
    puts stderr "    mpc_designation_cli.tcl 620000     -> ~0000"
    puts stderr ""
    puts stderr "  Provisional designations:"
    puts stderr "    mpc_designation_cli.tcl J95X00A    -> 1995 XA"
    puts stderr "    mpc_designation_cli.tcl '1995 XA'  -> J95X00A"
    puts stderr "    mpc_designation_cli.tcl J98SA8Q    -> 1998 SQ108"
    puts stderr "    mpc_designation_cli.tcl '1998 SQ108' -> J98SA8Q"
    puts stderr ""
    puts stderr "  Survey designations:"
    puts stderr "    mpc_designation_cli.tcl PLS2040    -> 2040 P-L"
    puts stderr "    mpc_designation_cli.tcl '2040 P-L' -> PLS2040"
    puts stderr ""
    puts stderr "COMET Examples:"
    puts stderr "  Numbered periodic comets:"
    puts stderr "    mpc_designation_cli.tcl 0001P      -> 1P"
    puts stderr "    mpc_designation_cli.tcl 1P         -> 0001P"
    puts stderr "    mpc_designation_cli.tcl 0354P      -> 354P"
    puts stderr ""
    puts stderr "  Provisional comets (type/year letter number):"
    puts stderr "    mpc_designation_cli.tcl 'C/1995 O1'    -> CJ95O010"
    puts stderr "    mpc_designation_cli.tcl 'P/2019 A4'    -> PK19A040"
    puts stderr "    mpc_designation_cli.tcl 'D/1993 F2-B'  -> DJ93F02b (fragment B)"
    puts stderr ""
    puts stderr "  Packed comet provisional (7-8 char):"
    puts stderr "    mpc_designation_cli.tcl J95O010    -> 1995 O1"
    puts stderr "    mpc_designation_cli.tcl J94P01b    -> 1994 P1-B"
    puts stderr ""
    puts stderr "  Full packed comet (12-char with number):"
    puts stderr "    mpc_designation_cli.tcl '0001PJ82U010' -> 1P/1982 U1"
    puts stderr ""
    puts stderr "Comet type prefixes: P=periodic, C=non-periodic, D=defunct,"
    puts stderr "                     X=uncertain, A=asteroid-like, I=interstellar"
    puts stderr ""
    puts stderr "Multiple designations:"
    puts stderr "    mpc_designation_cli.tcl 03202 J95X00A 'C/1995 O1' 1P"
    puts stderr ""
    puts stderr "Verbose output:"
    puts stderr "    mpc_designation_cli.tcl -v 'C/1995 O1'"
}

#
# Format verbose output for a conversion result
#
proc formatVerbose {convResult} {
    set input [dict get $convResult input]
    set output [dict get $convResult output]
    set info [dict get $convResult info]

    set format [dict get $info format]
    set type [dict get $info type]
    set subtype [dict get $info subtype]

    set lines {}
    lappend lines "  Input:    $input"
    lappend lines "  Detected: $format format, $subtype"
    if {$format eq "packed"} {
        lappend lines "  Action:   unpacking to human-readable form"
    } else {
        lappend lines "  Action:   packing to MPC compact form"
    }
    lappend lines "  Output:   $output"

    return [join $lines "\n"]
}

#
# Main program
#
proc main {args} {
    set verbose 0
    set designations {}

    # Parse arguments
    foreach arg $args {
        switch -glob -- $arg {
            -v - --verbose {
                set verbose 1
            }
            -h - --help {
                printUsage
                exit 0
            }
            --version {
                puts "mpc_designation $MPCDesignation::version"
                exit 0
            }
            -* {
                puts stderr "Error: Unknown option: $arg"
                printUsage
                exit 1
            }
            default {
                lappend designations $arg
            }
        }
    }

    if {[llength $designations] == 0} {
        printUsage
        exit 1
    }

    set hasError 0
    set multipleInputs [expr {[llength $designations] > 1}]

    foreach designation $designations {
        if {[catch {set convResult [MPCDesignation::convert $designation]} err]} {
            if {$multipleInputs} {
                puts stderr "Error ($designation): $err"
            } else {
                puts stderr "Error: $err"
            }
            set hasError 1
            continue
        }

        set output [dict get $convResult output]

        if {$verbose} {
            if {$multipleInputs} {
                puts "Conversion:"
            }
            puts [formatVerbose $convResult]
            if {$multipleInputs} {
                puts ""
            }
        } elseif {$multipleInputs} {
            puts "$designation -> $output"
        } else {
            puts $output
        }
    }

    if {$hasError} {
        exit 2
    }
    exit 0
}

# Run main
main {*}$argv
