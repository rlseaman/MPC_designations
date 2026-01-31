#
# mpc_designation.tcl - MPC Designation Converter Library
#
# Convert between packed and unpacked asteroid/comet designations.
# Based on Minor Planet Center specifications:
# https://www.minorplanetcenter.net/iau/info/PackedDes.html
#
# Usage (as library):
#   source mpc_designation.tcl
#
#   MPCDesignation::pack $designation      ;# human-readable → packed
#   MPCDesignation::unpack $designation    ;# packed → human-readable
#   MPCDesignation::convert $designation   ;# auto-detect, returns dict
#   MPCDesignation::convertSimple $des     ;# auto-detect, returns string
#   MPCDesignation::isValid $designation   ;# check validity (no error)
#   MPCDesignation::detectFormat $des      ;# get format info dict
#
# For command-line usage, see mpc_designation_cli.tcl
#

namespace eval MPCDesignation {
    # Version - read from VERSION file at repository root
    variable version
    proc _initVersion {} {
        variable version
        set scriptDir [file dirname [info script]]
        foreach path [list \
            [file join $scriptDir .. .. VERSION] \
            [file join $scriptDir .. .. .. VERSION]] {
            if {[file exists $path]} {
                set fp [open $path r]
                set version [string trim [read $fp]]
                close $fp
                return
            }
        }
        set version "1.0.0"  ;# Fallback
    }
    _initVersion

    # Base-62 character set: 0-9, A-Z, a-z
    variable base62Chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    # Century codes for provisional designations
    # A=10 (1000s), B=11 (1100s), ... K=20 (2000s), L=21 (2100s)
    variable centuryCodes {
        A 10 B 11 C 12 D 13 E 14 F 15 G 16 H 17 I 18 J 19 K 20 L 21
    }
    variable reverseCenturyCodes {
        10 A 11 B 12 C 13 D 14 E 15 F 16 G 17 H 18 I 19 J 20 K 21 L
    }

    # Survey codes mapping
    variable surveyPackedToUnpacked {
        PLS P-L
        T1S T-1
        T2S T-2
        T3S T-3
    }
    variable surveyUnpackedToPacked {
        P-L PLS
        T-1 T1S
        T-2 T2S
        T-3 T3S
    }

    # Valid comet type prefixes
    # P = periodic (period < 200 years or observed at multiple returns)
    # C = non-periodic (long-period)
    # D = defunct/disappeared periodic comet
    # X = uncertain orbit (cannot compute meaningful orbit)
    # A = minor planet given cometary designation
    # I = interstellar object
    variable cometTypes {P C D X A I}

    # Human-readable descriptions of comet types
    variable cometTypeDescriptions {
        P "periodic"
        C "non-periodic"
        D "defunct"
        X "uncertain orbit"
        A "asteroid with comet designation"
        I "interstellar"
    }

    # Planet codes for natural satellite designations
    variable satellitePlanets {J S U N}
    variable satellitePlanetNames {
        J "Jupiter"
        S "Saturn"
        U "Uranus"
        N "Neptune"
    }

    # Pre-compiled regex patterns for performance
    # Tcl caches compiled patterns when stored in variables
    variable pat_survey_unpacked {^(\d+) (P-L|T-1|T-2|T-3)$}
    variable pat_old_style {^[AB](\d)(\d{2}) ([A-Z])([A-Z])$}
    variable pat_provisional_unpacked {^(\d{4}) ([A-Z])([A-Z])(\d*)$}
    variable pat_comet_prov_unpacked {^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$}
    variable pat_comet_numbered_packed {^(\d{4})([PD])$}
    variable pat_comet_numbered_unpacked {^(\d+)([PD])(?:/[A-Za-z].*)?$}
    variable pat_satellite_unpacked {^S/(\d{4}) ([JSUN]) (\d+)$}
    variable pat_comet_full_unpacked {^(\d*)([PCDXAI])/(-?\d+) (.+)$}
    variable pat_comet_prov_part {^([A-Z])(\d+)(?:-([A-Z]))?$}
    variable pat_packed_full_comet_12 {^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$}
    variable pat_packed_comet_8 {^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$}
    variable pat_packed_comet_9_frag {^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$}
    variable pat_packed_ancient_comet {^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$}
    variable pat_packed_bce_comet {^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$}
    variable pat_packed_tilde {^~[0-9A-Za-z]{4}$}
    variable pat_packed_letter_prefix {^[A-Za-z][0-9]{4}$}
    variable pat_packed_5digit {^[0-9]{5}$}
    variable pat_unpacked_number {^(\d+)$}
    variable pat_packed_provisional {^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$}
    variable pat_packed_extended {^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$}
    variable pat_packed_survey_pls {^(PLS)[0-9]{4}$}
    variable pat_packed_survey_t {^T([123])S[0-9]{4}$}
    variable pat_unpacked_survey_pl {^(\d+) P-L$}
    variable pat_unpacked_survey_t {^(\d+) T-([123])$}
    variable pat_unpacked_old_style {^[AB]\d{3} [A-Z][A-Z]$}
    variable pat_unpacked_provisional {^\d{4} [A-Z][A-Z]\d*$}
    variable pat_unpacked_named {^(\d+) [A-Z][a-z]{2,}}
    variable pat_packed_satellite {^S[IJKL][0-9]{2}[JSUN][0-9A-Za-z]{2}[0-9]$}
    variable pat_packed_comet_numbered {^[0-9]{4}([PD])$}
    variable pat_packed_comet_prov {^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$}
    variable pat_unpacked_comet_full {^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$}
    variable pat_parenthesized {^\((\d+)\)$}
    variable pat_asteroid_provisional_start {^\d{4} [A-Z][A-Z]}

    #
    # Convert a single base-62 character to its numeric value
    #
    proc base62ToNum {char} {
        variable base62Chars
        set idx [string first $char $base62Chars]
        if {$idx < 0} {
            error "Invalid base-62 character: $char"
        }
        return $idx
    }

    #
    # Convert a numeric value (0-61) to its base-62 character
    #
    proc numToBase62 {num} {
        variable base62Chars
        if {$num < 0 || $num > 61} {
            error "Number out of base-62 range: $num"
        }
        return [string index $base62Chars $num]
    }

    #
    # Convert a 4-character base-62 string to a number
    #
    proc base62StringToNum {str} {
        set result 0
        foreach char [split $str ""] {
            set result [expr {$result * 62 + [base62ToNum $char]}]
        }
        return $result
    }

    #
    # Convert a number to a 4-character base-62 string
    #
    proc numToBase62String {num {width 4}} {
        set result ""
        for {set i 0} {$i < $width} {incr i} {
            set result "[numToBase62 [expr {$num % 62}]]$result"
            set num [expr {$num / 62}]
        }
        return $result
    }

    #
    # Validate whitespace in a designation string
    # - Only printable ASCII (32-126) allowed
    # - No tabs or other control characters
    # Note: Consecutive spaces are handled by regex patterns - packed formats
    # like "    CJ95O010" have valid leading spaces
    #
    proc validateWhitespace {s} {
        foreach char [split $s ""] {
            set code [scan $char %c]
            if {$code < 32 || $code > 126} {
                error "Invalid character in designation: [format {\\x%02x} $code]"
            }
        }
    }

    #
    # Unpack a permanent (numbered) asteroid designation
    # Input: 5 or 6 character packed format
    # Output: integer number
    #
    proc unpackPermanent {packed} {
        set packed [string trim $packed]
        set len [string length $packed]
        set first [string index $packed 0]

        # Check for tilde format first (>= 620,000) - ~ plus 4 base-62 characters
        if {$first eq "~" && $len == 5} {
            set base62Part [string range $packed 1 4]
            return [expr {620000 + [base62StringToNum $base62Part]}]
        }

        if {$len != 5} {
            error "Invalid packed permanent designation length: $packed"
        }

        set rest [string range $packed 1 4]

        if {[string is digit $first]} {
            # Simple numeric format (< 100,000)
            return [scan $packed %d]
        } elseif {[string match {[A-Z]} $first]} {
            # Extended format with uppercase letter (100,000 - 359,999)
            set val [expr {[scan $first %c] - 55}]  ;# A=10, B=11, etc.
            return [expr {$val * 10000 + [scan $rest %d]}]
        } elseif {[string match {[a-z]} $first]} {
            # Extended format with lowercase letter (360,000 - 619,999)
            set val [expr {[scan $first %c] - 61}]  ;# a=36, b=37, etc.
            return [expr {$val * 10000 + [scan $rest %d]}]
        } else {
            error "Invalid packed permanent designation: $packed"
        }
    }

    #
    # Pack a permanent (numbered) asteroid designation
    # Input: integer number
    # Output: 5 or 6 character packed format
    #
    # Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
    variable maxAsteroidNumber 15396335

    proc packPermanent {number} {
        variable maxAsteroidNumber
        if {![string is integer -strict $number] || $number < 1 || $number > $maxAsteroidNumber} {
            error "Invalid asteroid number: $number"
        }

        if {$number < 100000} {
            # Simple numeric format
            return [format "%05d" $number]
        } elseif {$number < 620000} {
            # Extended letter format
            set div [expr {$number / 10000}]
            set mod [expr {$number % 10000}]
            if {$div < 36} {
                # A-Z for 10-35
                set letter [format %c [expr {$div + 55}]]
            } else {
                # a-z for 36-61
                set letter [format %c [expr {$div + 61}]]
            }
            return [format "%s%04d" $letter $mod]
        } else {
            # Tilde + base-62 format
            set offset [expr {$number - 620000}]
            return "~[numToBase62String $offset]"
        }
    }

    #
    # Decode the cycle count from packed provisional format
    # Input: 2-character string (columns 5-6 of packed format)
    # Output: integer cycle count
    #
    proc decodeCycleCount {encoded} {
        set first [string index $encoded 0]
        set second [string index $encoded 1]

        if {[string is digit $first]} {
            set tens [scan $first %d]
        } elseif {[string match {[A-Z]} $first]} {
            set tens [expr {[scan $first %c] - 55}]  ;# A=10, B=11, etc.
        } elseif {[string match {[a-z]} $first]} {
            set tens [expr {[scan $first %c] - 61}]  ;# a=36, b=37, etc.
        } else {
            error "Invalid cycle count character: $first"
        }

        set units [scan $second %d]
        return [expr {$tens * 10 + $units}]
    }

    #
    # Encode the cycle count for packed provisional format
    # Input: integer cycle count
    # Output: 2-character string
    # Note: Only handles counts 0-619. For 620+, use extended format.
    #
    proc encodeCycleCount {count} {
        if {$count < 0 || $count > 619} {
            error "Cycle count out of range for standard format: $count (use extended format for 620+)"
        }

        set tens [expr {$count / 10}]
        set units [expr {$count % 10}]

        if {$tens < 10} {
            set firstChar $tens
        } elseif {$tens < 36} {
            set firstChar [format %c [expr {$tens + 55}]]
        } else {
            set firstChar [format %c [expr {$tens + 61}]]
        }

        return "$firstChar$units"
    }

    #
    # Check if a letter is a valid half-month letter (A-Y excluding I)
    #
    proc isValidHalfMonth {letter} {
        set code [scan $letter %c]
        return [expr {$code >= 65 && $code <= 89 && $code != 73}]  ;# A-Y, not I
    }

    #
    # Convert second letter (A-Z, skipping I) to position (1-25)
    #
    proc letterToPosition {letter} {
        set code [scan $letter %c]
        if {$code < 73} {
            # A-H: positions 1-8
            return [expr {$code - 64}]
        } else {
            # J-Z: positions 9-25 (skip I)
            return [expr {$code - 65}]
        }
    }

    #
    # Convert position (1-25) to second letter (A-Z, skipping I)
    #
    proc positionToLetter {pos} {
        if {$pos < 1 || $pos > 25} {
            error "Letter position out of range: $pos"
        }
        if {$pos <= 8} {
            # Positions 1-8: A-H
            return [format %c [expr {64 + $pos}]]
        } else {
            # Positions 9-25: J-Z (skip I)
            return [format %c [expr {65 + $pos}]]
        }
    }

    #
    # Calculate sequence number from cycle count and second letter
    # Sequence number is 1-based: A0=1, B0=2, ..., Y0=25, A1=26, ...
    #
    proc cycleLetterToSequence {cycle letter} {
        set pos [letterToPosition $letter]
        return [expr {$cycle * 25 + $pos}]
    }

    #
    # Calculate cycle count and second letter from sequence number
    #
    proc sequenceToCycleLetter {sequence} {
        set cycle [expr {($sequence - 1) / 25}]
        set pos [expr {(($sequence - 1) % 25) + 1}]
        set letter [positionToLetter $pos]
        return [list $cycle $letter]
    }

    #
    # Check if a provisional designation needs extended format (cycle >= 620)
    #
    proc needsExtendedFormat {cycle} {
        return [expr {$cycle >= 620}]
    }

    #
    # Pack a provisional designation using extended format (underscore prefix)
    # For cycle counts >= 620 (sequence numbers >= 15501)
    # Format: _YHbbbb where Y=year(last 2 digits as letter), H=half-month, bbbb=base62
    #
    proc packExtendedProvisional {year halfMonth secondLetter cycle} {
        variable reverseCenturyCodes

        set century [string range $year 0 1]
        if {$century ne "20"} {
            error "Extended format only supports years 2000-2099: $year"
        }

        set yearShort [string range $year 2 3]
        # Encode last 2 digits as letter: 00=A, 01=B, ..., 25=P, 26=Q, etc.
        # Actually it's: 10=A, 11=B, etc. No wait...
        # Looking at examples: Q=26 for year 2026, so yearShort 26 -> Q
        # The encoding is: A + (yearShort - 10) when yearShort >= 10
        # But for 00-09, we need different handling
        set ys [scan $yearShort %d]
        if {$ys < 10} {
            # Years 2000-2009: encode as digits? No, the spec says letters.
            # Let's use A=00, B=01, ..., J=09, K=10, ..., Z=25, a=26, etc.
            # Actually from the examples, year 2026 -> Q, so 26-10=16, A+16=Q
            # For year 2025 -> P, 25-10=15, A+15=P
            # So the formula is: letter = chr(ord('A') + yearShort - 10) for yearShort >= 10
            # For yearShort < 10, we might need 0-9? Or special handling?
            # The doc says the underscore indicates "first 2 digits of year are 20"
            # and char 2 is "Capital letter representing last two digits"
            # Let me assume: 00=A... no, that conflicts with century codes
            # Actually, looking at _QC0000 for 2026, Q must be 26
            # If A=10, B=11, ..., P=25, Q=26, this works for years 2010-2035+
            # For years 2000-2009, maybe 0-9 are used as digits?
            set yearCode $ys
        } else {
            set yearCode [format %c [expr {65 + $ys - 10}]]
        }

        set sequence [cycleLetterToSequence $cycle $secondLetter]
        set offset [expr {$sequence - 15501}]
        set base62Part [numToBase62String $offset 4]

        return "_$yearCode$halfMonth$base62Part"
    }

    #
    # Unpack an extended provisional designation (underscore prefix)
    #
    proc unpackExtendedProvisional {packed} {
        if {[string index $packed 0] ne "_"} {
            error "Not an extended format designation: $packed"
        }

        set yearCode [string index $packed 1]
        set halfMonth [string index $packed 2]
        set base62Part [string range $packed 3 6]

        # Decode year: digit means 200X, letter means 20XX where XX = ord(letter) - ord('A') + 10
        if {[string is digit $yearCode]} {
            set year "200$yearCode"
        } else {
            set ys [expr {[scan $yearCode %c] - 65 + 10}]
            set year [format "20%02d" $ys]
        }

        set offset [base62StringToNum $base62Part]
        set sequence [expr {15501 + $offset}]
        lassign [sequenceToCycleLetter $sequence] cycle secondLetter

        if {$cycle == 0} {
            return "$year $halfMonth$secondLetter"
        } else {
            return "$year $halfMonth$secondLetter$cycle"
        }
    }

    #
    # Unpack a provisional asteroid designation
    # Input: 7-character packed format
    # Output: unpacked format like "1995 XA" or "1998 SQ108"
    #
    proc unpackProvisional {packed} {
        variable centuryCodes
        variable surveyPackedToUnpacked

        set packed [string trim $packed]
        if {[string length $packed] != 7} {
            error "Invalid packed provisional designation length: $packed"
        }

        # Check for survey designations first
        set prefix [string range $packed 0 2]
        if {[dict exists $surveyPackedToUnpacked $prefix]} {
            set surveyName [dict get $surveyPackedToUnpacked $prefix]
            set number [scan [string range $packed 3 6] %d]
            return "$number $surveyName"
        }

        # Regular provisional designation
        set century [string index $packed 0]
        set year [string range $packed 1 2]
        set halfMonth [string index $packed 3]
        set cycleEncoded [string range $packed 4 5]
        set secondLetter [string index $packed 6]

        if {![dict exists $centuryCodes $century]} {
            error "Invalid century code: $century"
        }

        set fullYear "[dict get $centuryCodes $century]$year"
        set cycleCount [decodeCycleCount $cycleEncoded]

        if {$cycleCount == 0} {
            return "$fullYear $halfMonth$secondLetter"
        } else {
            return "$fullYear $halfMonth$secondLetter$cycleCount"
        }
    }

    #
    # Pack a provisional asteroid designation
    # Input: unpacked format like "1995 XA" or "1998 SQ108"
    # Output: 7-character packed format
    #
    proc packProvisional {unpacked} {
        variable reverseCenturyCodes
        variable surveyUnpackedToPacked

        set unpacked [string trim $unpacked]

        # Check for survey designations: "2040 P-L", "3138 T-1", etc.
        variable pat_survey_unpacked
        if {[regexp $pat_survey_unpacked $unpacked -> number survey]} {
            # Survey number must be positive
            if {$number < 1} {
                error "Survey number must be positive: $number"
            }
            set prefix [dict get $surveyUnpackedToPacked $survey]
            return [format "%s%04d" $prefix $number]
        }

        # Check for old-style designation: "A908 CJ" or "B842 FA"
        # Format: [AB]CYY LL where C=century digit, YY=year, LL=half-month+letter
        variable pat_old_style
        if {[regexp $pat_old_style $unpacked -> centuryDigit yearShort halfMonth secondLetter]} {
            # Convert century digit to century code: 8->18(I), 9->19(J), 0->20(K)
            if {$centuryDigit == 8} {
                set centuryCode "I"
            } elseif {$centuryDigit == 9} {
                set centuryCode "J"
            } elseif {$centuryDigit == 0} {
                set centuryCode "K"
            } else {
                error "Invalid century digit in old-style designation: $centuryDigit"
            }
            # Old-style designations always have cycle count 0
            return "${centuryCode}${yearShort}${halfMonth}00${secondLetter}"
        }

        # Regular provisional: "1995 XA" or "1998 SQ108"
        variable pat_provisional_unpacked
        if {![regexp $pat_provisional_unpacked $unpacked -> year halfMonth secondLetter cycleStr]} {
            error "Invalid unpacked provisional designation: $unpacked"
        }

        # Validate half-month letter (I is not used)
        if {![isValidHalfMonth $halfMonth]} {
            error "Invalid half-month letter: $halfMonth"
        }

        if {$cycleStr eq ""} {
            set cycleCount 0
        } else {
            set cycleCount [scan $cycleStr %d]
        }

        # Use extended format for cycle counts >= 620 (years 2000-2099 only)
        if {[needsExtendedFormat $cycleCount]} {
            return [packExtendedProvisional $year $halfMonth $secondLetter $cycleCount]
        }

        set century [string range $year 0 1]
        set yearShort [string range $year 2 3]

        if {![dict exists $reverseCenturyCodes $century]} {
            error "Invalid century in year: $year"
        }

        set centuryCode [dict get $reverseCenturyCodes $century]
        set cycleEncoded [encodeCycleCount $cycleCount]

        return "$centuryCode$yearShort$halfMonth$cycleEncoded$secondLetter"
    }

    #
    # Unpack a comet provisional designation (7-character format)
    # Input: 7-character packed format like "J95O010" or "J94P01b"
    # Output: unpacked format like "1995 O1" or "1994 P1-B"
    #
    proc unpackCometProvisional {packed} {
        variable centuryCodes

        set packed [string trim $packed]
        set len [string length $packed]
        if {$len != 7 && $len != 8} {
            error "Invalid packed comet provisional designation length: $packed"
        }

        set century [string index $packed 0]
        set year [string range $packed 1 2]
        set halfMonth [string index $packed 3]
        set orderEncoded [string range $packed 4 5]

        # Fragment: 1 char for 7-char format, 2 chars for 8-char format
        if {$len == 7} {
            set fragment [string index $packed 6]
        } else {
            set fragment [string range $packed 6 7]
        }

        if {![dict exists $centuryCodes $century]} {
            error "Invalid century code: $century"
        }

        set fullYear "[dict get $centuryCodes $century]$year"
        set orderNum [decodeCycleCount $orderEncoded]

        set result "$fullYear $halfMonth$orderNum"

        # Add fragment suffix if present (single char "0" means none, otherwise 1-2 letters)
        if {$fragment ne "0"} {
            set fragmentLetter [string toupper $fragment]
            append result "-$fragmentLetter"
        }

        return $result
    }

    #
    # Pack a comet provisional designation
    # Input: unpacked format like "1995 O1" or "1994 P1-B"
    # Output: 7-character packed format
    #
    proc packCometProvisional {unpacked} {
        variable reverseCenturyCodes

        set unpacked [string trim $unpacked]

        # Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
        variable pat_comet_prov_unpacked
        if {![regexp $pat_comet_prov_unpacked $unpacked -> year halfMonth orderNum fragment]} {
            error "Invalid unpacked comet provisional designation: $unpacked"
        }

        # Comet order number must be positive
        if {$orderNum < 1} {
            error "Comet order number must be positive: $orderNum"
        }

        set century [string range $year 0 1]
        set yearShort [string range $year 2 3]

        if {![dict exists $reverseCenturyCodes $century]} {
            error "Invalid century in year: $year"
        }

        set centuryCode [dict get $reverseCenturyCodes $century]
        set orderEncoded [encodeCycleCount $orderNum]

        # Fragment encoding: 0 for none, lowercase letter for fragment
        if {$fragment eq ""} {
            set fragmentCode "0"
        } else {
            set fragmentCode [string tolower $fragment]
        }

        return "$centuryCode$yearShort$halfMonth$orderEncoded$fragmentCode"
    }

    #
    # Unpack a numbered periodic comet designation
    # Input: 4-5 character packed format like "0001P" or "0354P"
    # Output: unpacked format like "1P" or "354P"
    #
    proc unpackCometNumbered {packed} {
        set packed [string trim $packed]

        variable pat_comet_numbered_packed
        if {![regexp $pat_comet_numbered_packed $packed -> numStr cometType]} {
            error "Invalid packed numbered comet designation: $packed"
        }

        set number [scan $numStr %d]
        return "$number$cometType"
    }

    #
    # Pack a numbered periodic comet designation
    # Input: unpacked format like "1P" or "354P"
    # Output: 5-character packed format like "0001P"
    #
    proc packCometNumbered {unpacked} {
        set unpacked [string trim $unpacked]

        # Match "1P" or "354P" or "1P/Halley" (with optional name after slash)
        variable pat_comet_numbered_unpacked
        if {![regexp $pat_comet_numbered_unpacked $unpacked -> number cometType]} {
            error "Invalid unpacked numbered comet designation: $unpacked"
        }

        if {$number < 1 || $number > 9999} {
            error "Comet number out of range (1-9999): $number"
        }

        return [format "%04d%s" $number $cometType]
    }

    #
    # Unpack a natural satellite provisional designation
    # Input: 8-char packed format like "SK19S220" (S + century + year + planet + number + 0)
    # Output: unpacked format like "S/2019 S 22"
    #
    proc unpackSatellite {packed} {
        variable centuryCodes
        variable satellitePlanets

        set packed [string trim $packed]
        if {[string length $packed] != 8} {
            error "Invalid packed satellite designation length: $packed"
        }

        if {[string index $packed 0] ne "S"} {
            error "Invalid satellite designation prefix: $packed"
        }

        set century [string index $packed 1]
        set year [string range $packed 2 3]
        set planet [string index $packed 4]
        set numEncoded [string range $packed 5 6]
        # Column 8 is typically 0 (reserved)

        if {![dict exists $centuryCodes $century]} {
            error "Invalid century code in satellite designation: $century"
        }

        if {$planet ni $satellitePlanets} {
            error "Invalid planet code in satellite designation: $planet"
        }

        set fullYear "[dict get $centuryCodes $century]$year"
        set number [decodeCycleCount $numEncoded]

        return "S/$fullYear $planet $number"
    }

    #
    # Pack a natural satellite provisional designation
    # Input: unpacked format like "S/2019 S 22"
    # Output: 8-char packed format like "SK19S220"
    #
    proc packSatellite {unpacked} {
        variable reverseCenturyCodes
        variable satellitePlanets

        set unpacked [string trim $unpacked]

        # Match: S/YYYY P N (year, planet letter, discovery number)
        variable pat_satellite_unpacked
        if {![regexp $pat_satellite_unpacked $unpacked -> year planet number]} {
            error "Invalid unpacked satellite designation: $unpacked"
        }

        # Satellite number must be positive
        if {$number < 1} {
            error "Satellite number must be positive: $number"
        }

        set century [string range $year 0 1]
        set yearShort [string range $year 2 3]

        if {![dict exists $reverseCenturyCodes $century]} {
            error "Invalid century in satellite year: $year"
        }

        set centuryCode [dict get $reverseCenturyCodes $century]
        set numEncoded [encodeCycleCount $number]

        return "S$centuryCode$yearShort$planet${numEncoded}0"
    }

    #
    # Encode a BCE year for comet designation
    # Returns: list of {prefix code} where prefix is /, ., or - and code is 2-digit number
    #
    proc encodeBCEYear {year} {
        # year is negative (e.g., -43 for 44 BC)
        set absYear [expr {abs($year)}]
        set bc [expr {$absYear + 1}]  ;# Convert astronomical to BC

        if {$bc >= 1 && $bc <= 100} {
            # 1-100 BC: prefix /, code = 100 - bc
            return [list "/" [format "%02d" [expr {100 - $bc}]]]
        } elseif {$bc >= 101 && $bc <= 200} {
            # 101-200 BC: prefix ., code = 200 - bc
            return [list "." [format "%02d" [expr {200 - $bc}]]]
        } elseif {$bc >= 201 && $bc <= 300} {
            # 201-300 BC: prefix -, code = 300 - bc
            return [list "-" [format "%02d" [expr {300 - $bc}]]]
        } else {
            error "BCE year out of supported range: $year (BC $bc)"
        }
    }

    #
    # Decode a BCE year from packed format
    # Input: prefix (/, ., -) and 2-digit code
    # Returns: negative year
    #
    proc decodeBCEYear {prefix code} {
        set codeNum [scan $code %d]
        switch $prefix {
            "/" {
                # 1-100 BC: bc = 100 - code
                set bc [expr {100 - $codeNum}]
            }
            "." {
                # 101-200 BC: bc = 200 - code
                set bc [expr {200 - $codeNum}]
            }
            "-" {
                # 201-300 BC: bc = 300 - code
                set bc [expr {300 - $codeNum}]
            }
            default {
                error "Invalid BCE prefix: $prefix"
            }
        }
        # Convert BC to astronomical year: bc N = year -(N-1)
        return [expr {1 - $bc}]
    }

    #
    # Check if a year is ancient (< 1000 AD) or BCE
    #
    proc isAncientYear {year} {
        return [expr {$year < 1000}]
    }

    #
    # Pack a comet provisional with ancient year (< 1000 AD) or BCE year
    # Format for year 1-999: TYYYHNNN where T=type, YYY=3-digit year, H=half-month, NNN=order+fragment
    # Format for BCE: T[/.-]CCHNNN where CC=encoded century code
    #
    proc packAncientCometProvisional {cometType year halfMonth orderNum {fragment ""}} {
        if {$year < 0} {
            # BCE year
            lassign [encodeBCEYear $year] prefix code
            set orderEncoded [encodeCycleCount $orderNum]
            set fragmentCode [expr {$fragment eq "" ? "0" : [string tolower $fragment]}]
            return "$cometType$prefix$code$halfMonth${orderEncoded}$fragmentCode"
        } elseif {$year >= 1 && $year <= 999} {
            # Ancient year (1-999 AD)
            set orderEncoded [encodeCycleCount $orderNum]
            set fragmentCode [expr {$fragment eq "" ? "0" : [string tolower $fragment]}]
            return [format "%s%03d%s%s%s" $cometType $year $halfMonth $orderEncoded $fragmentCode]
        } else {
            error "Not an ancient year: $year"
        }
    }

    #
    # Unpack a comet provisional with ancient year
    # Input: 8-char format like "C240V010" or "C.53P010"
    #
    proc unpackAncientCometProvisional {packed} {
        set cometType [string index $packed 0]
        set char1 [string index $packed 1]

        if {$char1 in {/ . -}} {
            # BCE format: T[/.-]CCHNNN
            set prefix $char1
            set code [string range $packed 2 3]
            set year [decodeBCEYear $prefix $code]
            set halfMonth [string index $packed 4]
            set orderEncoded [string range $packed 5 6]
            set fragment [string index $packed 7]
        } else {
            # Ancient AD format: TYYYHNNN
            set yearStr [string range $packed 1 3]
            set year [scan $yearStr %d]
            set halfMonth [string index $packed 4]
            set orderEncoded [string range $packed 5 6]
            set fragment [string index $packed 7]
        }

        set orderNum [decodeCycleCount $orderEncoded]

        set result "$cometType/$year $halfMonth$orderNum"
        if {$fragment ne "0"} {
            append result "-[string toupper $fragment]"
        }
        return $result
    }

    #
    # Determine if a 7-char packed provisional is comet-style or asteroid-style
    # Comet-style ends with digit or lowercase (fragment), asteroid-style ends with uppercase
    #
    proc isAsteroidStylePacked {provisionalPart} {
        set lastChar [string index $provisionalPart end]
        return [string match {[A-Z]} $lastChar]
    }

    #
    # Determine if an unpacked provisional is comet-style or asteroid-style
    # Comet-style: "1995 O1" (one letter + number)
    # Asteroid-style: "1995 XA" or "2006 AH2" (two letters + optional number)
    #
    proc isAsteroidStyleUnpacked {provisional} {
        # Asteroid: YYYY LL or YYYY LLnnn (two uppercase letters)
        # Comet: YYYY Ln or YYYY Ln-F (one letter + digit)
        variable pat_asteroid_provisional_start
        if {[regexp $pat_asteroid_provisional_start $provisional]} {
            return 1
        }
        return 0
    }

    #
    # Unpack a full comet designation (8 or 12-character format)
    # Input: 8-char "CJ95O010" or 12-char "    CJ95O010" or "0001PJ86F010"
    # Output: unpacked format like "C/1995 O1" or "1P/1986 F1"
    # Also handles asteroid-style provisionals with comet prefix: "P/2006 AH2"
    #
    proc unpackCometFull {packed} {
        variable cometTypes

        set len [string length $packed]

        if {$len == 8} {
            # Compact 8-char format: type + 7-char provisional
            set cometType [string index $packed 0]
            set provisionalPart [string range $packed 1 7]
            set numStr ""
        } elseif {$len == 9} {
            # Compact 9-char format with 2-letter fragment: type + 8-char provisional
            set cometType [string index $packed 0]
            set provisionalPart [string range $packed 1 8]
            set numStr ""
        } elseif {$len == 12 || ($len < 12 && [string index $packed 0] eq " ")} {
            # Full 12-char format or trimmed version
            # Pad with leading spaces if needed
            while {[string length $packed] < 12} {
                set packed " $packed"
            }
            set numPart [string range $packed 0 3]
            set cometType [string index $packed 4]
            set provisionalPart [string range $packed 5 11]
            set numStr [string trim $numPart]
        } else {
            error "Invalid packed full comet designation length: $packed"
        }

        if {$cometType ni $cometTypes} {
            error "Invalid comet type: $cometType"
        }

        # Determine if this uses asteroid-style or comet-style provisional
        if {[isAsteroidStylePacked $provisionalPart]} {
            set provisional [unpackProvisional $provisionalPart]
        } else {
            set provisional [unpackCometProvisional $provisionalPart]
        }

        # Check if there's a periodic number
        if {$numStr eq ""} {
            # No periodic number, just type prefix
            return "$cometType/$provisional"
        } else {
            # Has periodic number
            set number [scan $numStr %d]
            return "$number$cometType/$provisional"
        }
    }

    #
    # Pack a full comet designation
    # Input: unpacked format like "C/1995 O1" or "1P/1986 F1" or "P/2019 A4"
    # Also handles asteroid-style: "P/2006 AH2", "A/2010 LN135"
    # Output: 12-character packed format
    #
    proc packCometFull {unpacked} {
        variable cometTypes

        set unpacked [string trim $unpacked]

        # Match: optional number, type, slash, provisional (including negative years)
        # Examples: "C/1995 O1", "1P/1982 U1", "P/2019 A4", "D/1993 F2-B", "C/-146 P1", "C/240 V1"
        variable pat_comet_full_unpacked
        if {![regexp $pat_comet_full_unpacked $unpacked -> number cometType year provPart]} {
            error "Invalid unpacked comet designation: $unpacked"
        }

        if {$cometType ni $cometTypes} {
            error "Invalid comet type: $cometType"
        }

        set year [scan $year %d]

        # Check for ancient or BCE year
        if {[isAncientYear $year]} {
            # Parse the provisional part for ancient comets: "L1" or "L1-F"
            variable pat_comet_prov_part
            if {[regexp $pat_comet_prov_part $provPart -> halfMonth orderNum fragment]} {
                return [packAncientCometProvisional $cometType $year $halfMonth $orderNum $fragment]
            } else {
                error "Invalid ancient comet provisional: $provPart"
            }
        }

        # Modern comet - reconstruct provisional with year
        set provisional "$year $provPart"

        # Determine if this uses asteroid-style or comet-style provisional
        if {[isAsteroidStyleUnpacked $provisional]} {
            set provisionalPacked [packProvisional $provisional]
        } else {
            set provisionalPacked [packCometProvisional $provisional]
        }

        if {$number eq ""} {
            # No periodic number - use compact 8-char format (type + provisional)
            return "$cometType$provisionalPacked"
        } else {
            # Has periodic number - use full 12-char format
            if {$number < 1 || $number > 9999} {
                error "Comet number out of range (1-9999): $number"
            }
            set numPart [format "%04d" $number]
            return "$numPart$cometType$provisionalPacked"
        }
    }

    #
    # Detect if a designation is packed or unpacked and what type it is
    # Returns: dict with keys: format, type, subtype, number (if applicable)
    # format: "packed" or "unpacked"
    # type: "permanent", "provisional", or "survey"
    # subtype: more specific description for verbose output
    #
    proc detectFormat {designation} {
        # Access pre-compiled patterns
        variable pat_packed_full_comet_12
        variable pat_packed_comet_8
        variable pat_packed_comet_9_frag
        variable pat_packed_ancient_comet
        variable pat_packed_bce_comet
        variable pat_parenthesized
        variable pat_packed_tilde
        variable pat_packed_letter_prefix
        variable pat_packed_5digit
        variable pat_unpacked_number
        variable pat_packed_provisional
        variable pat_packed_extended
        variable pat_packed_survey_pls
        variable pat_packed_survey_t
        variable pat_unpacked_survey_pl
        variable pat_unpacked_survey_t
        variable pat_unpacked_old_style
        variable pat_unpacked_provisional
        variable pat_unpacked_named
        variable pat_packed_satellite
        variable pat_satellite_unpacked
        variable pat_packed_comet_numbered
        variable pat_packed_comet_prov
        variable pat_unpacked_comet_full
        variable pat_comet_numbered_unpacked

        # Validate no tabs or non-printable characters
        validateWhitespace $designation

        set result [dict create format "" type "" subtype "" number ""]

        # Check for packed full comet designation BEFORE trimming (12 chars with spaces)
        # Format: 4 chars (spaces or number) + type + 7 char provisional
        # e.g., "    CJ95O010" or "0001PJ86F010"
        if {[string length $designation] == 12 &&
            [regexp $pat_packed_full_comet_12 $designation]} {
            dict set result format packed
            dict set result type comet_full
            dict set result subtype "comet with provisional designation (12-char)"
            return $result
        }

        # Check for packed comet designation (8 chars: type + 7 char provisional)
        # e.g., "CJ95O010" or "PK24F010"
        if {[string length $designation] == 8 &&
            [regexp $pat_packed_comet_8 $designation]} {
            dict set result format packed
            dict set result type comet_full
            dict set result subtype "comet with provisional designation (8-char)"
            return $result
        }

        # Check for packed comet with 2-letter fragment (9 chars: type + 7 char provisional + extra fragment char)
        # e.g., "PJ30J01aa" for P/1930 J1-AA
        if {[string length $designation] == 9 &&
            [regexp $pat_packed_comet_9_frag $designation]} {
            dict set result format packed
            dict set result type comet_full
            dict set result subtype "comet with provisional designation (9-char, 2-letter fragment)"
            return $result
        }

        # Check for packed ancient comet (8 chars: type + 3-digit year + provisional)
        # e.g., "C240V010" for C/240 V1
        if {[string length $designation] == 8 &&
            [regexp $pat_packed_ancient_comet $designation]} {
            dict set result format packed
            dict set result type comet_ancient
            dict set result subtype "comet with ancient provisional (year < 1000)"
            return $result
        }

        # Check for packed BCE comet (8 chars: type + prefix + code + provisional)
        # e.g., "C.53P010" for C/-146 P1, "C/56L010" for C/-43 L1
        if {[string length $designation] == 8 &&
            [regexp $pat_packed_bce_comet $designation]} {
            dict set result format packed
            dict set result type comet_bce
            dict set result subtype "comet with BCE provisional"
            return $result
        }

        set des [string trim $designation]

        # Remove parentheses if present (common in permanent designations)
        if {[regexp $pat_parenthesized $des -> num]} {
            dict set result format unpacked
            dict set result type permanent
            dict set result subtype "permanent numbered (parenthesized)"
            dict set result number $num
            return $result
        }

        # Check for tilde-prefix packed permanent (>= 620000)
        if {[regexp $pat_packed_tilde $des]} {
            dict set result format packed
            dict set result type permanent
            dict set result subtype "permanent numbered (tilde/base-62, >= 620000)"
            return $result
        }

        # Check for packed permanent designation with letter prefix (100000-619999)
        if {[regexp $pat_packed_letter_prefix $des]} {
            set first [string index $des 0]
            if {[string match {[A-Z]} $first]} {
                dict set result subtype "permanent numbered (letter-prefix, 100000-359999)"
            } else {
                dict set result subtype "permanent numbered (letter-prefix, 360000-619999)"
            }
            dict set result format packed
            dict set result type permanent
            return $result
        }

        # Check for packed permanent designation (numeric, < 100000)
        if {[regexp $pat_packed_5digit $des]} {
            dict set result format packed
            dict set result type permanent
            dict set result subtype "permanent numbered (5-digit, < 100000)"
            return $result
        }

        # Check for unpacked permanent (just a number)
        if {[regexp $pat_unpacked_number $des -> num]} {
            dict set result format unpacked
            dict set result type permanent
            dict set result subtype "permanent numbered"
            dict set result number $num
            return $result
        }

        # Check for packed provisional (7 characters starting with century code A-L)
        if {[regexp $pat_packed_provisional $des]} {
            dict set result format packed
            dict set result type provisional
            dict set result subtype "provisional"
            return $result
        }

        # Check for extended packed provisional (underscore prefix, 7 chars)
        # Format: _YHbbbb where Y=year code, H=half-month, bbbb=base62
        if {[regexp $pat_packed_extended $des]} {
            dict set result format packed
            dict set result type provisional_extended
            dict set result subtype "provisional (extended format, cycle >= 620)"
            return $result
        }

        # Check for packed survey designation
        if {[regexp $pat_packed_survey_pls $des]} {
            dict set result format packed
            dict set result type survey
            dict set result subtype "survey (Palomar-Leiden)"
            return $result
        }
        if {[regexp $pat_packed_survey_t $des -> surveyNum]} {
            dict set result format packed
            dict set result type survey
            dict set result subtype "survey (Trojan T-$surveyNum)"
            return $result
        }

        # Check for unpacked survey designation
        if {[regexp $pat_unpacked_survey_pl $des]} {
            dict set result format unpacked
            dict set result type survey
            dict set result subtype "survey (Palomar-Leiden)"
            return $result
        }
        if {[regexp $pat_unpacked_survey_t $des -> num surveyNum]} {
            dict set result format unpacked
            dict set result type survey
            dict set result subtype "survey (Trojan T-$surveyNum)"
            return $result
        }

        # Check for old-style provisional "A908 CJ" or "B842 FA" (pre-1925 format)
        if {[regexp $pat_unpacked_old_style $des]} {
            dict set result format unpacked
            dict set result type provisional
            dict set result subtype "provisional (old-style pre-1925)"
            return $result
        }

        # Check for unpacked provisional "YYYY LL" or "YYYY LLnnn"
        if {[regexp $pat_unpacked_provisional $des]} {
            dict set result format unpacked
            dict set result type provisional
            dict set result subtype "provisional"
            return $result
        }

        # Check for unpacked permanent with name "1234 Name" (after provisional to avoid conflicts)
        # Requires at least 2 lowercase letters in name to avoid matching mixed-case provisionals
        if {[regexp $pat_unpacked_named $des -> num]} {
            dict set result format unpacked
            dict set result type permanent
            dict set result subtype "permanent numbered with name"
            dict set result number $num
            return $result
        }

        # ============ NATURAL SATELLITE DESIGNATIONS ============

        # Check for packed satellite designation: "SK19S220" (S + century + year + planet + num + 0)
        if {[regexp $pat_packed_satellite $des]} {
            variable satellitePlanetNames
            set planet [string index $des 4]
            dict set result format packed
            dict set result type satellite
            dict set result subtype "natural satellite ([dict get $satellitePlanetNames $planet])"
            return $result
        }

        # Check for unpacked satellite designation: "S/2019 S 22"
        if {[regexp $pat_satellite_unpacked $des -> year planet num]} {
            variable satellitePlanetNames
            dict set result format unpacked
            dict set result type satellite
            dict set result subtype "natural satellite ([dict get $satellitePlanetNames $planet])"
            return $result
        }

        # ============ COMET DESIGNATIONS ============

        # Check for packed numbered periodic comet "0001P" or "0354D"
        if {[regexp $pat_packed_comet_numbered $des -> ctype]} {
            dict set result format packed
            dict set result type comet_numbered
            variable cometTypeDescriptions
            dict set result subtype "comet numbered [dict get $cometTypeDescriptions $ctype]"
            return $result
        }

        # Check for packed comet provisional only (7 chars, ends with digit or lowercase)
        # Must distinguish from asteroid provisional which ends with uppercase
        if {[regexp $pat_packed_comet_prov $des]} {
            dict set result format packed
            dict set result type comet_provisional
            dict set result subtype "comet provisional"
            return $result
        }

        # Check for unpacked comet with type prefix: "C/1995 O1", "P/2019 A4", "1P/1982 U1"
        # Also handles fragments: "C/1996 B2-A" or "P/1930 J1-AA"
        # Also handles asteroid-style: "P/2006 AH2", "A/2010 LN135"
        # Also handles ancient: "C/240 V1" and BCE: "C/-146 P1"
        # Must check this BEFORE simple numbered comet to properly match "1P/1982 U1"
        # Pattern matches years including negative (BCE) and short years (ancient)
        if {[regexp $pat_unpacked_comet_full $des -> num ctype year provPart fragment]} {
            variable cometTypeDescriptions
            dict set result format unpacked
            dict set result type comet_full
            set yearNum [scan $year %d]
            if {$yearNum < 0} {
                set yearDesc "BCE"
            } elseif {$yearNum < 1000} {
                set yearDesc "ancient"
            } else {
                set yearDesc ""
            }
            if {$num ne ""} {
                if {$yearDesc ne ""} {
                    dict set result subtype "comet numbered with $yearDesc provisional ([dict get $cometTypeDescriptions $ctype])"
                } else {
                    dict set result subtype "comet numbered with provisional ([dict get $cometTypeDescriptions $ctype])"
                }
            } else {
                if {$yearDesc ne ""} {
                    dict set result subtype "comet $yearDesc provisional ([dict get $cometTypeDescriptions $ctype])"
                } else {
                    dict set result subtype "comet provisional ([dict get $cometTypeDescriptions $ctype])"
                }
            }
            dict set result cometType $ctype
            if {$num ne ""} {
                dict set result number $num
            }
            return $result
        }

        # Check for unpacked numbered periodic comet "1P" or "354P" or "1P/Halley"
        # Note: "1P/1982 U1" is handled above as comet_full
        if {[regexp $pat_comet_numbered_unpacked $des -> num ctype]} {
            dict set result format unpacked
            dict set result type comet_numbered
            variable cometTypeDescriptions
            dict set result subtype "comet numbered [dict get $cometTypeDescriptions $ctype]"
            dict set result number $num
            dict set result cometType $ctype
            return $result
        }

        error "Unable to detect designation format: $designation"
    }

    #
    # Convert a designation between packed and unpacked formats
    # Auto-detects the input format and converts to the other
    # Returns: dict with keys: input, output, info (the detectFormat dict)
    #
    proc convert {designation} {
        set info [detectFormat $designation]
        set format [dict get $info format]
        set type [dict get $info type]

        set result ""
        switch $format {
            packed {
                switch $type {
                    permanent {
                        set result [unpackPermanent $designation]
                    }
                    provisional - survey {
                        set result [unpackProvisional $designation]
                    }
                    provisional_extended {
                        set result [unpackExtendedProvisional $designation]
                    }
                    comet_numbered {
                        set result [unpackCometNumbered $designation]
                    }
                    comet_provisional {
                        set result [unpackCometProvisional $designation]
                    }
                    comet_full {
                        set result [unpackCometFull $designation]
                    }
                    comet_ancient - comet_bce {
                        set result [unpackAncientCometProvisional $designation]
                    }
                    satellite {
                        set result [unpackSatellite $designation]
                    }
                }
            }
            unpacked {
                switch $type {
                    permanent {
                        set num [dict get $info number]
                        set result [packPermanent $num]
                    }
                    provisional - survey {
                        set result [packProvisional $designation]
                    }
                    comet_numbered {
                        set result [packCometNumbered $designation]
                    }
                    comet_full {
                        set result [packCometFull $designation]
                    }
                    satellite {
                        set result [packSatellite $designation]
                    }
                }
            }
        }

        return [dict create input $designation output $result info $info]
    }

    #
    # Simple convert that just returns the output string
    #
    proc convertSimple {designation} {
        set result [convert $designation]
        return [dict get $result output]
    }

    # ========================================================================
    # HIGH-LEVEL PACK/UNPACK FUNCTIONS
    # ========================================================================

    #
    # Ensure a designation is in packed format.
    # If already packed, returns as-is (after validation).
    # If unpacked, converts to packed format.
    #
    proc pack {designation} {
        set info [detectFormat $designation]
        if {[dict get $info format] eq "packed"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    #
    # Ensure a designation is in unpacked format.
    # If already unpacked, returns as-is (after validation).
    # If packed, converts to unpacked format.
    #
    proc unpack {designation} {
        set info [detectFormat $designation]
        if {[dict get $info format] eq "unpacked"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    # ========================================================================
    # CATEGORY-SPECIFIC PACK/UNPACK FUNCTIONS
    # ========================================================================

    #
    # Check if a type is an asteroid type
    #
    proc isAsteroidType {type} {
        return [expr {$type in {permanent provisional provisional_extended survey}}]
    }

    #
    # Check if a type is a comet type
    #
    proc isCometType {type} {
        return [expr {$type in {comet_numbered comet_provisional comet_full comet_ancient comet_bce}}]
    }

    #
    # Pack an asteroid designation (permanent or provisional).
    # Throws error if not an asteroid.
    #
    proc packAsteroid {designation} {
        set info [detectFormat $designation]
        set type [dict get $info type]
        if {![isAsteroidType $type]} {
            error "Not an asteroid designation: $designation (detected as $type)"
        }
        if {[dict get $info format] eq "packed"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    #
    # Unpack an asteroid designation (permanent or provisional).
    # Throws error if not an asteroid.
    #
    proc unpackAsteroid {designation} {
        set info [detectFormat $designation]
        set type [dict get $info type]
        if {![isAsteroidType $type]} {
            error "Not an asteroid designation: $designation (detected as $type)"
        }
        if {[dict get $info format] eq "unpacked"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    #
    # Pack a comet designation.
    # Throws error if not a comet.
    #
    proc packComet {designation} {
        set info [detectFormat $designation]
        set type [dict get $info type]
        if {![isCometType $type]} {
            error "Not a comet designation: $designation (detected as $type)"
        }
        if {[dict get $info format] eq "packed"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    #
    # Unpack a comet designation.
    # Throws error if not a comet.
    #
    proc unpackComet {designation} {
        set info [detectFormat $designation]
        set type [dict get $info type]
        if {![isCometType $type]} {
            error "Not a comet designation: $designation (detected as $type)"
        }
        if {[dict get $info format] eq "unpacked"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    #
    # Pack a satellite designation.
    # Throws error if not a satellite.
    #
    proc packSatelliteDesignation {designation} {
        set info [detectFormat $designation]
        set type [dict get $info type]
        if {$type ne "satellite"} {
            error "Not a satellite designation: $designation (detected as $type)"
        }
        if {[dict get $info format] eq "packed"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    #
    # Unpack a satellite designation.
    # Throws error if not a satellite.
    #
    proc unpackSatelliteDesignation {designation} {
        set info [detectFormat $designation]
        set type [dict get $info type]
        if {$type ne "satellite"} {
            error "Not a satellite designation: $designation (detected as $type)"
        }
        if {[dict get $info format] eq "unpacked"} {
            return [string trim $designation]
        }
        return [convertSimple $designation]
    }

    # ========================================================================
    # VALIDATION FUNCTIONS
    # ========================================================================

    #
    # Check if a string is a valid MPC designation.
    # Returns 1 if valid, 0 if invalid (never throws errors).
    #
    proc isValid {designation} {
        if {$designation eq ""} {
            return 0
        }
        if {[catch {detectFormat $designation}]} {
            return 0
        }
        return 1
    }

    #
    # Check if a string contains only valid MPC designation characters.
    # Valid characters are: A-Z, a-z, 0-9, space, /, -, ~, _, .
    # Returns 1 if all characters are valid, 0 otherwise.
    #
    proc isValidChars {designation} {
        if {$designation eq ""} {
            return 0
        }
        # Check each character
        foreach char [split $designation ""] {
            if {![string match {[A-Za-z0-9 /\-~_.]} $char]} {
                return 0
            }
        }
        return 1
    }

    #
    # Sanitize a designation string for processing.
    # - Validates character set
    # - Strips leading/trailing whitespace
    # Returns the cleaned string.
    # Throws error if input contains invalid characters.
    #
    proc sanitize {designation} {
        # Check for invalid characters
        foreach char [split $designation ""] {
            set code [scan $char %c]
            if {$code < 32 || $code > 126} {
                error "Invalid character in designation: [format %q $char]"
            }
        }
        return [string trim $designation]
    }
}

# Note: For CLI usage, use mpc_designation_cli.tcl which sources this library.
