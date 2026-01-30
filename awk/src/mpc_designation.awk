#!/usr/bin/awk -f
# mpc_designation.awk - MPC Designation Converter
#
# Converts between packed and unpacked Minor Planet Center designations.
# Compatible with gawk, mawk, and POSIX awk.
#
# Usage: echo "1995 XA" | awk -f mpc_designation.awk
#        awk -f mpc_designation.awk <<< "00001"

BEGIN {
    # Base-62 character set
    BASE62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    # Build lookup table for base62 values
    for (i = 1; i <= length(BASE62); i++) {
        c = substr(BASE62, i, 1)
        BASE62_VAL[c] = i - 1
    }

    # Century codes
    CENTURY_TO_NUM["I"] = 18
    CENTURY_TO_NUM["J"] = 19
    CENTURY_TO_NUM["K"] = 20
    CENTURY_TO_NUM["L"] = 21

    NUM_TO_CENTURY[18] = "I"
    NUM_TO_CENTURY[19] = "J"
    NUM_TO_CENTURY[20] = "K"
    NUM_TO_CENTURY[21] = "L"

    # Survey prefixes
    SURVEY_TO_PACKED["P-L"] = "PLS"
    SURVEY_TO_PACKED["T-1"] = "T1S"
    SURVEY_TO_PACKED["T-2"] = "T2S"
    SURVEY_TO_PACKED["T-3"] = "T3S"

    SURVEY_TO_UNPACKED["PLS"] = "P-L"
    SURVEY_TO_UNPACKED["T1S"] = "T-1"
    SURVEY_TO_UNPACKED["T2S"] = "T-2"
    SURVEY_TO_UNPACKED["T3S"] = "T-3"
}

# Get base-62 character from value
function base62_char(val) {
    return substr(BASE62, val + 1, 1)
}

# Get base-62 value from character
function base62_value(c) {
    return BASE62_VAL[c]
}

# Check if string is all digits
function is_numeric(s) {
    return s ~ /^[0-9]+$/
}

# Remove leading zeros
function strip_zeros(s) {
    sub(/^0+/, "", s)
    return (s == "") ? "0" : s
}

#==============================================================================
# Permanent asteroid encoding/decoding
#==============================================================================

function pack_permanent(num,    prefix_val, suffix, c1, c2, c3, c4, val) {
    num = int(num)

    if (num < 100000) {
        return sprintf("%05d", num)
    } else if (num < 620000) {
        prefix_val = int(num / 10000)
        suffix = num % 10000
        return base62_char(prefix_val) sprintf("%04d", suffix)
    } else {
        val = num - 620000
        c1 = base62_char(int(val / (62*62*62)) % 62)
        c2 = base62_char(int(val / (62*62)) % 62)
        c3 = base62_char(int(val / 62) % 62)
        c4 = base62_char(val % 62)
        return "~" c1 c2 c3 c4
    }
}

function unpack_permanent(packed,    first, v1, v2, v3, v4, prefix_val, suffix) {
    first = substr(packed, 1, 1)

    if (first == "~") {
        v1 = base62_value(substr(packed, 2, 1))
        v2 = base62_value(substr(packed, 3, 1))
        v3 = base62_value(substr(packed, 4, 1))
        v4 = base62_value(substr(packed, 5, 1))
        return 620000 + v1*62*62*62 + v2*62*62 + v3*62 + v4
    } else if (first ~ /[A-Z]/) {
        prefix_val = base62_value(first)
        suffix = int(substr(packed, 2, 4))
        return prefix_val * 10000 + suffix
    } else if (first ~ /[a-z]/) {
        prefix_val = base62_value(first)
        suffix = int(substr(packed, 2, 4))
        return prefix_val * 10000 + suffix
    } else {
        return int(packed)
    }
}

#==============================================================================
# Provisional designation encoding/decoding
#==============================================================================

function encode_cycle(cycle,    c1, c2) {
    c1 = base62_char(int(cycle / 10))
    c2 = cycle % 10
    return c1 c2
}

function decode_cycle(encoded,    v1, v2) {
    v1 = base62_value(substr(encoded, 1, 1))
    v2 = int(substr(encoded, 2, 1))
    return v1 * 10 + v2
}

function pack_provisional(desig,    year, half, second, cycle, century, century_char, yy) {
    # Parse: "1995 XA" or "1995 XA123"
    year = substr(desig, 1, 4)
    half = substr(desig, 6, 1)
    second = substr(desig, 7, 1)
    cycle = substr(desig, 8)
    if (cycle == "") cycle = 0

    century = int(year / 100)
    century_char = NUM_TO_CENTURY[century]
    yy = year % 100

    return century_char sprintf("%02d", yy) half encode_cycle(cycle) second
}

function unpack_provisional(packed,    century_char, yy, half, cycle_enc, second, century, year, cycle) {
    century_char = substr(packed, 1, 1)
    yy = substr(packed, 2, 2)
    half = substr(packed, 4, 1)
    cycle_enc = substr(packed, 5, 2)
    second = substr(packed, 7, 1)

    century = CENTURY_TO_NUM[century_char]
    year = century * 100 + int(yy)
    cycle = decode_cycle(cycle_enc)

    if (cycle == 0) {
        return year " " half second
    } else {
        return year " " half second cycle
    }
}

#==============================================================================
# Survey designation encoding/decoding
#==============================================================================

function pack_survey(desig,    num, survey, prefix) {
    # Parse: "2040 P-L"
    split(desig, parts, " ")
    num = parts[1]
    survey = parts[2]
    prefix = SURVEY_TO_PACKED[survey]
    return prefix sprintf("%04d", num)
}

function unpack_survey(packed,    prefix, num, survey) {
    prefix = substr(packed, 1, 3)
    num = int(substr(packed, 4, 4))
    survey = SURVEY_TO_UNPACKED[prefix]
    return num " " survey
}

#==============================================================================
# Comet encoding/decoding
#==============================================================================

function pack_numbered_comet(desig,    num, type) {
    # Parse: "1P"
    match(desig, /[PDCXA]$/)
    type = substr(desig, RSTART, 1)
    num = substr(desig, 1, RSTART - 1)
    return sprintf("%04d", num) type
}

function unpack_numbered_comet(packed,    num, type) {
    num = int(substr(packed, 1, 4))
    type = substr(packed, 5, 1)
    return num type
}

function pack_provisional_comet(desig,    type, year, half, order, fragment, century, century_char, yy, base_desig) {
    # Handle fragment: "C/1995 O1-b"
    fragment = ""
    base_desig = desig
    if (match(desig, /-[a-z]$/)) {
        fragment = substr(desig, RSTART + 1, 1)
        base_desig = substr(desig, 1, RSTART - 1)
    }

    type = substr(base_desig, 1, 1)
    year = substr(base_desig, 3, 4)
    half = substr(base_desig, 8, 1)
    order = substr(base_desig, 9)
    if (order == "") order = 0

    century = int(year / 100)
    century_char = NUM_TO_CENTURY[century]
    yy = year % 100

    if (fragment == "") fragment = "0"
    return type century_char sprintf("%02d", yy) half sprintf("%02d", order) fragment
}

function unpack_provisional_comet(packed,    type, century_char, yy, half, order, fragment, century, year, result) {
    type = substr(packed, 1, 1)
    century_char = substr(packed, 2, 1)
    yy = substr(packed, 3, 2)
    half = substr(packed, 5, 1)
    order = int(substr(packed, 6, 2))
    fragment = substr(packed, 8, 1)

    century = CENTURY_TO_NUM[century_char]
    year = century * 100 + int(yy)

    result = type "/" year " " half
    if (order > 0) result = result order
    if (fragment != "0" && fragment != "") result = result "-" fragment

    return result
}

#==============================================================================
# Natural satellite encoding/decoding
#==============================================================================

function pack_satellite(desig,    year, planet, num, century, century_char, yy) {
    # Parse: "S/2019 S 22"
    year = substr(desig, 3, 4)
    planet = substr(desig, 8, 1)
    num = substr(desig, 10)

    century = int(year / 100)
    century_char = NUM_TO_CENTURY[century]
    yy = year % 100

    return "S" century_char sprintf("%02d", yy) planet sprintf("%02d", num) "0"
}

function unpack_satellite(packed,    century_char, yy, planet, num, century, year) {
    century_char = substr(packed, 2, 1)
    yy = substr(packed, 3, 2)
    planet = substr(packed, 5, 1)
    num = int(substr(packed, 6, 2))

    century = CENTURY_TO_NUM[century_char]
    year = century * 100 + int(yy)

    return "S/" year " " planet " " num
}

#==============================================================================
# Format detection
#==============================================================================

function detect_format(desig,    len, first, prefix, last) {
    len = length(desig)
    first = substr(desig, 1, 1)

    # Packed permanent asteroid (5 chars, no slash or space)
    if (len == 5 && index(desig, "/") == 0 && index(desig, " ") == 0) {
        last = substr(desig, 5, 1)
        # Check for numbered comet
        if (last ~ /[PDCXA]/ && substr(desig, 1, 4) ~ /^[0-9]+$/) {
            return "packed_numbered_comet"
        }
        return "packed_permanent"
    }

    # Packed provisional asteroid (7 chars starting with century code)
    if (len == 7 && first ~ /[IJKL]/ && index(desig, "/") == 0) {
        return "packed_provisional"
    }

    # Packed survey (7 chars starting with PLS, T1S, T2S, T3S)
    if (len == 7) {
        prefix = substr(desig, 1, 3)
        if (prefix ~ /^(PLS|T[123]S)$/) {
            return "packed_survey"
        }
    }

    # Packed provisional comet or satellite (8 chars)
    if (len == 8) {
        second = substr(desig, 2, 1)
        if (first ~ /[PDCXA]/ && second ~ /[IJKL]/) {
            return "packed_provisional_comet"
        }
        if (first == "S" && second ~ /[IJKL]/) {
            return "packed_satellite"
        }
    }

    # Unpacked numbered asteroid (all digits)
    if (is_numeric(desig)) {
        return "unpacked_permanent"
    }

    # Unpacked survey
    if (desig ~ /^[0-9]+ (P-L|T-[123])$/) {
        return "unpacked_survey"
    }

    # Unpacked numbered comet
    if (desig ~ /^[0-9]+[PDCXA]$/) {
        return "unpacked_numbered_comet"
    }

    # Unpacked provisional comet
    if (desig ~ /^[PDCXA]\/[0-9]{4} [A-HJ-Y]/) {
        return "unpacked_provisional_comet"
    }

    # Unpacked satellite
    if (desig ~ /^S\/[0-9]{4} [JSUN] [0-9]+$/) {
        return "unpacked_satellite"
    }

    # Unpacked provisional asteroid
    if (desig ~ /^[0-9]{4} [A-HJ-Y][A-HJ-Z]/) {
        return "unpacked_provisional"
    }

    return "unknown"
}

#==============================================================================
# Main conversion function
#==============================================================================

function convert_simple(desig,    fmt) {
    fmt = detect_format(desig)

    if (fmt == "packed_permanent") return unpack_permanent(desig)
    if (fmt == "unpacked_permanent") return pack_permanent(desig)
    if (fmt == "packed_provisional") return unpack_provisional(desig)
    if (fmt == "unpacked_provisional") return pack_provisional(desig)
    if (fmt == "packed_survey") return unpack_survey(desig)
    if (fmt == "unpacked_survey") return pack_survey(desig)
    if (fmt == "packed_numbered_comet") return unpack_numbered_comet(desig)
    if (fmt == "unpacked_numbered_comet") return pack_numbered_comet(desig)
    if (fmt == "packed_provisional_comet") return unpack_provisional_comet(desig)
    if (fmt == "unpacked_provisional_comet") return pack_provisional_comet(desig)
    if (fmt == "packed_satellite") return unpack_satellite(desig)
    if (fmt == "unpacked_satellite") return pack_satellite(desig)

    return "ERROR: Unknown format: " desig
}

# Library only - no main block
# For CLI use, see mpc_designation_main.awk
