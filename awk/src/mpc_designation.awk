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

# Convert number to base-62 string with specified width
function num_to_base62(num, width,    result, i) {
    result = ""
    for (i = 0; i < width; i++) {
        result = base62_char(num % 62) result
        num = int(num / 62)
    }
    return result
}

# Get letter position (A=0, B=1, ..., H=7, J=8, ..., Z=24)
# Skips I
function letter_position(c,    pos) {
    pos = index("ABCDEFGHJKLMNOPQRSTUVWXYZ", c) - 1
    return pos
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

function pack_provisional(desig,    year, half, second, cycle, century, century_char, yy, second_pos, encoded_val) {
    # Parse: "1995 XA" or "1995 XA123"
    year = int(substr(desig, 1, 4))
    half = substr(desig, 6, 1)
    second = substr(desig, 7, 1)
    cycle = substr(desig, 8)
    if (cycle == "") cycle = 0
    cycle = int(cycle)

    # Extended format for cycle >= 620
    if (cycle >= 620) {
        second_pos = letter_position(second)
        encoded_val = (cycle - 620) * 25 + second_pos
        return "_" base62_char(year - 2000) half num_to_base62(encoded_val, 4)
    }

    century = int(year / 100)
    century_char = NUM_TO_CENTURY[century]
    yy = year % 100

    return century_char sprintf("%02d", yy) half encode_cycle(cycle) second
}

function unpack_provisional(packed,    century_char, yy, half, cycle_enc, second, century, year, cycle, prefix, short_year) {
    century_char = substr(packed, 1, 1)
    yy = substr(packed, 2, 2)
    half = substr(packed, 4, 1)
    cycle_enc = substr(packed, 5, 2)
    second = substr(packed, 7, 1)

    century = CENTURY_TO_NUM[century_char]
    year = century * 100 + int(yy)
    cycle = decode_cycle(cycle_enc)

    # Pre-1925 designations use A-prefix format
    if (year < 1925) {
        # A for 1xxx years, B for 2xxx years
        prefix = (year < 2000) ? "A" : "B"
        short_year = year % 1000  # Last 3 digits
        if (cycle == 0) {
            return prefix sprintf("%03d", short_year) " " half second
        } else {
            return prefix sprintf("%03d", short_year) " " half second cycle
        }
    }

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

function pack_numbered_comet(desig,    num, type, fragment, base_desig) {
    # Parse: "1P", "73P-A", "73P-AA"
    fragment = ""
    base_desig = desig

    # Check for fragment suffix
    if (match(desig, /-[A-Z][A-Z]$/)) {
        fragment = tolower(substr(desig, RSTART + 1, 2))
        base_desig = substr(desig, 1, RSTART - 1)
    } else if (match(desig, /-[A-Z]$/)) {
        fragment = tolower(substr(desig, RSTART + 1, 1))
        base_desig = substr(desig, 1, RSTART - 1)
    }

    match(base_desig, /[PD]$/)
    type = substr(base_desig, RSTART, 1)
    num = substr(base_desig, 1, RSTART - 1)
    return sprintf("%04d", num) type fragment
}

function unpack_numbered_comet(packed,    num, type, fragment, len) {
    len = length(packed)
    num = int(substr(packed, 1, 4))
    type = substr(packed, 5, 1)

    # Check for fragment (6 or 7 chars total)
    if (len > 5) {
        fragment = toupper(substr(packed, 6))
        return num type "-" fragment
    }
    return num type
}

function pack_provisional_comet(desig,    type, year, half, order, fragment, century, century_char, yy, base_desig, second, cycle, space_pos, desig_part, char9) {
    # Handle fragment: "C/1995 O1-b" or "C/1995 O1-B" (1 or 2 letter)
    fragment = ""
    base_desig = desig
    if (match(desig, /-[A-Za-z][A-Za-z]$/)) {
        # 2-letter fragment
        fragment = tolower(substr(desig, RSTART + 1, 2))
        base_desig = substr(desig, 1, RSTART - 1)
    } else if (match(desig, /-[A-Za-z]$/)) {
        # 1-letter fragment
        fragment = tolower(substr(desig, RSTART + 1, 1))
        base_desig = substr(desig, 1, RSTART - 1)
    }

    type = substr(base_desig, 1, 1)

    # Find the space to handle variable-length years
    space_pos = index(base_desig, " ")
    year = substr(base_desig, 3, space_pos - 3)
    desig_part = substr(base_desig, space_pos + 1)  # "O1" or "AH2"
    half = substr(desig_part, 1, 1)

    century = int(year / 100)
    yy = year % 100

    # Get century character (A-H for 10-17, I-L for 18-21)
    if (century >= 10 && century <= 17) {
        century_char = substr("ABCDEFGH", century - 9, 1)
    } else {
        century_char = NUM_TO_CENTURY[century]
    }

    # Check if asteroid-style format: "AH2" (half + second letter + cycle)
    # vs simple format: "O1" (half + order number)
    char9 = substr(desig_part, 2, 1)
    if (length(desig_part) > 1 && char9 ~ /[A-Z]/) {
        # Asteroid-style format
        second = char9
        cycle = substr(desig_part, 3)
        if (cycle == "") cycle = 0
        return type century_char sprintf("%02d", yy) half encode_cycle(int(cycle)) second
    } else {
        # Simple format - order number only
        order = substr(desig_part, 2)
        if (order == "") order = 0
        if (fragment == "") fragment = "0"
        return type century_char sprintf("%02d", yy) half sprintf("%02d", order) fragment
    }
}

function unpack_provisional_comet(packed,    type, century_char, yy, half, pos67, char8, century, year, result, cycle, second, order, fragment) {
    type = substr(packed, 1, 1)
    century_char = substr(packed, 2, 1)
    yy = substr(packed, 3, 2)
    half = substr(packed, 5, 1)
    pos67 = substr(packed, 6, 2)
    char8 = substr(packed, 8, 1)

    century = CENTURY_TO_NUM[century_char]
    year = century * 100 + int(yy)

    # Check if asteroid-style format (position 8 is uppercase letter)
    if (char8 ~ /[A-Z]/) {
        # Asteroid-style: positions 6-7 are cycle encoded, position 8 is second letter
        cycle = decode_cycle(pos67)
        second = char8
        result = type "/" year " " half second
        if (cycle > 0) result = result cycle
    } else {
        # Simple format: positions 6-7 are order number, position 8 is fragment
        order = int(pos67)
        fragment = char8
        result = type "/" year " " half
        if (order > 0) result = result order
        if (fragment != "0" && fragment != "") result = result "-" fragment
    }

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
# Old-style provisional encoding (A908 CJ format)
#==============================================================================

function pack_old_style(desig,    yyy, half, second, yy, year, century, century_char) {
    # Parse: "A908 CJ" or "A873 OA" - old-style format used 1800s-1924
    # The A prefix + 3 digits gives year: A908 = 1908, A873 = 1873
    yyy = int(substr(desig, 2, 3))  # "908" or "873"
    half = substr(desig, 6, 1)
    second = substr(desig, 7, 1)

    # Calculate full year: add 1000 to 3-digit year
    year = 1000 + yyy
    century = int(year / 100)
    yy = year % 100
    century_char = NUM_TO_CENTURY[century]

    return century_char sprintf("%02d", yy) half "00" second
}

#==============================================================================
# Ancient comet encoding (year < 1000 or 1000-1999)
#==============================================================================

function pack_ancient_comet(desig,    type, space_pos, year, desig_part, half, order, century, yy, century_char) {
    # Parse: "C/240 V1" or "C/1014 C1"
    type = substr(desig, 1, 1)
    space_pos = index(desig, " ")
    year = int(substr(desig, 3, space_pos - 3))
    desig_part = substr(desig, space_pos + 1)
    half = substr(desig_part, 1, 1)
    order = substr(desig_part, 2)
    if (order == "") order = 0

    if (year >= 1000) {
        # Years 1000-1999 use extended century codes (A=10, B=11, ...)
        century = int(year / 100)
        yy = year % 100
        # A=10, B=11, ..., I=18 (then J-L for 19-21 as usual)
        if (century >= 10 && century <= 17) {
            century_char = substr("ABCDEFGH", century - 9, 1)
        } else if (century >= 18 && century <= 21) {
            century_char = NUM_TO_CENTURY[century]
        }
        return type century_char sprintf("%02d", yy) half sprintf("%02d", int(order)) "0"
    } else {
        # Years < 1000: format is type + 3-digit year + half + order + fragment
        return type sprintf("%03d", year) half sprintf("%02d", int(order)) "0"
    }
}

#==============================================================================
# BCE comet encoding (negative year)
#==============================================================================

function pack_bce_comet(desig,    type, space_pos, year, desig_part, half, order, abs_year, century_code, yy) {
    # Parse: "C/-146 P1" - BCE comet with negative year
    type = substr(desig, 1, 1)
    space_pos = index(desig, " ")
    year = substr(desig, 3, space_pos - 3)  # "-146"
    desig_part = substr(desig, space_pos + 1)
    half = substr(desig_part, 1, 1)
    order = substr(desig_part, 2)
    if (order == "") order = 0

    abs_year = -int(year)
    # Century codes: / for 1-99, . for 100-199, - for 200-299
    # yy = (century_max - abs_year - 1)
    if (abs_year < 100) {
        century_code = "/"
        yy = 100 - abs_year - 1
    } else if (abs_year < 200) {
        century_code = "."
        yy = 200 - abs_year - 1
    } else {
        century_code = "-"
        yy = 300 - abs_year - 1
    }

    return type century_code sprintf("%02d", yy) half sprintf("%02d", int(order)) "0"
}

#==============================================================================
# Format detection
#==============================================================================

function detect_format(desig,    len, first, prefix, last) {
    len = length(desig)
    first = substr(desig, 1, 1)

    # Packed numbered comet with fragment (6 or 7 chars: 0073Pa or 0073Paa)
    if ((len == 6 || len == 7) && desig ~ /^[0-9]{4}[PD][a-z]{1,2}$/) {
        return "packed_numbered_comet"
    }

    # Packed permanent asteroid (5 chars, no slash, space, or hyphen)
    if (len == 5 && index(desig, "/") == 0 && index(desig, " ") == 0 && index(desig, "-") == 0) {
        last = substr(desig, 5, 1)
        # Check for numbered comet
        if (last ~ /[PD]/ && substr(desig, 1, 4) ~ /^[0-9]+$/) {
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

    # Unpacked numbered comet (with optional fragment)
    if (desig ~ /^[0-9]+[PD](-[A-Z]{1,2})?$/) {
        return "unpacked_numbered_comet"
    }

    # Unpacked provisional comet (4-digit year)
    if (desig ~ /^[PDCXA]\/[0-9]{4} [A-HJ-Y]/) {
        return "unpacked_provisional_comet"
    }

    # Unpacked ancient comet (1-3 digit year)
    if (desig ~ /^[PDCXA]\/[0-9]{1,3} [A-HJ-Y]/) {
        return "unpacked_ancient_comet"
    }

    # Unpacked BCE comet (negative year)
    if (desig ~ /^[PDCXA]\/-[0-9]+ [A-HJ-Y]/) {
        return "unpacked_bce_comet"
    }

    # Unpacked satellite
    if (desig ~ /^S\/[0-9]{4} [JSUN] [0-9]+$/) {
        return "unpacked_satellite"
    }

    # Old-style provisional asteroid (A908 CJ format, 1900-1924)
    if (desig ~ /^[A-Z][0-9]{3} [A-HJ-Y][A-HJ-Z]/) {
        return "unpacked_old_style"
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
    if (fmt == "unpacked_old_style") return pack_old_style(desig)
    if (fmt == "unpacked_ancient_comet") return pack_ancient_comet(desig)
    if (fmt == "unpacked_bce_comet") return pack_bce_comet(desig)

    return "ERROR: Unknown format: " desig
}

#==============================================================================
# Pack/Unpack functions (ensure specific format)
#==============================================================================

function pack(desig,    fmt) {
    fmt = detect_format(desig)
    if (fmt ~ /^packed/) {
        return desig
    }
    return convert_simple(desig)
}

function unpack(desig,    fmt) {
    fmt = detect_format(desig)
    if (fmt ~ /^unpacked/) {
        return desig
    }
    return convert_simple(desig)
}

#==============================================================================
# Helper functions for report format and fragment handling
#==============================================================================

# Convert minimal packed format to 12-character MPC report format
function to_report_format(minimal,    fmt, len, base_part, fragment, padding) {
    fmt = detect_format(minimal)

    # If not packed, pack it first
    if (fmt !~ /^packed/) {
        minimal = pack(minimal)
        fmt = detect_format(minimal)
    }

    len = length(minimal)

    # Numbered comets: right-pad to 12 chars
    # Handle fragments: "0073Pa" -> "0073P      a", "0073Paa" -> "0073P     aa"
    if (fmt == "packed_numbered_comet") {
        if (len > 5) {
            base_part = substr(minimal, 1, 5)  # "0073P"
            fragment = substr(minimal, 6)       # "a" or "aa"
            padding = 12 - 5 - length(fragment)
            return base_part sprintf("%" padding "s", "") fragment
        }
        return sprintf("%-12s", minimal)
    }

    # Everything else: left-pad to 12 chars
    return sprintf("%12s", minimal)
}

# Convert 12-character MPC report format to minimal packed format
function from_report_format(report,    trimmed, base_part, fragment) {
    if (length(report) != 12) {
        return "ERROR: Report format must be exactly 12 characters"
    }

    # Check for numbered comet format (starts with 4 digits + P/D)
    if (report ~ /^[0-9]{4}[PD]/) {
        base_part = substr(report, 1, 5)
        # Trim spaces and get fragment if any
        fragment = substr(report, 6)
        gsub(/^[ ]+/, "", fragment)
        gsub(/[ ]+$/, "", fragment)
        return base_part fragment
    }

    # For everything else, just trim whitespace
    trimmed = report
    gsub(/^[ ]+/, "", trimmed)
    gsub(/[ ]+$/, "", trimmed)
    return trimmed
}

# Check if a comet designation has a fragment suffix
function has_fragment(desig,    d) {
    d = desig
    gsub(/^[ ]+/, "", d)
    gsub(/[ ]+$/, "", d)

    # Unpacked format with fragment: "73P-A", "73P-AA", "D/1993 F2-A"
    if (d ~ /-[A-Z]{1,2}$/) {
        return 1
    }

    # Packed numbered comet with fragment: "0073Pa", "0073Paa"
    if (d ~ /^[0-9]{4}[PD][a-z]{1,2}$/) {
        return 1
    }

    # Packed provisional comet with fragment: "DJ93F02a" (8 chars, last char lowercase)
    if (d ~ /^[PCDXAI][A-L][0-9]{2}[A-Z][0-9]{2}[a-z]$/) {
        return 1
    }

    return 0
}

# Get the fragment letter(s) from a comet designation (uppercase)
function get_fragment(desig,    d, frag) {
    d = desig
    gsub(/^[ ]+/, "", d)
    gsub(/[ ]+$/, "", d)

    # Unpacked format: "73P-A" -> "A", "73P-AA" -> "AA"
    if (match(d, /-([A-Z]{1,2})$/)) {
        return substr(d, RSTART + 1)
    }

    # Packed numbered comet: "0073Pa" -> "A", "0073Paa" -> "AA"
    if (d ~ /^[0-9]{4}[PD][a-z]{1,2}$/) {
        frag = substr(d, 6)
        return toupper(frag)
    }

    # Packed provisional comet: "DJ93F02a" -> "A"
    if (d ~ /^[PCDXAI][A-L][0-9]{2}[A-Z][0-9]{2}[a-z]$/) {
        return toupper(substr(d, 8, 1))
    }

    return ""
}

# Get the parent comet designation without fragment suffix
function get_parent(desig,    d) {
    d = desig
    gsub(/^[ ]+/, "", d)
    gsub(/[ ]+$/, "", d)

    # Unpacked format: "73P-A" -> "73P"
    if (match(d, /-[A-Z]{1,2}$/)) {
        return substr(d, 1, RSTART - 1)
    }

    # Packed numbered comet: "0073Pa" -> "0073P"
    if (d ~ /^[0-9]{4}[PD][a-z]{1,2}$/) {
        return substr(d, 1, 5)
    }

    # Packed provisional comet: "DJ93F02a" -> "DJ93F020"
    if (d ~ /^[PCDXAI][A-L][0-9]{2}[A-Z][0-9]{2}[a-z]$/) {
        return substr(d, 1, 7) "0"
    }

    return d
}

# Check if two designations refer to the same object
function designations_equal(desig1, desig2,    packed1, packed2) {
    packed1 = pack(desig1)
    packed2 = pack(desig2)

    # Check for errors
    if (packed1 ~ /^ERROR/ || packed2 ~ /^ERROR/) {
        return 0
    }

    return (packed1 == packed2) ? 1 : 0
}

# Library only - no main block
# For CLI use, see mpc_designation_main.awk
