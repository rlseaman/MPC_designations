#!/bin/bash
# mpc_designation.sh - MPC Designation Converter
#
# Converts between packed and unpacked Minor Planet Center designations.
# Supports: asteroids (permanent, provisional, survey), comets, natural satellites.
#
# Compatible with bash 3.2+ (macOS default)
#
# Usage: source this file and call convert_simple "designation"
#        or run directly: ./mpc_designation.sh <designation>

# Base-62 character set
readonly BASE62="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

#==============================================================================
# Helper functions
#==============================================================================

# Get character value in base-62 (0-9=0-9, A-Z=10-35, a-z=36-61)
base62_value() {
    local c="$1"
    local pos="${BASE62%%$c*}"
    echo "${#pos}"
}

# Get base-62 character from value
base62_char() {
    local val="$1"
    echo "${BASE62:$val:1}"
}

# Convert century character to number (asteroids: I-L = 1800-2199)
century_to_num() {
    case "$1" in
        I) echo 18 ;;
        J) echo 19 ;;
        K) echo 20 ;;
        L) echo 21 ;;
        *) echo -1 ;;
    esac
}

# Convert century character to number for comets (A-L = 1000-2199)
comet_century_to_num() {
    case "$1" in
        A) echo 10 ;;
        B) echo 11 ;;
        C) echo 12 ;;
        D) echo 13 ;;
        E) echo 14 ;;
        F) echo 15 ;;
        G) echo 16 ;;
        H) echo 17 ;;
        I) echo 18 ;;
        J) echo 19 ;;
        K) echo 20 ;;
        L) echo 21 ;;
        *) echo -1 ;;
    esac
}

# Convert century number to character for comets
num_to_comet_century() {
    case "$1" in
        10) echo "A" ;;
        11) echo "B" ;;
        12) echo "C" ;;
        13) echo "D" ;;
        14) echo "E" ;;
        15) echo "F" ;;
        16) echo "G" ;;
        17) echo "H" ;;
        18) echo "I" ;;
        19) echo "J" ;;
        20) echo "K" ;;
        21) echo "L" ;;
        *) echo "" ;;
    esac
}

# Convert century number to character
num_to_century() {
    case "$1" in
        18) echo "I" ;;
        19) echo "J" ;;
        20) echo "K" ;;
        21) echo "L" ;;
        *) echo "" ;;
    esac
}

# Survey prefix conversions
survey_to_packed() {
    case "$1" in
        "P-L") echo "PLS" ;;
        "T-1") echo "T1S" ;;
        "T-2") echo "T2S" ;;
        "T-3") echo "T3S" ;;
        *) echo "" ;;
    esac
}

survey_to_unpacked() {
    case "$1" in
        PLS) echo "P-L" ;;
        T1S) echo "T-1" ;;
        T2S) echo "T-2" ;;
        T3S) echo "T-3" ;;
        *) echo "" ;;
    esac
}

# Check if string is all digits
is_numeric() {
    case "$1" in
        ''|*[!0-9]*) return 1 ;;
        *) return 0 ;;
    esac
}

# Check if character is uppercase letter
is_upper() {
    case "$1" in
        [A-Z]) return 0 ;;
        *) return 1 ;;
    esac
}

# Check if character is lowercase letter
is_lower() {
    case "$1" in
        [a-z]) return 0 ;;
        *) return 1 ;;
    esac
}

# Remove leading zeros from number
strip_leading_zeros() {
    local num="$1"
    # Remove leading zeros, handle all-zeros case
    num="${num#"${num%%[!0]*}"}"
    echo "${num:-0}"
}

#==============================================================================
# Permanent asteroid encoding/decoding
#==============================================================================

# Pack permanent asteroid number to 5-char format
pack_permanent() {
    local num
    num=$(strip_leading_zeros "$1")

    if [ "$num" -lt 100000 ]; then
        # Simple 5-digit format with leading zeros
        printf "%05d\n" "$num"
    elif [ "$num" -lt 620000 ]; then
        # Letter prefix format: A0001-z9999
        local prefix_val=$((num / 10000))
        local suffix=$((num % 10000))
        local prefix_char
        prefix_char=$(base62_char "$prefix_val")
        printf "%s%04d\n" "$prefix_char" "$suffix"
    else
        # Tilde + base-62 format for 620000+
        local val=$((num - 620000))
        local c1 c2 c3 c4
        c1=$(base62_char $((val / 62 / 62 / 62 % 62)))
        c2=$(base62_char $((val / 62 / 62 % 62)))
        c3=$(base62_char $((val / 62 % 62)))
        c4=$(base62_char $((val % 62)))
        echo "~${c1}${c2}${c3}${c4}"
    fi
}

# Unpack permanent asteroid from 5-char format
unpack_permanent() {
    local packed="$1"
    local first="${packed:0:1}"

    if [ "$first" = "~" ]; then
        # Tilde format: decode base-62
        local v1 v2 v3 v4
        v1=$(base62_value "${packed:1:1}")
        v2=$(base62_value "${packed:2:1}")
        v3=$(base62_value "${packed:3:1}")
        v4=$(base62_value "${packed:4:1}")
        echo $((620000 + v1*62*62*62 + v2*62*62 + v3*62 + v4))
    elif is_upper "$first"; then
        # Uppercase letter prefix: A=10, Z=35
        local prefix_val
        prefix_val=$(base62_value "$first")
        local suffix
        suffix=$(strip_leading_zeros "${packed:1:4}")
        echo $((prefix_val * 10000 + suffix))
    elif is_lower "$first"; then
        # Lowercase letter prefix: a=36, z=61
        local prefix_val
        prefix_val=$(base62_value "$first")
        local suffix
        suffix=$(strip_leading_zeros "${packed:1:4}")
        echo $((prefix_val * 10000 + suffix))
    else
        # Pure numeric
        strip_leading_zeros "$packed"
    fi
}

#==============================================================================
# Provisional designation encoding/decoding
#==============================================================================

# Encode cycle count to 2-char format
encode_cycle() {
    local cycle="$1"
    local c1 c2
    c1=$(base62_char $((cycle / 10)))
    c2=$((cycle % 10))
    echo "${c1}${c2}"
}

# Decode 2-char cycle count
decode_cycle() {
    local encoded="$1"
    local v1 v2
    v1=$(base62_value "${encoded:0:1}")
    v2="${encoded:1:1}"
    echo $((v1 * 10 + v2))
}

# Pack provisional designation: "1995 XA" -> "J95X00A", "A908 CJ" -> "J08C00J"
pack_provisional() {
    local desig="$1"

    # Parse: year half second [cycle]
    local year half second cycle

    # Check for A-prefix format (A908 CJ -> 1908 CJ)
    local first="${desig:0:1}"
    if [ "$first" = "A" ] || [ "$first" = "B" ]; then
        local rest="${desig:1:3}"
        local first_digit
        if [ "$first" = "A" ]; then
            first_digit=1
        else
            first_digit=2
        fi
        year="${first_digit}${rest}"
        # Skip space, get half-month letter
        half="${desig:5:1}"
        # Get second letter
        second="${desig:6:1}"
        # Get optional cycle number
        cycle="${desig:7}"
        cycle="${cycle:-0}"
    else
        # Extract year (first 4 chars)
        year="${desig:0:4}"
        # Skip space, get half-month letter
        half="${desig:5:1}"
        # Get second letter
        second="${desig:6:1}"
        # Get optional cycle number (rest of string after position 7)
        cycle="${desig:7}"
        cycle="${cycle:-0}"
    fi

    # Century code
    local century=$((year / 100))
    local century_char
    century_char=$(num_to_century "$century")
    if [ -z "$century_char" ]; then
        echo "ERROR: Unsupported century: $century" >&2
        return 1
    fi

    # Year within century
    local yy=$((year % 100))
    local yy_str
    printf -v yy_str "%02d" "$yy"

    # Cycle encoding
    local cycle_encoded
    cycle_encoded=$(encode_cycle "$cycle")

    echo "${century_char}${yy_str}${half}${cycle_encoded}${second}"
}

# Unpack provisional: "J95X00A" -> "1995 XA", "J08C00J" -> "A908 CJ" (pre-1925)
unpack_provisional() {
    local packed="$1"

    local century_char="${packed:0:1}"
    local yy="${packed:1:2}"
    local half="${packed:3:1}"
    local cycle_encoded="${packed:4:2}"
    local second="${packed:6:1}"

    # Decode century
    local century
    century=$(century_to_num "$century_char")
    if [ "$century" -lt 0 ]; then
        echo "ERROR: Invalid century code: $century_char" >&2
        return 1
    fi

    local year_suffix
    year_suffix=$(strip_leading_zeros "$yy")
    local year=$((century * 100 + year_suffix))

    # Decode cycle
    local cycle
    cycle=$(decode_cycle "$cycle_encoded")

    # Pre-1925 designations use A-prefix format
    if [ "$year" -lt 1925 ]; then
        local first_digit=$((year / 1000))
        local rest=$((year % 1000))
        local prefix
        if [ "$first_digit" -eq 1 ]; then
            prefix="A"
        elif [ "$first_digit" -eq 2 ]; then
            prefix="B"
        fi
        if [ "$cycle" -eq 0 ]; then
            printf "%s%03d %s%s\n" "$prefix" "$rest" "$half" "$second"
        else
            printf "%s%03d %s%s%d\n" "$prefix" "$rest" "$half" "$second" "$cycle"
        fi
    else
        if [ "$cycle" -eq 0 ]; then
            echo "$year $half$second"
        else
            echo "$year $half$second$cycle"
        fi
    fi
}

#==============================================================================
# Survey designation encoding/decoding
#==============================================================================

# Pack survey: "2040 P-L" -> "PLS2040"
pack_survey() {
    local desig="$1"

    # Extract number and survey type
    local num="${desig%% *}"
    local survey="${desig#* }"

    local prefix
    prefix=$(survey_to_packed "$survey")
    if [ -z "$prefix" ]; then
        echo "ERROR: Invalid survey format: $desig" >&2
        return 1
    fi

    printf "%s%04d\n" "$prefix" "$num"
}

# Unpack survey: "PLS2040" -> "2040 P-L"
unpack_survey() {
    local packed="$1"
    local prefix="${packed:0:3}"
    local num="${packed:3:4}"

    local survey
    survey=$(survey_to_unpacked "$prefix")
    if [ -z "$survey" ]; then
        echo "ERROR: Invalid survey prefix: $prefix" >&2
        return 1
    fi

    local num_val
    num_val=$(strip_leading_zeros "$num")
    echo "$num_val $survey"
}

#==============================================================================
# Comet encoding/decoding
#==============================================================================

# Pack numbered comet: "1P" -> "0001P", "73P-A" -> "0073Pa", "73P-AA" -> "0073Paa"
pack_numbered_comet() {
    local desig="$1"
    local fragment=""
    local base_desig="$desig"

    # Check for fragment suffix: 73P-A or 73P-AA
    if [[ "$desig" =~ ^([0-9]+)([PD])-([A-Z]{1,2})(/.*)?$ ]]; then
        local num="${BASH_REMATCH[1]}"
        local type="${BASH_REMATCH[2]}"
        fragment="${BASH_REMATCH[3]}"
        # Convert fragment to lowercase
        fragment=$(echo "$fragment" | tr 'A-Z' 'a-z')
        printf "%04d%s%s\n" "$num" "$type" "$fragment"
        return
    fi

    # Check for base comet with optional name suffix: 73P or 73P/Schwassmann-Wachmann
    if [[ "$desig" =~ ^([0-9]+)([PDCXA])(/.*)?$ ]]; then
        local num="${BASH_REMATCH[1]}"
        local type="${BASH_REMATCH[2]}"
        printf "%04d%s\n" "$num" "$type"
        return
    fi

    # Fallback
    local len="${#desig}"
    local type="${desig: -1}"
    local num="${desig:0:$((len-1))}"
    printf "%04d%s\n" "$num" "$type"
}

# Unpack numbered comet: "0001P" -> "1P", "0073Pa" -> "73P-A", "0073Paa" -> "73P-AA"
unpack_numbered_comet() {
    local packed="$1"
    local len="${#packed}"
    local num="${packed:0:4}"
    local type="${packed:4:1}"
    local num_val
    num_val=$(strip_leading_zeros "$num")

    # Check for fragment suffix (6 or 7 chars)
    if [ "$len" -eq 6 ]; then
        # Single-letter fragment
        local fragment="${packed:5:1}"
        fragment=$(echo "$fragment" | tr 'a-z' 'A-Z')
        echo "${num_val}${type}-${fragment}"
    elif [ "$len" -eq 7 ]; then
        # Two-letter fragment
        local fragment="${packed:5:2}"
        fragment=$(echo "$fragment" | tr 'a-z' 'A-Z')
        echo "${num_val}${type}-${fragment}"
    else
        echo "${num_val}${type}"
    fi
}

# Pack provisional comet: "C/1995 O1" -> "CJ95O010", "P/1930 J1-AA" -> "PJ30J01aa"
pack_provisional_comet() {
    local desig="$1"

    # Handle fragment suffix (single or double letter, upper or lower case)
    local fragment=""
    local base_desig="$desig"
    if [[ "$desig" =~ -([A-Za-z]{1,2})$ ]]; then
        fragment="${BASH_REMATCH[1]}"
        base_desig="${desig:0:$((${#desig}-${#fragment}-1))}"
        # Convert fragment to lowercase
        fragment=$(echo "$fragment" | tr 'A-Z' 'a-z')
    fi

    local type="${base_desig:0:1}"
    local year="${base_desig:2:4}"
    local half="${base_desig:7:1}"
    local order="${base_desig:8}"
    order="${order:-0}"

    local century=$((year / 100))
    local century_char
    century_char=$(num_to_comet_century "$century")
    local yy=$((year % 100))

    if [ -n "$fragment" ]; then
        printf "%s%s%02d%s%02d%s\n" "$type" "$century_char" "$yy" "$half" "$order" "$fragment"
    else
        printf "%s%s%02d%s%02d0\n" "$type" "$century_char" "$yy" "$half" "$order"
    fi
}

# Unpack provisional comet: "CJ95O010" -> "C/1995 O1", "PJ30J01aa" -> "P/1930 J1-AA"
unpack_provisional_comet() {
    local packed="$1"
    local len="${#packed}"

    local type="${packed:0:1}"
    local century_char="${packed:1:1}"
    local yy="${packed:2:2}"
    local half="${packed:4:1}"
    local order="${packed:5:2}"

    # Handle fragment (single or double letter)
    local fragment=""
    if [ "$len" -eq 9 ]; then
        # Two-letter fragment
        fragment="${packed:7:2}"
    elif [ "$len" -eq 8 ]; then
        fragment="${packed:7:1}"
    fi

    local century
    century=$(comet_century_to_num "$century_char")
    local yy_val
    yy_val=$(strip_leading_zeros "$yy")
    local year=$((century * 100 + yy_val))
    local order_num
    order_num=$(strip_leading_zeros "$order")

    local result="$type/$year $half"
    if [ "$order_num" -gt 0 ]; then
        result="${result}${order_num}"
    fi

    if [ "$fragment" != "0" ] && [ -n "$fragment" ]; then
        # Convert fragment to uppercase
        fragment=$(echo "$fragment" | tr 'a-z' 'A-Z')
        result="${result}-${fragment}"
    fi

    echo "$result"
}

#==============================================================================
# Natural satellite encoding/decoding
#==============================================================================

# Pack satellite: "S/2019 S 22" -> "SK19S220"
pack_satellite() {
    local desig="$1"

    local year="${desig:2:4}"
    local planet="${desig:7:1}"
    local num="${desig:9}"

    local century=$((year / 100))
    local century_char
    century_char=$(num_to_century "$century")
    local yy=$((year % 100))

    printf "S%s%02d%s%02d0\n" "$century_char" "$yy" "$planet" "$num"
}

# Unpack satellite: "SK19S220" -> "S/2019 S 22"
unpack_satellite() {
    local packed="$1"

    local century_char="${packed:1:1}"
    local yy="${packed:2:2}"
    local planet="${packed:4:1}"
    local num="${packed:5:2}"

    local century
    century=$(century_to_num "$century_char")
    local yy_val
    yy_val=$(strip_leading_zeros "$yy")
    local year=$((century * 100 + yy_val))
    local num_val
    num_val=$(strip_leading_zeros "$num")

    echo "S/$year $planet $num_val"
}

#==============================================================================
# Format detection
#==============================================================================

detect_format() {
    local desig="$1"
    local len="${#desig}"
    local first="${desig:0:1}"

    # Packed numbered comet: 0001P (5 chars), 0073Pa (6 chars), 0073Paa (7 chars)
    # Must be exactly 4 digits + P/D + optional lowercase fragment
    if [ "$len" -ge 5 ] && [ "$len" -le 7 ]; then
        local c5="${desig:4:1}"
        case "$c5" in
            [PD])
                if is_numeric "${desig:0:4}"; then
                    if [ "$len" -eq 5 ]; then
                        echo "packed_numbered_comet"
                        return
                    elif [ "$len" -eq 6 ] || [ "$len" -eq 7 ]; then
                        local rest="${desig:5}"
                        case "$rest" in
                            [a-z]|[a-z][a-z])
                                echo "packed_numbered_comet"
                                return
                                ;;
                        esac
                    fi
                fi
                ;;
        esac
    fi

    # Unpacked numbered comet (with or without fragment)
    # Match: 73P, 73P-A, 73P-AA, 73P/Name, 73P-A/Name
    # Unpacked should NOT have leading zeros (e.g., 73P not 073P)
    if [[ "$desig" =~ ^([1-9][0-9]*)([PD])(-[A-Z]{1,2})?(/.*)?$ ]]; then
        echo "unpacked_numbered_comet"
        return
    fi

    # Packed permanent asteroid (5 chars, no slash or space)
    if [ "$len" -eq 5 ]; then
        case "$desig" in
            *[/\ ]*) ;;
            *)
                echo "packed_permanent"
                return
                ;;
        esac
    fi

    # Packed provisional asteroid (7 chars starting with century code)
    if [ "$len" -eq 7 ]; then
        case "$first" in
            [IJKL])
                case "$desig" in
                    */*) ;;
                    *) echo "packed_provisional"; return ;;
                esac
                ;;
        esac
        # Packed survey (7 chars starting with PLS, T1S, T2S, T3S)
        local prefix="${desig:0:3}"
        case "$prefix" in
            PLS|T1S|T2S|T3S)
                echo "packed_survey"
                return
                ;;
        esac
    fi

    # Packed provisional comet (8-9 chars) or satellite (8 chars)
    if [ "$len" -eq 8 ] || [ "$len" -eq 9 ]; then
        local second="${desig:1:1}"
        case "$first" in
            [PDCXA])
                case "$second" in
                    [A-L]) echo "packed_provisional_comet"; return ;;
                esac
                ;;
            S)
                if [ "$len" -eq 8 ]; then
                    case "$second" in
                        [IJKL]) echo "packed_satellite"; return ;;
                    esac
                fi
                ;;
        esac
    fi

    # Unpacked numbered asteroid (all digits)
    if is_numeric "$desig"; then
        echo "unpacked_permanent"
        return
    fi

    # Unpacked survey
    case "$desig" in
        *" P-L"|*" T-1"|*" T-2"|*" T-3")
            local num="${desig%% *}"
            if is_numeric "$num"; then
                echo "unpacked_survey"
                return
            fi
            ;;
    esac

    # Also match older comet types C, X, A (rare)
    case "$desig" in
        *[CXA])
            local num="${desig%[CXA]}"
            if is_numeric "$num"; then
                echo "unpacked_numbered_comet"
                return
            fi
            ;;
    esac

    # Unpacked provisional comet (with or without fragment)
    case "$desig" in
        [PDCXA]/*)
            echo "unpacked_provisional_comet"
            return
            ;;
    esac

    # Unpacked satellite
    case "$desig" in
        "S/"*)
            echo "unpacked_satellite"
            return
            ;;
    esac

    # Unpacked provisional asteroid (year + space + designation) or A-prefix format
    case "$desig" in
        [0-9][0-9][0-9][0-9]" "[A-HJ-Y][A-HJ-Z]*)
            echo "unpacked_provisional"
            return
            ;;
        [AB][0-9][0-9][0-9]" "[A-HJ-Y][A-HJ-Z]*)
            # A-prefix format: A908 CJ, B800 AA
            echo "unpacked_provisional"
            return
            ;;
    esac

    echo "unknown"
}

#==============================================================================
# Main conversion function
#==============================================================================

convert_simple() {
    local desig="$1"
    local format
    format=$(detect_format "$desig")

    case "$format" in
        packed_permanent)
            unpack_permanent "$desig"
            ;;
        unpacked_permanent)
            pack_permanent "$desig"
            ;;
        packed_provisional)
            unpack_provisional "$desig"
            ;;
        unpacked_provisional)
            pack_provisional "$desig"
            ;;
        packed_survey)
            unpack_survey "$desig"
            ;;
        unpacked_survey)
            pack_survey "$desig"
            ;;
        packed_numbered_comet)
            unpack_numbered_comet "$desig"
            ;;
        unpacked_numbered_comet)
            pack_numbered_comet "$desig"
            ;;
        packed_provisional_comet)
            unpack_provisional_comet "$desig"
            ;;
        unpacked_provisional_comet)
            pack_provisional_comet "$desig"
            ;;
        packed_satellite)
            unpack_satellite "$desig"
            ;;
        unpacked_satellite)
            pack_satellite "$desig"
            ;;
        *)
            echo "ERROR: Unable to detect format: $desig" >&2
            return 1
            ;;
    esac
}

#==============================================================================
# Helper functions for format conversion and fragment handling
#==============================================================================

# Convert minimal packed format to 12-character MPC report format
to_report_format() {
    local minimal="$1"
    local len="${#minimal}"

    # Numbered comet: 0001P -> "0001P       "
    # Numbered comet with fragment: 0073Pa -> "0073P      a", 0073Paa -> "0073P     aa"
    if [ "$len" -ge 5 ] && [ "$len" -le 7 ]; then
        local c5="${minimal:4:1}"
        case "$c5" in
            [PD])
                if is_numeric "${minimal:0:4}"; then
                    local base="${minimal:0:5}"
                    if [ "$len" -eq 5 ]; then
                        printf "%s       \n" "$base"
                    elif [ "$len" -eq 6 ]; then
                        local frag="${minimal:5:1}"
                        printf "%s      %s\n" "$base" "$frag"
                    elif [ "$len" -eq 7 ]; then
                        local frag="${minimal:5:2}"
                        printf "%s     %s\n" "$base" "$frag"
                    fi
                    return
                fi
                ;;
        esac
    fi

    # Other formats: right-align to 12 chars
    printf "%12s\n" "$minimal"
}

# Convert 12-character report format to minimal packed format
from_report_format() {
    local report="$1"

    # Check for numbered comet format (digits + P/D in positions 0-4)
    local first5="${report:0:5}"
    local c5="${report:4:1}"
    if is_numeric "${report:0:4}" 2>/dev/null; then
        case "$c5" in
            [PD])
                # Get fragment from positions 10-11, stripping spaces
                local frag="${report:10:2}"
                frag="${frag// /}"
                if [ -n "$frag" ]; then
                    echo "${first5}${frag}"
                else
                    echo "$first5"
                fi
                return
                ;;
        esac
    fi

    # Other formats: strip leading/trailing whitespace
    local result="${report#"${report%%[![:space:]]*}"}"
    result="${result%"${result##*[![:space:]]}"}"
    echo "$result"
}

# Check if designation has a comet fragment suffix
has_fragment() {
    local desig="$1"
    local len="${#desig}"

    # Unpacked numbered comet with fragment: 73P-A, 73P-AA
    if [[ "$desig" =~ ^[0-9]+[PD]-[A-Z]{1,2}(/.*)?$ ]]; then
        return 0  # true
    fi

    # Unpacked provisional comet with fragment: C/1995 O1-A, D/1993 F2-AA
    if [[ "$desig" =~ ^[PDCXA]/[0-9]{4}\ [A-Z][0-9]*-[A-Z]{1,2}$ ]]; then
        return 0  # true
    fi

    # Packed numbered comet with fragment: 0073Pa (6), 0073Paa (7)
    if [ "$len" -ge 6 ] && [ "$len" -le 7 ]; then
        local c5="${desig:4:1}"
        case "$c5" in
            [PD])
                if is_numeric "${desig:0:4}"; then
                    local rest="${desig:5}"
                    case "$rest" in
                        [a-z]|[a-z][a-z])
                            return 0  # true
                            ;;
                    esac
                fi
                ;;
        esac
    fi

    # Packed provisional comet with fragment: DJ93F02a (8 chars ending in lowercase)
    if [ "$len" -eq 8 ]; then
        local c1="${desig:0:1}"
        local c2="${desig:1:1}"
        local last="${desig:7:1}"
        case "$c1" in
            [PDCXA])
                case "$c2" in
                    [A-L])
                        case "$last" in
                            [a-z])
                                if [ "$last" != "0" ]; then
                                    return 0  # true
                                fi
                                ;;
                        esac
                        ;;
                esac
                ;;
        esac
    fi

    # Packed provisional comet with 2-letter fragment: PJ30J01aa (9 chars)
    if [ "$len" -eq 9 ]; then
        local c1="${desig:0:1}"
        local c2="${desig:1:1}"
        local last2="${desig:7:2}"
        case "$c1" in
            [PDCXA])
                case "$c2" in
                    [A-L])
                        case "$last2" in
                            [a-z][a-z])
                                return 0  # true
                                ;;
                        esac
                        ;;
                esac
                ;;
        esac
    fi

    return 1  # false
}

# Extract fragment suffix from comet designation (uppercase, e.g., "A", "AA")
get_fragment() {
    local desig="$1"
    local len="${#desig}"

    # Unpacked numbered comet with fragment: 73P-A, 73P-AA
    if [[ "$desig" =~ ^[0-9]+[PD]-([A-Z]{1,2})(/.*)?$ ]]; then
        echo "${BASH_REMATCH[1]}"
        return
    fi

    # Unpacked provisional comet with fragment: C/1995 O1-A, D/1993 F2-AA
    if [[ "$desig" =~ ^[PDCXA]/[0-9]{4}\ [A-Z][0-9]*-([A-Z]{1,2})$ ]]; then
        echo "${BASH_REMATCH[1]}"
        return
    fi

    # Packed numbered comet with fragment: 0073Pa (6), 0073Paa (7)
    if [ "$len" -ge 6 ] && [ "$len" -le 7 ]; then
        local c5="${desig:4:1}"
        case "$c5" in
            [PD])
                if is_numeric "${desig:0:4}"; then
                    local frag="${desig:5}"
                    case "$frag" in
                        [a-z]|[a-z][a-z])
                            echo "$frag" | tr 'a-z' 'A-Z'
                            return
                            ;;
                    esac
                fi
                ;;
        esac
    fi

    # Packed provisional comet with fragment: DJ93F02a (8 chars ending in lowercase)
    if [ "$len" -eq 8 ]; then
        local c1="${desig:0:1}"
        local c2="${desig:1:1}"
        local last="${desig:7:1}"
        case "$c1" in
            [PDCXA])
                case "$c2" in
                    [A-L])
                        case "$last" in
                            [a-z])
                                if [ "$last" != "0" ]; then
                                    echo "$last" | tr 'a-z' 'A-Z'
                                    return
                                fi
                                ;;
                        esac
                        ;;
                esac
                ;;
        esac
    fi

    # Packed provisional comet with 2-letter fragment: PJ30J01aa (9 chars)
    if [ "$len" -eq 9 ]; then
        local c1="${desig:0:1}"
        local c2="${desig:1:1}"
        local last2="${desig:7:2}"
        case "$c1" in
            [PDCXA])
                case "$c2" in
                    [A-L])
                        case "$last2" in
                            [a-z][a-z])
                                echo "$last2" | tr 'a-z' 'A-Z'
                                return
                                ;;
                        esac
                        ;;
                esac
                ;;
        esac
    fi

    echo ""
}

# Get parent comet designation without fragment suffix
get_parent() {
    local desig="$1"
    local len="${#desig}"

    # Unpacked numbered comet with fragment: 73P-A -> 73P
    if [[ "$desig" =~ ^([0-9]+[PD])-[A-Z]{1,2}(/.*)?$ ]]; then
        echo "${BASH_REMATCH[1]}"
        return
    fi

    # Unpacked provisional comet with fragment: D/1993 F2-A -> D/1993 F2
    if [[ "$desig" =~ ^([PDCXA]/[0-9]{4}\ [A-Z][0-9]*)-[A-Z]{1,2}$ ]]; then
        echo "${BASH_REMATCH[1]}"
        return
    fi

    # Packed numbered comet with fragment: 0073Pa -> 0073P
    if [ "$len" -ge 6 ] && [ "$len" -le 7 ]; then
        local c5="${desig:4:1}"
        case "$c5" in
            [PD])
                if is_numeric "${desig:0:4}"; then
                    local rest="${desig:5}"
                    case "$rest" in
                        [a-z]|[a-z][a-z])
                            echo "${desig:0:5}"
                            return
                            ;;
                    esac
                fi
                ;;
        esac
    fi

    # Packed provisional comet with fragment: DJ93F02a -> DJ93F020
    if [ "$len" -eq 8 ]; then
        local c1="${desig:0:1}"
        local c2="${desig:1:1}"
        local last="${desig:7:1}"
        case "$c1" in
            [PDCXA])
                case "$c2" in
                    [A-L])
                        case "$last" in
                            [a-z])
                                if [ "$last" != "0" ]; then
                                    echo "${desig:0:7}0"
                                    return
                                fi
                                ;;
                        esac
                        ;;
                esac
                ;;
        esac
    fi

    # Packed provisional comet with 2-letter fragment: PJ30J01aa -> PJ30J010
    if [ "$len" -eq 9 ]; then
        local c1="${desig:0:1}"
        local c2="${desig:1:1}"
        local last2="${desig:7:2}"
        case "$c1" in
            [PDCXA])
                case "$c2" in
                    [A-L])
                        case "$last2" in
                            [a-z][a-z])
                                echo "${desig:0:7}0"
                                return
                                ;;
                        esac
                        ;;
                esac
                ;;
        esac
    fi

    # No fragment, return as-is
    echo "$desig"
}

# Check if two designations refer to the same object
designations_equal() {
    local d1="$1"
    local d2="$2"

    # Try to pack both to a normalized format for comparison
    local p1 p2
    local format1 format2

    format1=$(detect_format "$d1")
    format2=$(detect_format "$d2")

    # Convert both to packed format
    case "$format1" in
        packed_*)
            p1="$d1"
            ;;
        unpacked_permanent)
            p1=$(pack_permanent "$d1")
            ;;
        unpacked_provisional)
            p1=$(pack_provisional "$d1")
            ;;
        unpacked_survey)
            p1=$(pack_survey "$d1")
            ;;
        unpacked_numbered_comet)
            p1=$(pack_numbered_comet "$d1")
            ;;
        unpacked_provisional_comet)
            p1=$(pack_provisional_comet "$d1")
            ;;
        unpacked_satellite)
            p1=$(pack_satellite "$d1")
            ;;
        *)
            p1="$d1"
            ;;
    esac

    case "$format2" in
        packed_*)
            p2="$d2"
            ;;
        unpacked_permanent)
            p2=$(pack_permanent "$d2")
            ;;
        unpacked_provisional)
            p2=$(pack_provisional "$d2")
            ;;
        unpacked_survey)
            p2=$(pack_survey "$d2")
            ;;
        unpacked_numbered_comet)
            p2=$(pack_numbered_comet "$d2")
            ;;
        unpacked_provisional_comet)
            p2=$(pack_provisional_comet "$d2")
            ;;
        unpacked_satellite)
            p2=$(pack_satellite "$d2")
            ;;
        *)
            p2="$d2"
            ;;
    esac

    [ "$p1" = "$p2" ]
}

#==============================================================================
# CLI entry point
#==============================================================================

if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    if [ $# -eq 0 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        echo "Usage: $(basename "$0") <designation> [designation ...]"
        echo "Converts between packed and unpacked MPC designations."
        echo ""
        echo "Examples:"
        echo "  $(basename "$0") 00001          # -> 1"
        echo "  $(basename "$0") '1995 XA'      # -> J95X00A"
        echo "  $(basename "$0") 1P             # -> 0001P"
        exit 0
    fi

    for arg in "$@"; do
        convert_simple "$arg"
    done
fi
