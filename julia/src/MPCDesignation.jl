"""
    MPCDesignation

Convert between packed and unpacked Minor Planet Center (MPC) designations
for asteroids, comets, and natural satellites.

Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
"""
module MPCDesignation

export convert_designation, convert_simple, pack, unpack, detect_format, is_valid_designation
export pack_permanent, unpack_permanent, pack_provisional, unpack_provisional
export pack_comet_full, unpack_comet_full, pack_satellite, unpack_satellite
export MPCDesignationError, FormatInfo, ConversionResult

const VERSION = "1.0.0"

# Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
const MAX_ASTEROID_NUMBER = 15396335

# Base-62 character set
const BASE62_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

# Century codes for provisional designations
const CENTURY_CODES = Dict(
    'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
    'G' => 16, 'H' => 17, 'I' => 18, 'J' => 19, 'K' => 20, 'L' => 21
)
const REVERSE_CENTURY_CODES = Dict(v => k for (k, v) in CENTURY_CODES)

# Survey codes
const SURVEY_PACKED_TO_UNPACKED = Dict(
    "PLS" => "P-L", "T1S" => "T-1", "T2S" => "T-2", "T3S" => "T-3"
)
const SURVEY_UNPACKED_TO_PACKED = Dict(v => k for (k, v) in SURVEY_PACKED_TO_UNPACKED)

# Comet types
const COMET_TYPES = Set(['P', 'C', 'D', 'X', 'A', 'I'])
const COMET_TYPE_DESCRIPTIONS = Dict(
    'P' => "periodic", 'C' => "non-periodic", 'D' => "defunct",
    'X' => "uncertain orbit", 'A' => "asteroid with comet designation", 'I' => "interstellar"
)

# Satellite planet codes
const SATELLITE_PLANETS = Set(['J', 'S', 'U', 'N'])
const SATELLITE_PLANET_NAMES = Dict(
    'J' => "Jupiter", 'S' => "Saturn", 'U' => "Uranus", 'N' => "Neptune"
)

"""
    MPCDesignationError <: Exception

Exception raised for invalid MPC designations.
"""
struct MPCDesignationError <: Exception
    msg::String
end

Base.showerror(io::IO, e::MPCDesignationError) = print(io, "MPCDesignationError: ", e.msg)

"""
    FormatInfo

Information about a detected designation format.
"""
struct FormatInfo
    format::Symbol  # :packed or :unpacked
    type::String
    subtype::String
end

"""
    ConversionResult

Result of a designation conversion.
"""
struct ConversionResult
    input::String
    output::String
    info::FormatInfo
end

# =============================================================================
# Input validation
# =============================================================================

function validate_raw_input(s::AbstractString)
    for c in s
        code = Int(c)
        if code < 32 || code > 126
            throw(MPCDesignationError("Invalid character in designation: '\\x$(string(code, base=16, pad=2))'"))
        end
    end
end

function validate_whitespace(s::AbstractString)
    prev_space = false
    for c in s
        code = Int(c)
        if code < 32 || code > 126
            throw(MPCDesignationError("Invalid character in designation"))
        end
        if c == ' '
            if prev_space
                throw(MPCDesignationError("Consecutive spaces in designation"))
            end
            prev_space = true
        else
            prev_space = false
        end
    end
end

function sanitize(designation::AbstractString)
    validate_raw_input(designation)
    result = strip(designation)
    if isempty(result)
        throw(MPCDesignationError("Empty designation"))
    end
    return String(result)
end

is_valid_half_month(c::Char) = 'A' <= c <= 'Y' && c != 'I'

# =============================================================================
# Base-62 encoding utilities
# =============================================================================

function base62_to_num(c::Char)
    idx = findfirst(==(c), BASE62_CHARS)
    if idx === nothing
        throw(MPCDesignationError("Invalid base-62 character: $c"))
    end
    return idx - 1  # Julia is 1-indexed
end

function num_to_base62(n::Integer)
    if n < 0 || n > 61
        throw(MPCDesignationError("Number out of base-62 range: $n"))
    end
    return BASE62_CHARS[n + 1]
end

function base62_string_to_num(s::AbstractString)
    result = 0
    for c in s
        result = result * 62 + base62_to_num(c)
    end
    return result
end

function num_to_base62_string(n::Integer, width::Integer)
    result = Vector{Char}(undef, width)
    for i in width:-1:1
        result[i] = num_to_base62(n % 62)
        n ÷= 62
    end
    return String(result)
end

# =============================================================================
# Cycle count encoding for provisional designations
# =============================================================================

function decode_cycle_count(encoded::AbstractString)
    length(encoded) < 2 && throw(MPCDesignationError("Invalid cycle count encoding"))

    first = encoded[1]
    second = encoded[2]

    tens = if '0' <= first <= '9'
        Int(first - '0')
    elseif 'A' <= first <= 'Z'
        Int(first - 'A') + 10
    elseif 'a' <= first <= 'z'
        Int(first - 'a') + 36
    else
        throw(MPCDesignationError("Invalid cycle count encoding"))
    end

    if !('0' <= second <= '9')
        throw(MPCDesignationError("Invalid cycle count encoding"))
    end

    return tens * 10 + Int(second - '0')
end

function encode_cycle_count(count::Integer)
    if count < 0 || count >= 620
        throw(MPCDesignationError("Cycle count out of range (0-619): $count"))
    end

    tens = count ÷ 10
    ones = count % 10

    first = if tens < 10
        Char('0' + tens)
    elseif tens < 36
        Char('A' + tens - 10)
    else
        Char('a' + tens - 36)
    end

    return string(first, Char('0' + ones))
end

# =============================================================================
# Letter/position utilities for provisional designations
# =============================================================================

function letter_to_position(letter::Char)
    if !('A' <= letter <= 'Z')
        throw(MPCDesignationError("Invalid half-month letter: $letter"))
    end
    pos = Int(letter - 'A') + 1
    if letter > 'I'
        pos -= 1  # Skip I
    end
    return pos
end

function position_to_letter(pos::Integer)
    if pos < 1 || pos > 25
        throw(MPCDesignationError("Invalid letter position: $pos"))
    end
    p = pos
    if p >= 9
        p += 1  # Skip I
    end
    return Char('A' + p - 1)
end

# =============================================================================
# Permanent (numbered) asteroid designations
# =============================================================================

"""
    unpack_permanent(packed::AbstractString) -> Int

Unpack a permanent (numbered) asteroid designation.
"""
function unpack_permanent(packed::AbstractString)
    p = strip(packed)
    length(p) != 5 && throw(MPCDesignationError("Invalid packed permanent designation length"))

    first = p[1]

    # Tilde format (>= 620,000)
    if first == '~'
        return 620000 + base62_string_to_num(p[2:5])
    end

    # Simple numeric format (< 100,000)
    if '0' <= first <= '9'
        return parse(Int, p)
    end

    # Extended format with uppercase letter (100,000 - 359,999)
    if 'A' <= first <= 'Z'
        val = Int(first) - 55  # A=10, B=11, etc.
        rest = parse(Int, p[2:5])
        return val * 10000 + rest
    end

    # Extended format with lowercase letter (360,000 - 619,999)
    if 'a' <= first <= 'z'
        val = Int(first) - 61  # a=36, b=37, etc.
        rest = parse(Int, p[2:5])
        return val * 10000 + rest
    end

    throw(MPCDesignationError("Invalid packed permanent designation"))
end

"""
    pack_permanent(number::Integer) -> String

Pack a permanent (numbered) asteroid designation.
"""
function pack_permanent(number::Integer)
    if number < 1 || number > MAX_ASTEROID_NUMBER
        throw(MPCDesignationError("Invalid asteroid number: $number"))
    end

    if number < 100000
        return lpad(number, 5, '0')
    end

    if number < 620000
        div_val = number ÷ 10000
        mod_val = number % 10000
        letter = if div_val < 36
            Char(div_val + 55)  # A-Z
        else
            Char(div_val + 61)  # a-z
        end
        return string(letter, lpad(mod_val, 4, '0'))
    end

    # Tilde + base-62 format
    offset = number - 620000
    return "~" * num_to_base62_string(offset, 4)
end

# =============================================================================
# Standard provisional asteroid designations
# =============================================================================

"""
    unpack_provisional(packed::AbstractString) -> String

Unpack a provisional asteroid designation.
"""
function unpack_provisional(packed::AbstractString)
    p = strip(packed)

    # Check for survey designations first
    if length(p) == 7 && haskey(SURVEY_PACKED_TO_UNPACKED, p[1:3])
        survey = SURVEY_PACKED_TO_UNPACKED[p[1:3]]
        num = parse(Int, p[4:7])
        return "$num $survey"
    end

    length(p) != 7 && throw(MPCDesignationError("Invalid packed provisional designation length"))

    century = p[1]
    year = p[2:3]
    half_month = p[4]
    order_encoded = p[5:6]
    second_letter = p[7]

    # Validate century code for asteroids: must be I-L (1800-2199)
    !(century in ['I', 'J', 'K', 'L']) && throw(MPCDesignationError("Invalid century code for asteroid: $century (must be I-L)"))

    !haskey(CENTURY_CODES, century) && throw(MPCDesignationError("Invalid century code: $century"))

    full_year = "$(CENTURY_CODES[century])$year"
    year_int = parse(Int, full_year)
    order_num = decode_cycle_count(order_encoded)

    # Pre-1925: output A-prefix format (A for 1xxx, B for 2xxx)
    if year_int < 1925
        prefix = full_year[1] == '1' ? 'A' : 'B'
        year_suffix = full_year[2:end]  # e.g., "908" from "1908"
        if order_num == 0
            return "$prefix$year_suffix $half_month$second_letter"
        end
        return "$prefix$year_suffix $half_month$second_letter$order_num"
    end

    if order_num == 0
        return "$full_year $half_month$second_letter"
    end
    return "$full_year $half_month$second_letter$order_num"
end

"""
    pack_provisional(unpacked::AbstractString) -> String

Pack a provisional asteroid designation.
"""
function pack_provisional(unpacked::AbstractString)
    u = strip(unpacked)

    # Check for survey designations
    survey_match = match(r"^(\d+) (P-L|T-[123])$", u)
    if survey_match !== nothing
        number = parse(Int, survey_match.captures[1])
        survey = survey_match.captures[2]
        number < 1 && throw(MPCDesignationError("Survey number must be positive"))
        return SURVEY_UNPACKED_TO_PACKED[survey] * lpad(number, 4, '0')
    end

    # Check for old-style designation: "A908 CJ" or "B842 FA"
    old_style_match = match(r"^[AB](\d)(\d{2}) ([A-Z])([A-Z])$", u)
    if old_style_match !== nothing
        century_digit = old_style_match.captures[1][1]
        year_short = old_style_match.captures[2]
        half_month = old_style_match.captures[3][1]
        second_letter = old_style_match.captures[4][1]

        century_code = if century_digit == '8'
            'I'
        elseif century_digit == '9'
            'J'
        elseif century_digit == '0'
            'K'
        else
            throw(MPCDesignationError("Invalid century digit in old-style designation"))
        end

        return "$century_code$(year_short)$(half_month)00$second_letter"
    end

    # Match standard provisional: "1995 XA" or "1995 XA12"
    prov_match = match(r"^(\d{4}) ([A-Z])([A-Z])(\d*)$", u)
    prov_match === nothing && throw(MPCDesignationError("Invalid unpacked provisional designation: $u"))

    year = prov_match.captures[1]
    half_month = prov_match.captures[2][1]
    second_letter = prov_match.captures[3][1]
    order_str = prov_match.captures[4]

    !is_valid_half_month(half_month) && throw(MPCDesignationError("Invalid half-month letter: $half_month"))

    century = parse(Int, year[1:2])
    year_short = year[3:4]

    !haskey(REVERSE_CENTURY_CODES, century) && throw(MPCDesignationError("Invalid century in year: $year"))

    century_code = REVERSE_CENTURY_CODES[century]

    order_num = if isempty(order_str)
        0
    else
        n = tryparse(Int, order_str)
        n === nothing && throw(MPCDesignationError("Cycle count out of range (overflow): $order_str"))
        n
    end

    # Check if we need extended format
    if order_num >= 620
        return pack_extended_provisional(parse(Int, year), half_month, second_letter, order_num)
    end

    order_encoded = encode_cycle_count(order_num)
    return "$century_code$year_short$half_month$order_encoded$second_letter"
end

# =============================================================================
# Extended provisional format (cycle >= 620)
# =============================================================================

function pack_extended_provisional(year::Integer, half_month::Char, second_letter::Char, cycle::Integer)
    year_short = year % 100
    letter_pos = letter_to_position(second_letter)
    base_sequence = (cycle - 620) * 25 + letter_pos - 1
    seq_encoded = num_to_base62_string(base_sequence, 4)
    year_char = num_to_base62(year_short)
    return "_$year_char$half_month$seq_encoded"
end

"""
    unpack_extended_provisional(packed::AbstractString) -> String

Unpack an extended provisional designation (underscore format).
"""
function unpack_extended_provisional(packed::AbstractString)
    p = strip(packed)
    (length(p) != 7 || p[1] != '_') && throw(MPCDesignationError("Invalid extended packed provisional"))

    year_digit = p[2]
    half_month = p[3]
    seq_encoded = p[4:7]

    base_sequence = base62_string_to_num(seq_encoded)
    cycle = 620 + base_sequence ÷ 25
    letter_pos = (base_sequence % 25) + 1
    second_letter = position_to_letter(letter_pos)

    year_val = base62_to_num(year_digit)
    year = 2000 + year_val

    return "$year $half_month$second_letter$cycle"
end

# =============================================================================
# Comet provisional designations
# =============================================================================

"""
    unpack_comet_provisional(packed::AbstractString) -> String

Unpack a comet provisional designation.
"""
function unpack_comet_provisional(packed::AbstractString)
    p = strip(packed)
    len = length(p)

    (len != 7 && len != 8) && throw(MPCDesignationError("Invalid packed comet provisional designation length"))

    century = p[1]
    year = p[2:3]
    half_month = p[4]
    order_encoded = p[5:6]
    fragment = len == 7 ? string(p[7]) : p[7:8]

    !haskey(CENTURY_CODES, century) && throw(MPCDesignationError("Invalid century code: $century"))

    full_year = "$(CENTURY_CODES[century])$year"
    order_num = decode_cycle_count(order_encoded)

    result = "$full_year $half_month$order_num"
    if fragment != "0"
        result *= "-" * uppercase(fragment)
    end

    return result
end

"""
    pack_comet_provisional(unpacked::AbstractString) -> String

Pack a comet provisional designation.
"""
function pack_comet_provisional(unpacked::AbstractString)
    u = strip(unpacked)

    # Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
    m = match(r"^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$", u)
    m === nothing && throw(MPCDesignationError("Invalid unpacked comet provisional designation: $u"))

    year = m.captures[1]
    half_month = m.captures[2][1]
    order_str = m.captures[3]
    fragment = m.captures[4]

    order_num = tryparse(Int, order_str)
    order_num === nothing && throw(MPCDesignationError("Comet order number out of range (overflow): $order_str"))
    order_num < 1 && throw(MPCDesignationError("Comet order number must be positive"))

    century = parse(Int, year[1:2])
    year_short = year[3:4]

    !haskey(REVERSE_CENTURY_CODES, century) && throw(MPCDesignationError("Invalid century in year: $year"))

    century_code = REVERSE_CENTURY_CODES[century]
    order_encoded = encode_cycle_count(order_num)
    fragment_code = fragment === nothing ? "0" : lowercase(fragment)

    return "$century_code$year_short$half_month$order_encoded$fragment_code"
end

# =============================================================================
# Numbered comet designations
# =============================================================================

"""
    unpack_comet_numbered(packed::AbstractString) -> String

Unpack a numbered periodic comet designation.
"""
function unpack_comet_numbered(packed::AbstractString)
    p = strip(packed)

    m = match(r"^(\d{4})([PD])([a-z]{1,2})?$", p)
    m === nothing && throw(MPCDesignationError("Invalid packed numbered comet designation"))

    number = parse(Int, m.captures[1])
    comet_type = m.captures[2]

    # Check for fragment
    if m.captures[3] !== nothing
        fragment = uppercase(m.captures[3])
        return "$number$comet_type-$fragment"
    end

    return "$number$comet_type"
end

"""
    pack_comet_numbered(unpacked::AbstractString) -> String

Pack a numbered periodic comet designation.
"""
function pack_comet_numbered(unpacked::AbstractString)
    u = strip(unpacked)

    # Match "1P" or "354P" or "73P-A" or "73P-AA" or "1P/Halley"
    m = match(r"^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$", u)
    m === nothing && throw(MPCDesignationError("Invalid unpacked numbered comet designation"))

    number = parse(Int, m.captures[1])
    comet_type = m.captures[2]

    (number < 1 || number > 9999) && throw(MPCDesignationError("Comet number out of range (1-9999): $number"))

    # Check for fragment
    if m.captures[3] !== nothing
        fragment = lowercase(m.captures[3])
        return lpad(number, 4, '0') * comet_type * fragment
    end

    return lpad(number, 4, '0') * comet_type
end

# =============================================================================
# Natural satellite designations
# =============================================================================

"""
    unpack_satellite(packed::AbstractString) -> String

Unpack a natural satellite provisional designation.
"""
function unpack_satellite(packed::AbstractString)
    p = strip(packed)

    (length(p) != 8 || p[1] != 'S') && throw(MPCDesignationError("Invalid packed satellite designation"))

    century = p[2]
    year = p[3:4]
    planet = p[5]
    number_encoded = p[6:7]

    !haskey(CENTURY_CODES, century) && throw(MPCDesignationError("Invalid century code: $century"))
    !(planet in SATELLITE_PLANETS) && throw(MPCDesignationError("Invalid planet code: $planet"))

    full_year = "$(CENTURY_CODES[century])$year"
    number = decode_cycle_count(number_encoded)

    return "S/$full_year $planet $number"
end

"""
    pack_satellite(unpacked::AbstractString) -> String

Pack a natural satellite provisional designation.
"""
function pack_satellite(unpacked::AbstractString)
    u = strip(unpacked)

    m = match(r"^S/(\d{4}) ([JSUN]) (\d+)$", u)
    m === nothing && throw(MPCDesignationError("Invalid unpacked satellite designation"))

    year = m.captures[1]
    planet = m.captures[2][1]
    number = parse(Int, m.captures[3])

    number < 1 && throw(MPCDesignationError("Satellite number must be positive"))

    century = parse(Int, year[1:2])
    year_short = year[3:4]

    !haskey(REVERSE_CENTURY_CODES, century) && throw(MPCDesignationError("Invalid century in year: $year"))

    century_code = REVERSE_CENTURY_CODES[century]
    number_encoded = encode_cycle_count(number)

    return "S$century_code$year_short$planet$(number_encoded)0"
end

# =============================================================================
# BCE year encoding for ancient comets
# =============================================================================

function encode_bce_year(year::Integer)
    year >= 0 && throw(MPCDesignationError("Not a BCE year: $year"))

    abs_year = abs(year)
    code = 99 - (abs_year % 100)

    prefix = if abs_year < 100
        '/'
    elseif abs_year < 200
        '.'
    elseif abs_year < 300
        '-'
    else
        throw(MPCDesignationError("BCE year out of supported range: $year"))
    end

    return prefix, lpad(code, 2, '0')
end

function decode_bce_year(prefix::Char, code::AbstractString)
    code_num = parse(Int, code)
    year_part = 99 - code_num

    return if prefix == '/'
        -year_part
    elseif prefix == '.'
        -(year_part + 100)
    elseif prefix == '-'
        -(year_part + 200)
    else
        throw(MPCDesignationError("Invalid BCE prefix: $prefix"))
    end
end

# =============================================================================
# Ancient/BCE comet provisional designations
# =============================================================================

function pack_ancient_comet_provisional(comet_type::Char, year::Integer, half_month::Char,
                                         order_num::Integer, fragment::AbstractString)
    order_encoded = encode_cycle_count(order_num)
    fragment_code = isempty(fragment) ? "0" : lowercase(fragment)

    if year < 0
        prefix, code = encode_bce_year(year)
        return "$comet_type$prefix$code$half_month$order_encoded$fragment_code"
    end

    return "$comet_type$(lpad(year, 3, '0'))$half_month$order_encoded$fragment_code"
end

"""
    unpack_ancient_comet_provisional(packed::AbstractString) -> String

Unpack an ancient or BCE comet provisional designation.
"""
function unpack_ancient_comet_provisional(packed::AbstractString)
    p = strip(packed)

    length(p) != 8 && throw(MPCDesignationError("Invalid ancient comet designation length"))

    comet_type = p[1]
    !(comet_type in COMET_TYPES) && throw(MPCDesignationError("Invalid comet type: $comet_type"))

    year, half_month, order_encoded, fragment = if p[2] in ['/', '.', '-']
        decode_bce_year(p[2], p[3:4]), p[5], p[6:7], p[8]
    else
        parse(Int, p[2:4]), p[5], p[6:7], p[8]
    end

    order_num = decode_cycle_count(order_encoded)
    result = "$comet_type/$year $half_month$order_num"

    if fragment != '0'
        result *= "-" * uppercase(string(fragment))
    end

    return result
end

# =============================================================================
# Helper functions for comet format detection
# =============================================================================

function is_asteroid_style_packed(provisional_part::AbstractString)
    length(provisional_part) != 7 && return false
    last_char = provisional_part[7]
    return 'A' <= last_char <= 'Z'
end

function is_asteroid_style_unpacked(provisional::AbstractString)
    m = match(r"^\d{4} ([A-Z])(.)", provisional)
    m === nothing && return false
    second_char = m.captures[2][1]
    return isletter(second_char)
end

# =============================================================================
# Full comet designations (with type prefix)
# =============================================================================

"""
    unpack_comet_full(packed::AbstractString) -> String

Unpack a full comet designation (type + provisional or numbered).
"""
function unpack_comet_full(packed::AbstractString)
    p = packed
    len = length(p)

    if len == 8
        comet_type = p[1]
        provisional_part = p[2:8]

        !(comet_type in COMET_TYPES) && throw(MPCDesignationError("Invalid comet type: $comet_type"))

        provisional = if is_asteroid_style_packed(provisional_part)
            unpack_provisional(provisional_part)
        else
            unpack_comet_provisional(provisional_part)
        end

        return "$comet_type/$provisional"
    end

    if len == 9
        comet_type = p[1]
        provisional_part = p[2:9]

        !(comet_type in COMET_TYPES) && throw(MPCDesignationError("Invalid comet type: $comet_type"))

        provisional = unpack_comet_provisional(provisional_part)
        return "$comet_type/$provisional"
    end

    if len == 12 || (len < 12 && p[1] == ' ')
        while length(p) < 12
            p = " " * p
        end

        num_part = strip(p[1:4])
        comet_type = p[5]
        provisional_part = p[6:12]

        !(comet_type in COMET_TYPES) && throw(MPCDesignationError("Invalid comet type: $comet_type"))

        provisional = if is_asteroid_style_packed(provisional_part)
            unpack_provisional(provisional_part)
        else
            unpack_comet_provisional(provisional_part)
        end

        if isempty(num_part)
            return "$comet_type/$provisional"
        end

        num = parse(Int, num_part)
        return "$num$comet_type/$provisional"
    end

    throw(MPCDesignationError("Invalid packed full comet designation length"))
end

"""
    pack_comet_full(unpacked::AbstractString) -> String

Pack a full comet designation.
"""
function pack_comet_full(unpacked::AbstractString)
    u = strip(unpacked)

    m = match(r"^(\d*)([PCDXAI])/(-?\d+) (.+)$", u)
    m === nothing && throw(MPCDesignationError("Invalid unpacked comet designation: $u"))

    number_str = m.captures[1]
    comet_type = m.captures[2][1]
    year = parse(Int, m.captures[3])
    prov_part = m.captures[4]

    # Check for ancient or BCE year
    if year < 1000
        ancient_m = match(r"^([A-Z])(\d+)(?:-([A-Z]))?$", prov_part)
        if ancient_m !== nothing
            half_month = ancient_m.captures[1][1]
            order_num = parse(Int, ancient_m.captures[2])
            fragment = ancient_m.captures[3]
            return pack_ancient_comet_provisional(comet_type, year, half_month, order_num,
                                                   fragment === nothing ? "" : fragment)
        else
            throw(MPCDesignationError("Invalid ancient comet provisional: $prov_part"))
        end
    end

    # Modern comet
    provisional = "$year $prov_part"

    provisional_packed = if is_asteroid_style_unpacked(provisional)
        pack_provisional(provisional)
    else
        pack_comet_provisional(provisional)
    end

    if isempty(number_str)
        return "$comet_type$provisional_packed"
    end

    num = parse(Int, number_str)
    (num < 1 || num > 9999) && throw(MPCDesignationError("Comet number out of range (1-9999): $num"))

    return lpad(num, 4, '0') * "$comet_type$provisional_packed"
end

# =============================================================================
# Format detection
# =============================================================================

is_all_digits(s::AbstractString) = !isempty(s) && all(c -> '0' <= c <= '9', s)
is_base62(c::Char) = ('0' <= c <= '9') || ('A' <= c <= 'Z') || ('a' <= c <= 'z')
is_digit_or_lower(c::Char) = ('0' <= c <= '9') || ('a' <= c <= 'z')

"""
    detect_format(designation::AbstractString) -> FormatInfo

Detect if a designation is packed or unpacked and what type it is.
"""
function detect_format(designation::AbstractString)
    # Validate raw input BEFORE trimming
    validate_raw_input(designation)

    # Check for packed full comet designation BEFORE trimming (12 chars with spaces)
    if length(designation) == 12
        if match(r"^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation) !== nothing
            return FormatInfo(:packed, "comet_full", "comet with provisional designation (12-char)")
        end
    end

    # Check for packed comet designation (8 chars)
    if length(designation) == 8 && designation[1] in COMET_TYPES
        if match(r"^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$", designation) !== nothing
            return FormatInfo(:packed, "comet_full", "comet with provisional designation (8-char)")
        end
    end

    # Check for packed comet with 2-letter fragment (9 chars)
    if length(designation) == 9 && designation[1] in COMET_TYPES
        if match(r"^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$", designation) !== nothing
            return FormatInfo(:packed, "comet_full", "comet with provisional designation (9-char, 2-letter fragment)")
        end
    end

    # Check for packed ancient comet (8 chars)
    if length(designation) == 8 && designation[1] in COMET_TYPES
        if match(r"^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation) !== nothing
            return FormatInfo(:packed, "comet_ancient", "comet with ancient provisional (year < 1000)")
        end
    end

    # Check for packed BCE comet (8 chars)
    if length(designation) == 8 && designation[1] in COMET_TYPES
        if match(r"^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation) !== nothing
            return FormatInfo(:packed, "comet_bce", "comet with BCE provisional")
        end
    end

    des = strip(designation)

    # Validate whitespace
    validate_whitespace(des)

    # Check for packed satellite designation (8 chars starting with S)
    if length(des) == 8 && des[1] == 'S'
        if match(r"^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$", des) !== nothing
            planet = des[5]
            planet_name = get(SATELLITE_PLANET_NAMES, planet, string(planet))
            return FormatInfo(:packed, "satellite", "natural satellite ($planet_name)")
        end
    end

    # Check for packed permanent (numbered) asteroid
    if length(des) == 5
        if des[1] == '~'
            if match(r"^~[0-9A-Za-z]{4}$", des) !== nothing
                return FormatInfo(:packed, "permanent", "permanent numbered (tilde/base-62, >= 620000)")
            end
        elseif is_all_digits(des)
            return FormatInfo(:packed, "permanent", "permanent numbered (5-digit, < 100000)")
        elseif match(r"^[A-Za-z][0-9]{4}$", des) !== nothing
            subtype = if isuppercase(des[1])
                "permanent numbered (letter-prefix, 100000-359999)"
            else
                "permanent numbered (letter-prefix, 360000-619999)"
            end
            return FormatInfo(:packed, "permanent", subtype)
        end

        # Check for packed numbered comet
        if match(r"^[0-9]{4}[PD]$", des) !== nothing
            comet_type = des[5]
            type_desc = get(COMET_TYPE_DESCRIPTIONS, comet_type, string(comet_type))
            return FormatInfo(:packed, "comet_numbered", "comet numbered $type_desc")
        end
    end

    # Check for packed numbered comet with fragment (6-7 chars)
    if length(des) == 6 || length(des) == 7
        if match(r"^[0-9]{4}[PD][a-z]{1,2}$", des) !== nothing
            comet_type = des[5]
            type_desc = get(COMET_TYPE_DESCRIPTIONS, comet_type, string(comet_type))
            return FormatInfo(:packed, "comet_numbered", "comet numbered $type_desc with fragment")
        end
    end

    # Check for packed provisional asteroid (7 chars)
    if length(des) == 7
        if des[1] == '_'
            if match(r"^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$", des) !== nothing
                return FormatInfo(:packed, "provisional_extended", "provisional (extended format, cycle >=620)")
            end
        end

        if match(r"^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$", des) !== nothing
            return FormatInfo(:packed, "provisional", "provisional")
        end

        if startswith(des, "PLS") && is_all_digits(des[4:7])
            return FormatInfo(:packed, "survey", "survey (Palomar-Leiden)")
        end

        if match(r"^T[123]S\d{4}$", des) !== nothing
            return FormatInfo(:packed, "survey", "survey (Trojan T-$(des[2]))")
        end

        if match(r"^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", des) !== nothing
            return FormatInfo(:packed, "comet_provisional", "comet provisional")
        end
    end

    # --- UNPACKED FORMATS ---

    # Check for unpacked satellite
    if match(r"^S/\d{4} ([JSUN]) \d+$", des) !== nothing
        m = match(r"^S/\d{4} ([JSUN]) \d+$", des)
        planet = m.captures[1][1]
        planet_name = get(SATELLITE_PLANET_NAMES, planet, string(planet))
        return FormatInfo(:unpacked, "satellite", "natural satellite ($planet_name)")
    end

    # Check for unpacked permanent (numbered) asteroid
    if is_all_digits(des)
        return FormatInfo(:unpacked, "permanent", "permanent numbered")
    end

    # Check for unpacked survey designation
    if match(r"^\d+ (P-L|T-[123])$", des) !== nothing
        m = match(r"^\d+ (P-L|T-[123])$", des)
        survey = m.captures[1]
        subtype = if survey == "P-L"
            "survey (Palomar-Leiden)"
        else
            "survey (Trojan $survey)"
        end
        return FormatInfo(:unpacked, "survey", subtype)
    end

    # Check for old-style asteroid designation
    if match(r"^[AB]\d{3} [A-Z][A-Z]$", des) !== nothing
        return FormatInfo(:unpacked, "provisional", "provisional (old-style pre-1925)")
    end

    # Check for unpacked provisional asteroid
    if match(r"^\d{4} [A-Z][A-Z]\d*$", des) !== nothing
        return FormatInfo(:unpacked, "provisional", "provisional")
    end

    # Check for unpacked comet with type prefix
    m = match(r"^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$", des)
    if m !== nothing
        num = m.captures[1]
        ctype = m.captures[2][1]
        year = parse(Int, m.captures[3])

        year_desc = if year < 0
            "BCE"
        elseif year < 1000
            "ancient"
        else
            ""
        end

        type_desc = get(COMET_TYPE_DESCRIPTIONS, ctype, string(ctype))

        subtype = if !isempty(num)
            isempty(year_desc) ? "comet numbered with provisional ($type_desc)" :
                                 "comet numbered with $year_desc provisional ($type_desc)"
        else
            isempty(year_desc) ? "comet provisional ($type_desc)" :
                                 "comet $year_desc provisional ($type_desc)"
        end

        return FormatInfo(:unpacked, "comet_full", subtype)
    end

    # Check for unpacked numbered periodic comet (with optional fragment)
    if match(r"^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$", des) !== nothing
        m = match(r"^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$", des)
        comet_type = m.captures[2][1]
        type_desc = get(COMET_TYPE_DESCRIPTIONS, comet_type, string(comet_type))
        has_frag = m.captures[3] !== nothing
        subtype = has_frag ? "comet numbered $type_desc with fragment" : "comet numbered $type_desc"
        return FormatInfo(:unpacked, "comet_numbered", subtype)
    end

    throw(MPCDesignationError("Unable to detect designation format: $designation"))
end

# =============================================================================
# Main conversion functions
# =============================================================================

"""
    convert_designation(designation::AbstractString) -> ConversionResult

Convert a designation between packed and unpacked formats.
Auto-detects the input format and converts to the other.
"""
function convert_designation(designation::AbstractString)
    info = detect_format(designation)

    output = if info.format == :packed
        if info.type == "permanent"
            string(unpack_permanent(designation))
        elseif info.type in ["provisional", "survey"]
            unpack_provisional(designation)
        elseif info.type == "provisional_extended"
            unpack_extended_provisional(designation)
        elseif info.type == "comet_numbered"
            unpack_comet_numbered(designation)
        elseif info.type == "comet_provisional"
            unpack_comet_provisional(designation)
        elseif info.type == "comet_full"
            unpack_comet_full(designation)
        elseif info.type in ["comet_ancient", "comet_bce"]
            unpack_ancient_comet_provisional(designation)
        elseif info.type == "satellite"
            unpack_satellite(designation)
        else
            throw(MPCDesignationError("Unknown type: $(info.type)"))
        end
    else  # unpacked
        if info.type == "permanent"
            num = tryparse(Int, strip(designation))
            num === nothing && throw(MPCDesignationError("Invalid asteroid number (overflow): $(strip(designation))"))
            (num < 1 || num > MAX_ASTEROID_NUMBER) && throw(MPCDesignationError("Invalid asteroid number: $num"))
            pack_permanent(num)
        elseif info.type in ["provisional", "survey"]
            pack_provisional(designation)
        elseif info.type == "comet_numbered"
            pack_comet_numbered(designation)
        elseif info.type == "comet_full"
            pack_comet_full(designation)
        elseif info.type == "satellite"
            pack_satellite(designation)
        else
            throw(MPCDesignationError("Unknown type: $(info.type)"))
        end
    end

    return ConversionResult(designation, output, info)
end

"""
    convert_simple(designation::AbstractString) -> String

Convert a designation and return just the output string.
"""
convert_simple(designation::AbstractString) = convert_designation(designation).output

"""
    pack(designation::AbstractString) -> String

Ensure a designation is in packed format.
"""
function pack(designation::AbstractString)
    info = detect_format(designation)
    if info.format == :packed
        return strip(designation)
    end
    return convert_designation(designation).output
end

"""
    unpack(designation::AbstractString) -> String

Ensure a designation is in unpacked (human-readable) format.
"""
function unpack(designation::AbstractString)
    info = detect_format(designation)
    if info.format == :unpacked
        return strip(designation)
    end
    return convert_designation(designation).output
end

"""
    is_valid_designation(designation::AbstractString) -> Bool

Check if a string is a valid MPC designation.
"""
function is_valid_designation(designation::AbstractString)
    isempty(designation) && return false
    try
        detect_format(designation)
        return true
    catch
        return false
    end
end

# =============================================================================
# Helper functions
# =============================================================================

export to_report_format, from_report_format, has_fragment, get_fragment, get_parent, designations_equal

"""
    to_report_format(minimal::AbstractString) -> String

Convert minimal packed format to 12-character MPC observation report format.
"""
function to_report_format(minimal::AbstractString)
    m = strip(minimal)
    len = length(m)

    # Check if it's a numbered comet (5-7 chars with P or D at position 5)
    if 5 <= len <= 7 && all(c -> '0' <= c <= '9', m[1:4]) && m[5] in ['P', 'D']
        prefix = m[1:5]  # e.g., "0073P"
        if len == 5
            return prefix * "       "  # 7 spaces
        elseif len == 6
            return prefix * "      " * string(m[6])  # 6 spaces + 1 char
        else  # len == 7
            return prefix * "     " * m[6:7]  # 5 spaces + 2 chars
        end
    end

    # All other designations: right-align in 12 characters
    len > 12 && throw(MPCDesignationError("Designation too long for report format: $minimal"))
    return lpad(m, 12)
end

"""
    from_report_format(report::AbstractString) -> String

Convert 12-character MPC report format to minimal packed format.
"""
function from_report_format(report::AbstractString)
    length(report) != 12 && throw(MPCDesignationError("Report format must be 12 characters: $report"))

    # Check if it's a numbered comet (first 4 chars are digits, 5th is P or D)
    if all(c -> '0' <= c <= '9', report[1:4]) && report[5] in ['P', 'D']
        prefix = report[1:5]
        suffix = strip(report[6:12])
        return isempty(suffix) ? prefix : prefix * suffix
    end

    # All other formats: just trim
    return strip(report)
end

"""
    has_fragment(designation::AbstractString) -> Bool

Check if a designation has a comet fragment suffix.
"""
function has_fragment(designation::AbstractString)
    d = strip(designation)

    # Unpacked numbered comet with fragment: "73P-A", "73P-AA"
    match(r"^(\d+)([PD])-([A-Z]{1,2})(?:/.*)?$", d) !== nothing && return true

    # Packed numbered comet with fragment: "0073Pa", "0073Paa"
    match(r"^(\d{4})([PD])([a-z]{1,2})$", d) !== nothing && return true

    # Unpacked provisional comet with fragment: "D/1993 F2-A"
    match(r"^(\d*)([PCDXAI])/(.+)-([A-Z]{1,2})$", d) !== nothing && return true

    # Packed provisional comet with fragment: "DJ93F02a" (8 chars, ends with lowercase)
    if length(d) == 8 && d[1] in COMET_TYPES && islowercase(d[8]) && d[8] != '0'
        return true
    end

    # Packed provisional comet with 2-letter fragment: "DJ93F02aa"
    if length(d) == 9 && d[1] in COMET_TYPES && islowercase(d[8]) && islowercase(d[9])
        return true
    end

    return false
end

"""
    get_fragment(designation::AbstractString) -> String

Extract fragment suffix from a comet designation (returns uppercase).
"""
function get_fragment(designation::AbstractString)
    d = strip(designation)

    # Unpacked numbered comet with fragment: "73P-A", "73P-AA"
    m = match(r"^(\d+)([PD])-([A-Z]{1,2})(?:/.*)?$", d)
    m !== nothing && return m.captures[3]

    # Packed numbered comet with fragment: "0073Pa", "0073Paa"
    m = match(r"^(\d{4})([PD])([a-z]{1,2})$", d)
    m !== nothing && return uppercase(m.captures[3])

    # Unpacked provisional comet with fragment: "D/1993 F2-A"
    m = match(r"^(\d*)([PCDXAI])/(.+)-([A-Z]{1,2})$", d)
    m !== nothing && return m.captures[4]

    # Packed provisional comet with fragment: "DJ93F02a" (8 chars)
    if length(d) == 8 && d[1] in COMET_TYPES && islowercase(d[8]) && d[8] != '0'
        return uppercase(string(d[8]))
    end

    # Packed provisional comet with 2-letter fragment: "DJ93F02aa"
    if length(d) == 9 && d[1] in COMET_TYPES && islowercase(d[8]) && islowercase(d[9])
        return uppercase(d[8:9])
    end

    return ""
end

"""
    get_parent(designation::AbstractString) -> String

Get parent comet designation without fragment suffix.
"""
function get_parent(designation::AbstractString)
    d = strip(designation)

    # Unpacked numbered comet with fragment: "73P-A" -> "73P"
    m = match(r"^(\d+)([PD])-([A-Z]{1,2})(?:/.*)?$", d)
    m !== nothing && return m.captures[1] * m.captures[2]

    # Packed numbered comet with fragment: "0073Pa" -> "0073P"
    m = match(r"^(\d{4})([PD])([a-z]{1,2})$", d)
    m !== nothing && return m.captures[1] * m.captures[2]

    # Unpacked provisional comet with fragment: "D/1993 F2-A" -> "D/1993 F2"
    m = match(r"^(\d*)([PCDXAI])/(.+)-([A-Z]{1,2})$", d)
    m !== nothing && return m.captures[1] * m.captures[2] * "/" * m.captures[3]

    # Packed provisional comet with fragment: "DJ93F02a" -> "DJ93F020"
    if length(d) == 8 && d[1] in COMET_TYPES && islowercase(d[8]) && d[8] != '0'
        return d[1:7] * "0"
    end

    # Packed provisional comet with 2-letter fragment: "DJ93F02aa" -> "DJ93F020"
    if length(d) == 9 && d[1] in COMET_TYPES && islowercase(d[8]) && islowercase(d[9])
        return d[1:7] * "0"
    end

    # No fragment, return as-is
    return d
end

"""
    designations_equal(d1::AbstractString, d2::AbstractString) -> Bool

Check if two designations refer to the same object (compares packed forms).
"""
function designations_equal(d1::AbstractString, d2::AbstractString)
    try
        packed1 = pack(d1)
        packed2 = pack(d2)
        return packed1 == packed2
    catch
        return false
    end
end

end # module
