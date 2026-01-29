#!/usr/bin/env python3
"""
mpc_designation.py - Convert between packed and unpacked MPC designations

Based on Minor Planet Center specifications:
https://www.minorplanetcenter.net/iau/info/PackedDes.html

Supports asteroids, comets, and natural satellites.
"""

import re
import sys
from typing import Optional, Tuple, Dict, Any

# Base-62 character set: 0-9, A-Z, a-z
BASE62_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

# Century codes for provisional designations
# A=10 (1000s), B=11 (1100s), ... K=20 (2000s), L=21 (2100s)
CENTURY_CODES = {
    'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15,
    'G': 16, 'H': 17, 'I': 18, 'J': 19, 'K': 20, 'L': 21
}
REVERSE_CENTURY_CODES = {v: k for k, v in CENTURY_CODES.items()}

# Survey codes mapping
SURVEY_PACKED_TO_UNPACKED = {
    'PLS': 'P-L',
    'T1S': 'T-1',
    'T2S': 'T-2',
    'T3S': 'T-3'
}
SURVEY_UNPACKED_TO_PACKED = {v: k for k, v in SURVEY_PACKED_TO_UNPACKED.items()}

# Valid comet type prefixes
COMET_TYPES = {'P', 'C', 'D', 'X', 'A', 'I'}

# Human-readable descriptions of comet types
COMET_TYPE_DESCRIPTIONS = {
    'P': "periodic",
    'C': "non-periodic",
    'D': "defunct",
    'X': "uncertain orbit",
    'A': "asteroid with comet designation",
    'I': "interstellar"
}

# Planet codes for natural satellite designations
SATELLITE_PLANETS = {'J', 'S', 'U', 'N'}
SATELLITE_PLANET_NAMES = {
    'J': "Jupiter",
    'S': "Saturn",
    'U': "Uranus",
    'N': "Neptune"
}


class MPCDesignationError(Exception):
    """Exception raised for invalid MPC designations."""
    pass


# =============================================================================
# Base-62 encoding utilities
# =============================================================================

def base62_to_num(char: str) -> int:
    """Convert a single base-62 character to its numeric value."""
    idx = BASE62_CHARS.find(char)
    if idx < 0:
        raise MPCDesignationError(f"Invalid base-62 character: {char}")
    return idx


def num_to_base62(num: int) -> str:
    """Convert a numeric value (0-61) to its base-62 character."""
    if num < 0 or num > 61:
        raise MPCDesignationError(f"Number out of base-62 range: {num}")
    return BASE62_CHARS[num]


def base62_string_to_num(s: str) -> int:
    """Convert a base-62 string to a number."""
    result = 0
    for char in s:
        result = result * 62 + base62_to_num(char)
    return result


def num_to_base62_string(num: int, width: int = 4) -> str:
    """Convert a number to a base-62 string of specified width."""
    result = []
    for _ in range(width):
        result.append(num_to_base62(num % 62))
        num //= 62
    return ''.join(reversed(result))


# =============================================================================
# Permanent (numbered) asteroid designations
# =============================================================================

def unpack_permanent(packed: str) -> int:
    """
    Unpack a permanent (numbered) asteroid designation.
    Input: 5 or 6 character packed format
    Output: integer number
    """
    packed = packed.strip()
    length = len(packed)
    first = packed[0]

    # Check for tilde format first (>= 620,000) - ~ plus 4 base-62 characters
    if first == '~' and length == 5:
        base62_part = packed[1:5]
        return 620000 + base62_string_to_num(base62_part)

    if length != 5:
        raise MPCDesignationError(f"Invalid packed permanent designation length: {packed}")

    rest = packed[1:5]

    if first.isdigit():
        # Simple numeric format (< 100,000)
        return int(packed)
    elif 'A' <= first <= 'Z':
        # Extended format with uppercase letter (100,000 - 359,999)
        val = ord(first) - 55  # A=10, B=11, etc.
        return val * 10000 + int(rest)
    elif 'a' <= first <= 'z':
        # Extended format with lowercase letter (360,000 - 619,999)
        val = ord(first) - 61  # a=36, b=37, etc.
        return val * 10000 + int(rest)
    else:
        raise MPCDesignationError(f"Invalid packed permanent designation: {packed}")


def pack_permanent(number: int) -> str:
    """
    Pack a permanent (numbered) asteroid designation.
    Input: integer number
    Output: 5 or 6 character packed format
    """
    if not isinstance(number, int) or number < 1:
        raise MPCDesignationError(f"Invalid asteroid number: {number}")

    if number < 100000:
        # Simple numeric format
        return f"{number:05d}"
    elif number < 620000:
        # Extended letter format
        div = number // 10000
        mod = number % 10000
        if div < 36:
            # A-Z for 10-35
            letter = chr(div + 55)
        else:
            # a-z for 36-61
            letter = chr(div + 61)
        return f"{letter}{mod:04d}"
    else:
        # Tilde + base-62 format
        offset = number - 620000
        return "~" + num_to_base62_string(offset)


# =============================================================================
# Cycle count encoding for provisional designations
# =============================================================================

def decode_cycle_count(encoded: str) -> int:
    """
    Decode the cycle count from packed provisional format.
    Input: 2-character string (columns 5-6 of packed format)
    Output: integer cycle count
    """
    first = encoded[0]
    second = encoded[1]

    if first.isdigit():
        tens = int(first)
    elif 'A' <= first <= 'Z':
        tens = ord(first) - 55  # A=10, B=11, ..., Z=35
    elif 'a' <= first <= 'z':
        tens = ord(first) - 61  # a=36, b=37, ..., z=61
    else:
        raise MPCDesignationError(f"Invalid cycle count encoding: {encoded}")

    if not second.isdigit():
        raise MPCDesignationError(f"Invalid cycle count encoding: {encoded}")

    return tens * 10 + int(second)


def encode_cycle_count(count: int) -> str:
    """
    Encode a cycle count for packed provisional format.
    Input: integer cycle count (0-619)
    Output: 2-character encoded string
    """
    if count < 0 or count >= 620:
        raise MPCDesignationError(f"Cycle count out of range (0-619): {count}")

    tens = count // 10
    ones = count % 10

    if tens < 10:
        first = str(tens)
    elif tens < 36:
        first = chr(tens + 55)  # A-Z
    else:
        first = chr(tens + 61)  # a-z

    return f"{first}{ones}"


# =============================================================================
# Letter/position utilities for provisional designations
# =============================================================================

def letter_to_position(letter: str) -> int:
    """Convert a half-month letter to its position (A=1, B=2, ..., skipping I)."""
    if not letter or len(letter) != 1:
        raise MPCDesignationError(f"Invalid half-month letter: {letter}")

    pos = ord(letter.upper()) - ord('A') + 1
    if letter.upper() > 'I':
        pos -= 1  # Skip I
    return pos


def position_to_letter(pos: int) -> str:
    """Convert a position to half-month letter (1=A, 2=B, ..., skipping I)."""
    if pos < 1 or pos > 24:
        raise MPCDesignationError(f"Invalid half-month position: {pos}")

    if pos >= 9:
        pos += 1  # Skip I
    return chr(ord('A') + pos - 1)


def cycle_letter_to_sequence(cycle: int, letter: str) -> int:
    """Convert cycle count and second letter to sequence number."""
    return cycle * 25 + letter_to_position(letter)


def sequence_to_cycle_letter(sequence: int) -> Tuple[int, str]:
    """Convert sequence number to cycle count and second letter."""
    cycle = (sequence - 1) // 25
    pos = ((sequence - 1) % 25) + 1
    return cycle, position_to_letter(pos)


# =============================================================================
# Extended provisional format (cycle >= 620)
# =============================================================================

def needs_extended_format(cycle: int) -> bool:
    """Check if a cycle count requires the extended underscore format."""
    return cycle >= 620


def pack_extended_provisional(year: int, half_month: str, second_letter: str, cycle: int) -> str:
    """
    Pack a provisional designation using extended format (underscore prefix).
    Used when cycle count >= 620.
    Format: _YHbbbb where Y=base-62 encoded 2-digit year, H=half-month, bbbb=4-char base-62 sequence
    """
    year_short = year % 100  # 2-digit year, encoded as base-62 character

    # Calculate the sequence number within the extended range
    # Cycle 620 letter A = sequence 0, cycle 620 letter B = sequence 1, etc.
    base_sequence = (cycle - 620) * 25 + letter_to_position(second_letter) - 1

    # Encode as 4-character base-62
    seq_encoded = num_to_base62_string(base_sequence, 4)

    return f"_{num_to_base62(year_short)}{half_month}{seq_encoded}"


def unpack_extended_provisional(packed: str) -> str:
    """
    Unpack an extended provisional designation (underscore format).
    Input: 7-character packed format starting with underscore
    Output: unpacked format like "2024 AA620"
    """
    packed = packed.strip()
    if len(packed) != 7 or packed[0] != '_':
        raise MPCDesignationError(f"Invalid extended packed provisional: {packed}")

    year_digit = packed[1]
    half_month = packed[2]
    seq_encoded = packed[3:7]

    # Decode the base-62 sequence
    base_sequence = base62_string_to_num(seq_encoded)

    # Convert sequence to cycle and letter
    # sequence 0 = cycle 620 letter A, sequence 1 = cycle 620 letter B, etc.
    cycle = 620 + base_sequence // 25
    letter_pos = (base_sequence % 25) + 1
    second_letter = position_to_letter(letter_pos)

    # We need to determine the full year - this is ambiguous with just one digit
    # Use current decade assumption (2020s) but this may need context
    # For now, assume 2020s decade
    year = 2020 + int(year_digit)
    if year > 2029:
        year -= 10

    return f"{year} {half_month}{second_letter}{cycle}"


# =============================================================================
# Standard provisional asteroid designations
# =============================================================================

def unpack_provisional(packed: str) -> str:
    """
    Unpack a provisional asteroid designation.
    Input: 7-character packed format
    Output: unpacked format like "1995 XA" or "2024 AB12"
    """
    packed = packed.strip()

    # Check for survey designations first
    if len(packed) == 7 and packed[0:3] in SURVEY_PACKED_TO_UNPACKED:
        survey = SURVEY_PACKED_TO_UNPACKED[packed[0:3]]
        number = packed[3:7]
        return f"{int(number)} {survey}"

    if len(packed) != 7:
        raise MPCDesignationError(f"Invalid packed provisional designation length: {packed}")

    century = packed[0]
    year = packed[1:3]
    half_month = packed[3]
    order_encoded = packed[4:6]
    second_letter = packed[6]

    if century not in CENTURY_CODES:
        raise MPCDesignationError(f"Invalid century code: {century}")

    full_year = f"{CENTURY_CODES[century]}{year}"
    order_num = decode_cycle_count(order_encoded)

    if order_num == 0:
        return f"{full_year} {half_month}{second_letter}"
    else:
        return f"{full_year} {half_month}{second_letter}{order_num}"


def pack_provisional(unpacked: str) -> str:
    """
    Pack a provisional asteroid designation.
    Input: unpacked format like "1995 XA" or "2024 AB12"
    Output: 7-character packed format
    """
    unpacked = unpacked.strip()

    # Check for survey designations
    match = re.match(r'^(\d+)\s+(P-L|T-[123])$', unpacked)
    if match:
        number = int(match.group(1))
        survey = match.group(2)
        return f"{SURVEY_UNPACKED_TO_PACKED[survey]}{number:04d}"

    # Check for old-style designation: "A908 CJ" or "B842 FA"
    match = re.match(r'^[AB](\d)(\d{2})\s+([A-Z])([A-Z])$', unpacked)
    if match:
        century_digit = match.group(1)
        year_short = match.group(2)
        half_month = match.group(3)
        second_letter = match.group(4)

        # Convert century digit to century code: 8->18(I), 9->19(J), 0->20(K)
        if century_digit == '8':
            century_code = 'I'
        elif century_digit == '9':
            century_code = 'J'
        elif century_digit == '0':
            century_code = 'K'
        else:
            raise MPCDesignationError(f"Invalid century digit in old-style designation: {century_digit}")

        # Old-style designations always have cycle count 0
        return f"{century_code}{year_short}{half_month}00{second_letter}"

    # Match standard provisional: "1995 XA" or "1995 XA12"
    match = re.match(r'^(\d{4})\s+([A-Z])([A-Z])(\d*)$', unpacked)
    if not match:
        raise MPCDesignationError(f"Invalid unpacked provisional designation: {unpacked}")

    year = match.group(1)
    half_month = match.group(2)
    second_letter = match.group(3)
    order_str = match.group(4)

    century = year[0:2]
    year_short = year[2:4]

    if int(century) not in REVERSE_CENTURY_CODES:
        raise MPCDesignationError(f"Invalid century in year: {year}")

    century_code = REVERSE_CENTURY_CODES[int(century)]
    order_num = int(order_str) if order_str else 0

    # Check if we need extended format
    if needs_extended_format(order_num):
        return pack_extended_provisional(int(year), half_month, second_letter, order_num)

    order_encoded = encode_cycle_count(order_num)
    return f"{century_code}{year_short}{half_month}{order_encoded}{second_letter}"


# =============================================================================
# Comet provisional designations
# =============================================================================

def unpack_comet_provisional(packed: str) -> str:
    """
    Unpack a comet provisional designation.
    Input: 7 or 8 character packed format
    Output: unpacked format like "1995 O1" or "1995 O1-B"
    """
    packed = packed.strip()
    length = len(packed)

    if length not in (7, 8):
        raise MPCDesignationError(f"Invalid packed comet provisional designation length: {packed}")

    century = packed[0]
    year = packed[1:3]
    half_month = packed[3]
    order_encoded = packed[4:6]

    # Fragment: 1 char for 7-char format, 2 chars for 8-char format
    if length == 7:
        fragment = packed[6]
    else:
        fragment = packed[6:8]

    if century not in CENTURY_CODES:
        raise MPCDesignationError(f"Invalid century code: {century}")

    full_year = f"{CENTURY_CODES[century]}{year}"
    order_num = decode_cycle_count(order_encoded)

    result = f"{full_year} {half_month}{order_num}"

    # Add fragment suffix if present (single char "0" means none, otherwise 1-2 letters)
    if fragment != "0":
        fragment_letter = fragment.upper()
        result += f"-{fragment_letter}"

    return result


def pack_comet_provisional(unpacked: str) -> str:
    """
    Pack a comet provisional designation.
    Input: unpacked format like "1995 O1" or "1994 P1-B" or "1930 J1-AA"
    Output: 7 or 8 character packed format
    """
    unpacked = unpacked.strip()

    # Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
    match = re.match(r'^(\d{4})\s+([A-Z])(\d+)(?:-([A-Z]{1,2}))?$', unpacked)
    if not match:
        raise MPCDesignationError(f"Invalid unpacked comet provisional designation: {unpacked}")

    year = match.group(1)
    half_month = match.group(2)
    order_num = int(match.group(3))
    fragment = match.group(4)

    century = year[0:2]
    year_short = year[2:4]

    if int(century) not in REVERSE_CENTURY_CODES:
        raise MPCDesignationError(f"Invalid century in year: {year}")

    century_code = REVERSE_CENTURY_CODES[int(century)]
    order_encoded = encode_cycle_count(order_num)

    # Fragment encoding: 0 for none, lowercase letter(s) for fragment
    if fragment is None:
        fragment_code = "0"
    else:
        fragment_code = fragment.lower()

    return f"{century_code}{year_short}{half_month}{order_encoded}{fragment_code}"


# =============================================================================
# Numbered comet designations
# =============================================================================

def unpack_comet_numbered(packed: str) -> str:
    """
    Unpack a numbered periodic comet designation.
    Input: 5-character packed format like "0001P" or "0354P"
    Output: unpacked format like "1P" or "354P"
    """
    packed = packed.strip()

    match = re.match(r'^(\d{4})([PD])$', packed)
    if not match:
        raise MPCDesignationError(f"Invalid packed numbered comet designation: {packed}")

    number = int(match.group(1))
    comet_type = match.group(2)
    return f"{number}{comet_type}"


def pack_comet_numbered(unpacked: str) -> str:
    """
    Pack a numbered periodic comet designation.
    Input: unpacked format like "1P" or "354P"
    Output: 5-character packed format like "0001P"
    """
    unpacked = unpacked.strip()

    # Match "1P" or "354P" or "1P/Halley" (with optional name after slash)
    match = re.match(r'^(\d+)([PD])(?:/[A-Za-z].*)?$', unpacked)
    if not match:
        raise MPCDesignationError(f"Invalid unpacked numbered comet designation: {unpacked}")

    number = int(match.group(1))
    comet_type = match.group(2)

    if number < 1 or number > 9999:
        raise MPCDesignationError(f"Comet number out of range (1-9999): {number}")

    return f"{number:04d}{comet_type}"


# =============================================================================
# Natural satellite designations
# =============================================================================

def unpack_satellite(packed: str) -> str:
    """
    Unpack a natural satellite provisional designation.
    Input: 8-char packed format like "SK19S220" (S + century + year + planet + number + 0)
    Output: unpacked format like "S/2019 S 22"
    """
    packed = packed.strip()

    if len(packed) != 8 or packed[0] != 'S':
        raise MPCDesignationError(f"Invalid packed satellite designation: {packed}")

    century = packed[1]
    year = packed[2:4]
    planet = packed[4]
    number_encoded = packed[5:7]

    if century not in CENTURY_CODES:
        raise MPCDesignationError(f"Invalid century code: {century}")

    if planet not in SATELLITE_PLANETS:
        raise MPCDesignationError(f"Invalid planet code: {planet}")

    full_year = f"{CENTURY_CODES[century]}{year}"
    number = decode_cycle_count(number_encoded)

    return f"S/{full_year} {planet} {number}"


def pack_satellite(unpacked: str) -> str:
    """
    Pack a natural satellite provisional designation.
    Input: unpacked format like "S/2019 S 22"
    Output: 8-char packed format like "SK19S220"
    """
    unpacked = unpacked.strip()

    match = re.match(r'^S/(\d{4})\s+([JSUN])\s+(\d+)$', unpacked)
    if not match:
        raise MPCDesignationError(f"Invalid unpacked satellite designation: {unpacked}")

    year = match.group(1)
    planet = match.group(2)
    number = int(match.group(3))

    century = year[0:2]
    year_short = year[2:4]

    if int(century) not in REVERSE_CENTURY_CODES:
        raise MPCDesignationError(f"Invalid century in year: {year}")

    century_code = REVERSE_CENTURY_CODES[int(century)]
    number_encoded = encode_cycle_count(number)

    return f"S{century_code}{year_short}{planet}{number_encoded}0"


# =============================================================================
# BCE year encoding for ancient comets
# =============================================================================

def encode_bce_year(year: int) -> Tuple[str, str]:
    """
    Encode a BCE year for packed format.
    Returns: (prefix, 2-digit code)
    Uses complement encoding: code = 99 - (abs_year % 100)
    """
    if year >= 0:
        raise MPCDesignationError(f"Not a BCE year: {year}")

    abs_year = abs(year)
    code = 99 - (abs_year % 100)

    if abs_year < 100:
        return '/', f"{code:02d}"
    elif abs_year < 200:
        return '.', f"{code:02d}"
    elif abs_year < 300:
        return '-', f"{code:02d}"
    else:
        raise MPCDesignationError(f"BCE year out of supported range: {year}")


def decode_bce_year(prefix: str, code: str) -> int:
    """
    Decode a BCE year from packed format.
    Returns: negative year value
    Uses complement decoding: year_part = 99 - code
    """
    code_num = int(code)
    year_part = 99 - code_num

    if prefix == '/':
        return -year_part
    elif prefix == '.':
        return -(year_part + 100)
    elif prefix == '-':
        return -(year_part + 200)
    else:
        raise MPCDesignationError(f"Invalid BCE prefix: {prefix}")


def is_ancient_year(year: int) -> bool:
    """Check if a year is ancient (< 1000) or BCE."""
    return year < 1000


# =============================================================================
# Ancient/BCE comet provisional designations
# =============================================================================

def pack_ancient_comet_provisional(comet_type: str, year: int, half_month: str,
                                    order_num: int, fragment: str = "") -> str:
    """
    Pack an ancient or BCE comet provisional designation.
    Format for year 1-999: TYYYHNNN where T=type, YYY=3-digit year, H=half-month, NNN=order+fragment
    """
    order_encoded = encode_cycle_count(order_num)
    fragment_code = "0" if not fragment else fragment.lower()

    if year < 0:
        # BCE year
        prefix, code = encode_bce_year(year)
        return f"{comet_type}{prefix}{code}{half_month}{order_encoded}{fragment_code}"
    else:
        # Ancient (1-999)
        return f"{comet_type}{year:03d}{half_month}{order_encoded}{fragment_code}"


def unpack_ancient_comet_provisional(packed: str) -> str:
    """
    Unpack an ancient or BCE comet provisional designation.
    """
    packed = packed.strip()

    if len(packed) != 8:
        raise MPCDesignationError(f"Invalid ancient comet designation length: {packed}")

    comet_type = packed[0]

    if comet_type not in COMET_TYPES:
        raise MPCDesignationError(f"Invalid comet type: {comet_type}")

    # Check for BCE prefix
    if packed[1] in '/.\\-':
        prefix = packed[1]
        year_code = packed[2:4]
        year = decode_bce_year(prefix, year_code)
        half_month = packed[4]
        order_encoded = packed[5:7]
        fragment = packed[7]
    else:
        # Ancient year (3 digits)
        year = int(packed[1:4])
        half_month = packed[4]
        order_encoded = packed[5:7]
        fragment = packed[7]

    order_num = decode_cycle_count(order_encoded)
    result = f"{comet_type}/{year} {half_month}{order_num}"

    if fragment != "0":
        result += f"-{fragment.upper()}"

    return result


# =============================================================================
# Helper functions for comet format detection
# =============================================================================

def is_asteroid_style_packed(provisional_part: str) -> bool:
    """Check if a 7-char provisional uses asteroid-style (ends with uppercase letter)."""
    if len(provisional_part) != 7:
        return False
    last_char = provisional_part[6]
    return last_char.isupper()


def is_asteroid_style_unpacked(provisional: str) -> bool:
    """Check if an unpacked provisional uses asteroid-style (letter after half-month)."""
    # Comet-style: "1995 O1" - half-month followed by digit
    # Asteroid-style: "2006 AH2" - half-month followed by letter
    match = re.match(r'^\d{4}\s+([A-Z])(.)', provisional)
    if match:
        second_char = match.group(2)
        return second_char.isalpha()
    return False


# =============================================================================
# Full comet designations (with type prefix)
# =============================================================================

def unpack_comet_full(packed: str) -> str:
    """
    Unpack a full comet designation (type + provisional or numbered).
    Input: 8, 9, or 12 character packed format
    Output: unpacked format like "C/1995 O1" or "1P/1982 U1"
    """
    length = len(packed)

    if length == 8:
        # Compact 8-char format: type + 7-char provisional
        comet_type = packed[0]
        provisional_part = packed[1:8]
        num_str = ""
    elif length == 9:
        # Compact 9-char format with 2-letter fragment: type + 8-char provisional
        comet_type = packed[0]
        provisional_part = packed[1:9]
        num_str = ""
    elif length == 12 or (length < 12 and packed[0] == ' '):
        # Full 12-char format or trimmed version
        # Pad with leading spaces if needed
        while len(packed) < 12:
            packed = " " + packed
        num_part = packed[0:4]
        comet_type = packed[4]
        provisional_part = packed[5:12]
        num_str = num_part.strip()
    else:
        raise MPCDesignationError(f"Invalid packed full comet designation length: {packed}")

    if comet_type not in COMET_TYPES:
        raise MPCDesignationError(f"Invalid comet type: {comet_type}")

    # Determine if this uses asteroid-style or comet-style provisional
    if is_asteroid_style_packed(provisional_part):
        provisional = unpack_provisional(provisional_part)
    else:
        provisional = unpack_comet_provisional(provisional_part)

    # Check if there's a periodic number
    if num_str == "":
        return f"{comet_type}/{provisional}"
    else:
        number = int(num_str)
        return f"{number}{comet_type}/{provisional}"


def pack_comet_full(unpacked: str) -> str:
    """
    Pack a full comet designation.
    Input: unpacked format like "C/1995 O1" or "1P/1986 F1"
    Output: 8 or 12 character packed format
    """
    unpacked = unpacked.strip()

    # Match: optional number, type, slash, year, provisional
    match = re.match(r'^(\d*)([PCDXAI])/(-?\d+)\s+(.+)$', unpacked)
    if not match:
        raise MPCDesignationError(f"Invalid unpacked comet designation: {unpacked}")

    number = match.group(1)
    comet_type = match.group(2)
    year = int(match.group(3))
    prov_part = match.group(4)

    if comet_type not in COMET_TYPES:
        raise MPCDesignationError(f"Invalid comet type: {comet_type}")

    # Check for ancient or BCE year
    if is_ancient_year(year):
        # Parse the provisional part for ancient comets: "L1" or "L1-F"
        match = re.match(r'^([A-Z])(\d+)(?:-([A-Z]))?$', prov_part)
        if match:
            half_month = match.group(1)
            order_num = int(match.group(2))
            fragment = match.group(3) or ""
            return pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment)
        else:
            raise MPCDesignationError(f"Invalid ancient comet provisional: {prov_part}")

    # Modern comet - reconstruct provisional with year
    provisional = f"{year} {prov_part}"

    # Determine if this uses asteroid-style or comet-style provisional
    if is_asteroid_style_unpacked(provisional):
        provisional_packed = pack_provisional(provisional)
    else:
        provisional_packed = pack_comet_provisional(provisional)

    if number == "":
        # No periodic number - use compact format (type + provisional)
        return f"{comet_type}{provisional_packed}"
    else:
        # Has periodic number - use full 12-char format
        num = int(number)
        if num < 1 or num > 9999:
            raise MPCDesignationError(f"Comet number out of range (1-9999): {num}")
        return f"{num:04d}{comet_type}{provisional_packed}"


# =============================================================================
# Format detection
# =============================================================================

def detect_format(designation: str) -> Dict[str, Any]:
    """
    Detect if a designation is packed or unpacked and what type it is.
    Returns: dict with keys: format, type, subtype
    """
    result = {'format': '', 'type': '', 'subtype': ''}

    # Check for packed full comet designation BEFORE trimming (12 chars with spaces)
    if len(designation) == 12:
        if re.match(r'^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$', designation):
            result['format'] = 'packed'
            result['type'] = 'comet_full'
            result['subtype'] = 'comet with provisional designation (12-char)'
            return result

    # Check for packed comet designation (8 chars: type + 7 char provisional)
    if len(designation) == 8:
        if re.match(r'^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$', designation):
            result['format'] = 'packed'
            result['type'] = 'comet_full'
            result['subtype'] = 'comet with provisional designation (8-char)'
            return result

    # Check for packed comet with 2-letter fragment (9 chars)
    if len(designation) == 9:
        if re.match(r'^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$', designation):
            result['format'] = 'packed'
            result['type'] = 'comet_full'
            result['subtype'] = 'comet with provisional designation (9-char, 2-letter fragment)'
            return result

    # Check for packed ancient comet (8 chars: type + 3-digit year + provisional)
    if len(designation) == 8:
        if re.match(r'^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$', designation):
            result['format'] = 'packed'
            result['type'] = 'comet_ancient'
            result['subtype'] = 'comet with ancient provisional (year < 1000)'
            return result

    # Check for packed BCE comet (8 chars: type + BCE prefix + code + provisional)
    if len(designation) == 8:
        if re.match(r'^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$', designation):
            result['format'] = 'packed'
            result['type'] = 'comet_bce'
            result['subtype'] = 'comet with BCE provisional'
            return result

    des = designation.strip()

    # Check for packed satellite designation (8 chars starting with S)
    if len(des) == 8 and des[0] == 'S':
        if re.match(r'^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$', des):
            result['format'] = 'packed'
            result['type'] = 'satellite'
            result['subtype'] = 'natural satellite provisional'
            return result

    # Check for packed permanent (numbered) asteroid
    if len(des) == 5:
        if des[0] == '~':
            # Tilde format >= 620,000
            if re.match(r'^~[0-9A-Za-z]{4}$', des):
                result['format'] = 'packed'
                result['type'] = 'permanent'
                result['subtype'] = 'numbered asteroid (>=620,000)'
                return result
        elif des.isdigit():
            result['format'] = 'packed'
            result['type'] = 'permanent'
            result['subtype'] = 'numbered asteroid (<100,000)'
            return result
        elif re.match(r'^[A-Za-z][0-9]{4}$', des):
            result['format'] = 'packed'
            result['type'] = 'permanent'
            result['subtype'] = 'numbered asteroid (100,000-619,999)'
            return result

    # Check for packed provisional asteroid (7 chars)
    if len(des) == 7:
        # Extended format with underscore
        if des[0] == '_':
            if re.match(r'^_[0-9][A-Z][0-9A-Za-z]{4}$', des):
                result['format'] = 'packed'
                result['type'] = 'provisional_extended'
                result['subtype'] = 'provisional (extended format, cycle >=620)'
                return result
        # Standard provisional or survey
        if re.match(r'^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$', des):
            result['format'] = 'packed'
            result['type'] = 'provisional'
            result['subtype'] = 'provisional'
            return result
        if re.match(r'^(PLS|T[123]S)\d{4}$', des):
            result['format'] = 'packed'
            result['type'] = 'survey'
            result['subtype'] = 'survey designation'
            return result

    # Check for packed numbered comet (5 chars ending in P or D)
    if len(des) == 5:
        if re.match(r'^[0-9]{4}[PD]$', des):
            result['format'] = 'packed'
            result['type'] = 'comet_numbered'
            result['subtype'] = 'numbered periodic comet'
            return result

    # Check for packed comet provisional (7 chars starting with century code)
    if len(des) == 7:
        if re.match(r'^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$', des):
            result['format'] = 'packed'
            result['type'] = 'comet_provisional'
            result['subtype'] = 'comet provisional'
            return result

    # --- UNPACKED FORMATS ---

    # Check for unpacked satellite: "S/2019 S 22"
    if re.match(r'^S/\d{4}\s+[JSUN]\s+\d+$', des):
        result['format'] = 'unpacked'
        result['type'] = 'satellite'
        result['subtype'] = 'natural satellite provisional'
        return result

    # Check for unpacked permanent (numbered) asteroid
    if des.isdigit():
        result['format'] = 'unpacked'
        result['type'] = 'permanent'
        result['subtype'] = 'numbered asteroid'
        return result

    # Check for unpacked survey designation: "2040 P-L" or "3138 T-1"
    if re.match(r'^\d+\s+(P-L|T-[123])$', des):
        result['format'] = 'unpacked'
        result['type'] = 'survey'
        result['subtype'] = 'survey designation'
        return result

    # Check for old-style asteroid designation: "A908 CJ"
    if re.match(r'^[AB]\d{3}\s+[A-Z][A-Z]$', des):
        result['format'] = 'unpacked'
        result['type'] = 'provisional'
        result['subtype'] = 'provisional (old-style pre-1925)'
        return result

    # Check for unpacked provisional asteroid: "1995 XA" or "2024 AB12"
    if re.match(r'^\d{4}\s+[A-Z][A-Z]\d*$', des):
        result['format'] = 'unpacked'
        result['type'] = 'provisional'
        result['subtype'] = 'provisional'
        return result

    # Check for unpacked comet with type prefix
    if re.match(r'^(\d*)([PCDXAI])/(-?\d+)\s+([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$', des):
        match = re.match(r'^(\d*)([PCDXAI])/(-?\d+)\s+([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$', des)
        num = match.group(1)
        ctype = match.group(2)
        year = int(match.group(3))

        if year < 0:
            year_desc = "BCE"
        elif year < 1000:
            year_desc = "ancient"
        else:
            year_desc = ""

        result['format'] = 'unpacked'
        result['type'] = 'comet_full'

        type_desc = COMET_TYPE_DESCRIPTIONS.get(ctype, ctype)
        if num:
            if year_desc:
                result['subtype'] = f"comet numbered with {year_desc} provisional ({type_desc})"
            else:
                result['subtype'] = f"comet numbered with provisional ({type_desc})"
        else:
            if year_desc:
                result['subtype'] = f"comet {year_desc} provisional ({type_desc})"
            else:
                result['subtype'] = f"comet provisional ({type_desc})"
        return result

    # Check for unpacked numbered periodic comet "1P" or "354P"
    if re.match(r'^(\d+)([PD])(?:/[A-Za-z].*)?$', des):
        result['format'] = 'unpacked'
        result['type'] = 'comet_numbered'
        result['subtype'] = 'numbered periodic comet'
        return result

    raise MPCDesignationError(f"Unable to detect designation format: {designation}")


# =============================================================================
# Main conversion function
# =============================================================================

def convert(designation: str) -> Dict[str, Any]:
    """
    Convert a designation between packed and unpacked formats.
    Auto-detects the input format and converts to the other.
    Returns: dict with keys: input, output, info
    """
    info = detect_format(designation)
    fmt = info['format']
    dtype = info['type']

    result = ""

    if fmt == 'packed':
        if dtype == 'permanent':
            result = str(unpack_permanent(designation))
        elif dtype in ('provisional', 'survey'):
            result = unpack_provisional(designation)
        elif dtype == 'provisional_extended':
            result = unpack_extended_provisional(designation)
        elif dtype == 'comet_numbered':
            result = unpack_comet_numbered(designation)
        elif dtype == 'comet_provisional':
            result = unpack_comet_provisional(designation)
        elif dtype == 'comet_full':
            result = unpack_comet_full(designation)
        elif dtype in ('comet_ancient', 'comet_bce'):
            result = unpack_ancient_comet_provisional(designation)
        elif dtype == 'satellite':
            result = unpack_satellite(designation)
    else:  # unpacked
        if dtype == 'permanent':
            result = pack_permanent(int(designation))
        elif dtype in ('provisional', 'survey'):
            result = pack_provisional(designation)
        elif dtype == 'comet_numbered':
            result = pack_comet_numbered(designation)
        elif dtype == 'comet_full':
            result = pack_comet_full(designation)
        elif dtype == 'satellite':
            result = pack_satellite(designation)

    return {
        'input': designation,
        'output': result,
        'info': info
    }


def convert_simple(designation: str) -> str:
    """
    Convert a designation between packed and unpacked formats.
    Returns just the converted string.
    """
    return convert(designation)['output']


# =============================================================================
# Command-line interface
# =============================================================================

def print_usage():
    print("Usage: mpc_designation.py [-v|--verbose] <designation> [designation ...]", file=sys.stderr)
    print("", file=sys.stderr)
    print("Convert between packed and unpacked MPC designations.", file=sys.stderr)
    print("Auto-detects the input format and converts to the other.", file=sys.stderr)
    print("", file=sys.stderr)
    print("Options:", file=sys.stderr)
    print("  -v, --verbose   Show detailed information about the conversion", file=sys.stderr)
    print("", file=sys.stderr)
    print("Examples:", file=sys.stderr)
    print("  mpc_designation.py 00001             -> 1", file=sys.stderr)
    print("  mpc_designation.py 1                 -> 00001", file=sys.stderr)
    print("  mpc_designation.py J95X00A           -> 1995 XA", file=sys.stderr)
    print("  mpc_designation.py '1995 XA'         -> J95X00A", file=sys.stderr)
    print("  mpc_designation.py 'C/1995 O1'       -> CJ95O010", file=sys.stderr)
    print("  mpc_designation.py 1P                -> 0001P", file=sys.stderr)
    print("  mpc_designation.py 'D/1993 F2-B'     -> DJ93F02b (fragment B)", file=sys.stderr)


def main():
    args = sys.argv[1:]

    if not args:
        print_usage()
        sys.exit(1)

    verbose = False
    designations = []

    for arg in args:
        if arg in ('-v', '--verbose'):
            verbose = True
        elif arg in ('-h', '--help'):
            print_usage()
            sys.exit(0)
        else:
            designations.append(arg)

    if not designations:
        print_usage()
        sys.exit(1)

    multiple = len(designations) > 1

    for des in designations:
        try:
            result = convert(des)
            info = result['info']
            output = result['output']

            if verbose:
                print(f"  Input:    {des}")
                print(f"  Detected: {info['format']} format, {info['subtype']}")
                action = "unpacking to human-readable form" if info['format'] == 'packed' else "packing to MPC compact form"
                print(f"  Action:   {action}")
                print(f"  Output:   {output}")
                if multiple:
                    print()
            elif multiple:
                print(f"{des} -> {output}")
            else:
                print(output)
        except MPCDesignationError as e:
            print(f"Error: {e}", file=sys.stderr)
            sys.exit(1)


if __name__ == '__main__':
    main()
