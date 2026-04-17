"""
mpc_designation_lite.py - Simplified MPC designation converter

This is a lightweight version that handles the most common cases:
- Numbered asteroids: "1" <-> "00001"
- Provisional asteroids: "1995 XA" <-> "J95X00A"

For comets, satellites, and extended formats, use the full mpc_designation module.

Example:
    from mpc_designation.lite import pack, unpack, convert

    # Provisional asteroid
    pack("1995 XA")     # -> "J95X00A"
    unpack("J95X00A")   # -> "1995 XA"

    # Numbered asteroid
    pack("123")         # -> "00123"
    unpack("A0345")     # -> "100345"
"""

import re

__all__ = ['pack', 'unpack', 'convert', 'is_packed', 'is_unpacked']

# Century codes: J=19, K=20, L=21
CENTURY_TO_LETTER = {19: 'J', 20: 'K', 21: 'L'}
LETTER_TO_CENTURY = {'I': 18, 'J': 19, 'K': 20, 'L': 21}

# Maximum numbered asteroid (using standard 5-char format)
MAX_PERMANENT_SIMPLE = 619999


def _decode_cycle(encoded: str) -> int:
    """Decode 2-character cycle count."""
    first = encoded[0]
    second = int(encoded[1])
    if first.isdigit():
        tens = int(first)
    elif first.isupper():
        tens = ord(first) - ord('A') + 10
    else:  # lowercase
        tens = ord(first) - ord('a') + 36
    return tens * 10 + second


def _encode_cycle(count: int) -> str:
    """Encode cycle count as 2 characters."""
    tens = count // 10
    ones = count % 10
    if tens < 10:
        first = str(tens)
    elif tens < 36:
        first = chr(ord('A') + tens - 10)
    else:
        first = chr(ord('a') + tens - 36)
    return f"{first}{ones}"


def is_packed(designation: str) -> bool:
    """Check if a designation is in packed format."""
    s = designation.strip()
    # Packed permanent: 5 digits or letter + 4 digits
    if len(s) == 5:
        if s.isdigit():
            return True
        if s[0].isalpha() and s[1:].isdigit():
            return True
    # Packed provisional: 7 chars starting with century letter
    if len(s) == 7 and s[0] in LETTER_TO_CENTURY:
        return True
    return False


def is_unpacked(designation: str) -> bool:
    """Check if a designation is in unpacked format."""
    s = designation.strip()
    # Unpacked permanent: just digits
    if s.isdigit():
        return True
    # Unpacked provisional: YYYY LL or YYYY LLnnn
    if re.match(r'^\d{4} [A-Z][A-Z]\d*$', s):
        return True
    return False


def pack(designation: str) -> str:
    """
    Ensure designation is in packed format.

    Handles:
    - Numbered asteroids: "123" -> "00123", "100345" -> "A0345"
    - Provisional: "1995 XA" -> "J95X00A", "2024 AB12" -> "K24A1B2"

    Raises ValueError for unsupported formats.
    """
    s = designation.strip()

    if is_packed(s):
        return s

    # Unpacked permanent (just digits)
    if s.isdigit():
        num = int(s)
        if num < 1 or num > MAX_PERMANENT_SIMPLE:
            raise ValueError(f"Number out of range: {num}")
        if num < 100000:
            return f"{num:05d}"
        div = num // 10000
        mod = num % 10000
        if div < 36:
            letter = chr(ord('A') + div - 10)
        else:
            letter = chr(ord('a') + div - 36)
        return f"{letter}{mod:04d}"

    # Unpacked provisional: "1995 XA" or "1995 XA12"
    match = re.match(r'^(\d{4}) ([A-Z])([A-Z])(\d*)$', s)
    if match:
        year = int(match.group(1))
        half_month = match.group(2)
        second_letter = match.group(3)
        order_str = match.group(4)

        century = year // 100
        if century not in CENTURY_TO_LETTER:
            raise ValueError(f"Year out of range: {year}")

        order = int(order_str) if order_str else 0
        if order >= 620:
            raise ValueError(f"Order too large for lite version: {order}")

        century_code = CENTURY_TO_LETTER[century]
        year_short = f"{year % 100:02d}"
        order_encoded = _encode_cycle(order)

        return f"{century_code}{year_short}{half_month}{order_encoded}{second_letter}"

    raise ValueError(f"Unsupported format: {designation}")


def unpack(designation: str) -> str:
    """
    Ensure designation is in unpacked (human-readable) format.

    Handles:
    - Numbered asteroids: "00123" -> "123", "A0345" -> "100345"
    - Provisional: "J95X00A" -> "1995 XA"

    Raises ValueError for unsupported formats.
    """
    s = designation.strip()

    if is_unpacked(s):
        return s

    # Packed permanent (5 chars)
    if len(s) == 5:
        if s.isdigit():
            return str(int(s))
        if s[0].isalpha() and s[1:].isdigit():
            if s[0].isupper():
                val = ord(s[0]) - ord('A') + 10
            else:
                val = ord(s[0]) - ord('a') + 36
            return str(val * 10000 + int(s[1:]))

    # Packed provisional (7 chars)
    if len(s) == 7 and s[0] in LETTER_TO_CENTURY:
        century = LETTER_TO_CENTURY[s[0]]
        year_short = s[1:3]
        half_month = s[3]
        order_encoded = s[4:6]
        second_letter = s[6]

        year = century * 100 + int(year_short)
        order = _decode_cycle(order_encoded)

        if order == 0:
            return f"{year} {half_month}{second_letter}"
        return f"{year} {half_month}{second_letter}{order}"

    raise ValueError(f"Unsupported format: {designation}")


def convert(designation: str) -> str:
    """
    Convert between packed and unpacked formats.
    Auto-detects the input format.
    """
    s = designation.strip()
    if is_packed(s):
        return unpack(s)
    if is_unpacked(s):
        return pack(s)
    raise ValueError(f"Unrecognized format: {designation}")
