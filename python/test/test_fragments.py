#!/usr/bin/env python3
"""
Test comet fragment handling in MPC designation converter.

Tests numbered comets with fragments (73P-A, 73P-AA) and
provisional comets with fragments (P/1930 J1-A, P/1930 J1-AA).

Fragment letters include all A-Z (including I, per MPC data).
"""

import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src', 'mpc_designation'))
from mpc_designation import convert_simple, detect_format, MPCDesignationError


def test_case(inp, expected, description=""):
    """Run a single test case."""
    try:
        result = convert_simple(inp)
        if result == expected:
            return True, None
        else:
            return False, f"expected '{expected}', got '{result}'"
    except MPCDesignationError as e:
        return False, f"unexpected error: {e}"


def test_error(inp, description=""):
    """Test that an input raises an error."""
    try:
        result = convert_simple(inp)
        return False, f"expected error, got '{result}'"
    except MPCDesignationError:
        return True, None


def main():
    tests = []
    passed = 0
    failed = 0

    print("=== MPC Designation Fragment Tests ===")
    print()

    # === Numbered Comet Fragments (Pack) ===
    print("--- Numbered Comet Fragments (Pack) ---")
    pack_cases = [
        ("73P", "0073P", "Basic numbered comet"),
        ("73P-A", "0073Pa", "Single-letter fragment A"),
        ("73P-B", "0073Pb", "Single-letter fragment B"),
        ("73P-H", "0073Ph", "Single-letter fragment H"),
        ("73P-I", "0073Pi", "Single-letter fragment I (included!)"),
        ("73P-J", "0073Pj", "Single-letter fragment J"),
        ("73P-Z", "0073Pz", "Single-letter fragment Z"),
        ("73P-AA", "0073Paa", "Two-letter fragment AA"),
        ("73P-AB", "0073Pab", "Two-letter fragment AB"),
        ("73P-AI", "0073Pai", "Two-letter fragment AI (I included)"),
        ("73P-AZ", "0073Paz", "Two-letter fragment AZ"),
        ("73P-BA", "0073Pba", "Two-letter fragment BA"),
        ("73P-BI", "0073Pbi", "Two-letter fragment BI"),
        ("73P-BZ", "0073Pbz", "Two-letter fragment BZ"),
        ("73P-ZZ", "0073Pzz", "Two-letter fragment ZZ"),
        ("1P-A", "0001Pa", "Comet 1P with fragment"),
        ("9999P-ZZ", "9999Pzz", "Maximum comet with fragment"),
        ("1D-A", "0001Da", "Defunct comet with fragment"),
        ("354D-BB", "0354Dbb", "Defunct comet with two-letter fragment"),
    ]

    for inp, expected, desc in pack_cases:
        success, error = test_case(inp, expected, desc)
        if success:
            print(f"  PASS: {inp} -> {expected}")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # === Numbered Comet Fragments (Unpack) ===
    print()
    print("--- Numbered Comet Fragments (Unpack) ---")
    unpack_cases = [
        ("0073P", "73P", "Basic numbered comet"),
        ("0073Pa", "73P-A", "Single-letter fragment A"),
        ("0073Pb", "73P-B", "Single-letter fragment B"),
        ("0073Ph", "73P-H", "Single-letter fragment H"),
        ("0073Pi", "73P-I", "Single-letter fragment I"),
        ("0073Pj", "73P-J", "Single-letter fragment J"),
        ("0073Pz", "73P-Z", "Single-letter fragment Z"),
        ("0073Paa", "73P-AA", "Two-letter fragment AA"),
        ("0073Pab", "73P-AB", "Two-letter fragment AB"),
        ("0073Pai", "73P-AI", "Two-letter fragment AI"),
        ("0073Paz", "73P-AZ", "Two-letter fragment AZ"),
        ("0073Pba", "73P-BA", "Two-letter fragment BA"),
        ("0073Pbi", "73P-BI", "Two-letter fragment BI"),
        ("0073Pbz", "73P-BZ", "Two-letter fragment BZ"),
        ("0073Pzz", "73P-ZZ", "Two-letter fragment ZZ"),
        ("0001Pa", "1P-A", "Comet 1P with fragment"),
        ("9999Pzz", "9999P-ZZ", "Maximum comet with fragment"),
        ("0001Da", "1D-A", "Defunct comet with fragment"),
        ("0354Dbb", "354D-BB", "Defunct comet with two-letter fragment"),
    ]

    for inp, expected, desc in unpack_cases:
        success, error = test_case(inp, expected, desc)
        if success:
            print(f"  PASS: {inp} -> {expected}")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # === Provisional Comet Fragments (Pack) ===
    print()
    print("--- Provisional Comet Fragments (Pack) ---")
    prov_pack_cases = [
        ("C/1995 O1", "CJ95O010", "Basic provisional comet"),
        ("D/1993 F2-A", "DJ93F02a", "Single-letter fragment A"),
        ("D/1993 F2-B", "DJ93F02b", "Single-letter fragment B"),
        ("D/1993 F2-W", "DJ93F02w", "Single-letter fragment W"),
        ("C/1996 B2-A", "CJ96B02a", "Hyakutake fragment A"),
        ("P/1930 J1-AA", "PJ30J01aa", "Two-letter fragment AA"),
        ("P/1930 J1-AI", "PJ30J01ai", "Two-letter fragment AI (I included)"),
        ("P/1930 J1-AZ", "PJ30J01az", "Two-letter fragment AZ"),
        ("P/1930 J1-BA", "PJ30J01ba", "Two-letter fragment BA"),
        ("P/1930 J1-BI", "PJ30J01bi", "Two-letter fragment BI"),
    ]

    for inp, expected, desc in prov_pack_cases:
        success, error = test_case(inp, expected, desc)
        if success:
            print(f"  PASS: {inp} -> {expected}")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # === Provisional Comet Fragments (Unpack) ===
    print()
    print("--- Provisional Comet Fragments (Unpack) ---")
    prov_unpack_cases = [
        ("CJ95O010", "C/1995 O1", "Basic provisional comet"),
        ("DJ93F02a", "D/1993 F2-A", "Single-letter fragment A"),
        ("DJ93F02b", "D/1993 F2-B", "Single-letter fragment B"),
        ("DJ93F02w", "D/1993 F2-W", "Single-letter fragment W"),
        ("CJ96B02a", "C/1996 B2-A", "Hyakutake fragment A"),
        ("PJ30J01aa", "P/1930 J1-AA", "Two-letter fragment AA"),
        ("PJ30J01ai", "P/1930 J1-AI", "Two-letter fragment AI"),
        ("PJ30J01az", "P/1930 J1-AZ", "Two-letter fragment AZ"),
        ("PJ30J01ba", "P/1930 J1-BA", "Two-letter fragment BA"),
        ("PJ30J01bi", "P/1930 J1-BI", "Two-letter fragment BI"),
    ]

    for inp, expected, desc in prov_unpack_cases:
        success, error = test_case(inp, expected, desc)
        if success:
            print(f"  PASS: {inp} -> {expected}")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # === Pre-1925 A-Prefix Format ===
    print()
    print("--- Pre-1925 A-Prefix Format ---")
    aprefix_cases = [
        ("I01A00A", "A801 AA", "1801 unpacks to A801"),
        ("J08C00J", "A908 CJ", "1908 unpacks to A908"),
        ("J24Y00Z", "A924 YZ", "1924 unpacks to A924"),
        ("J25A00A", "1925 AA", "1925 stays as 1925"),
        ("J26B01C", "1926 BC1", "1926 stays as 1926"),
        ("A908 CJ", "J08C00J", "A908 packs normally"),
        ("A801 AA", "I01A00A", "A801 packs normally"),
    ]

    for inp, expected, desc in aprefix_cases:
        success, error = test_case(inp, expected, desc)
        if success:
            print(f"  PASS: {inp} -> {expected}")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # === Century Code Validation ===
    print()
    print("--- Century Code Validation ---")
    century_valid_cases = [
        ("1800 AA", "I00A00A", "Minimum asteroid year 1800"),
        ("2199 YZ", "L99Y00Z", "Maximum asteroid year 2199"),
        ("C/1014 C1", "CA14C010", "Ancient comet 1014 CE"),
        ("C/1596 S1", "CF96S010", "Historical comet 1596"),
    ]

    for inp, expected, desc in century_valid_cases:
        success, error = test_case(inp, expected, desc)
        if success:
            print(f"  PASS: {inp} -> {expected}")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # Test invalid century codes (should error)
    print()
    print("--- Invalid Century Codes (should error) ---")
    invalid_cases = [
        ("1700 AA", "Asteroid year before 1800"),
        ("2200 AA", "Asteroid year after 2199"),
    ]

    for inp, desc in invalid_cases:
        success, error = test_error(inp, desc)
        if success:
            print(f"  PASS: {inp} correctly rejected")
            passed += 1
        else:
            print(f"  FAIL: {inp}: {error}")
            failed += 1

    # === Roundtrip Tests ===
    print()
    print("--- Roundtrip Tests ---")
    roundtrip_cases = [
        "73P-A",
        "73P-AA",
        "73P-AI",
        "73P-ZZ",
        "P/1930 J1-AA",
        "P/1930 J1-AI",
        "C/1995 O1",
        "D/1993 F2-B",
        "A908 CJ",
        "1995 XA",
    ]

    for inp in roundtrip_cases:
        try:
            packed = convert_simple(inp)
            unpacked = convert_simple(packed)
            if unpacked == inp:
                print(f"  PASS: {inp} -> {packed} -> {unpacked}")
                passed += 1
            else:
                print(f"  FAIL: {inp} -> {packed} -> {unpacked} (expected {inp})")
                failed += 1
        except MPCDesignationError as e:
            print(f"  FAIL: {inp}: {e}")
            failed += 1

    # Summary
    print()
    print("=" * 50)
    print(f"Total: {passed + failed}, Passed: {passed}, Failed: {failed}")

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
