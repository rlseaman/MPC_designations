#!/usr/bin/env python3
"""
Test mpc_designation.py error handling.

Tests various classes of invalid input to ensure proper error detection.
Reads test cases from error_test_cases.csv.

Usage: python3 test_errors.py [error_test_cases.csv]
"""

import sys
import re
import codecs
from mpc_designation import convert_simple, MPCDesignationError


def unescape_string(s: str) -> str:
    """Parse escape sequences in a string."""
    # Handle \xNN hex escapes first
    def hex_replace(match):
        return chr(int(match.group(1), 16))

    s = re.sub(r'\\x([0-9a-fA-F]{2})', hex_replace, s)

    # Handle standard escapes
    s = s.replace('\\n', '\n')
    s = s.replace('\\r', '\r')
    s = s.replace('\\t', '\t')
    s = s.replace('\\f', '\f')
    s = s.replace('\\v', '\v')
    s = s.replace('\\0', '\0')
    s = s.replace('\\\\', '\\')

    return s


def run_error_tests(csv_file: str) -> bool:
    """Run error tests from CSV file."""
    total = 0
    passed = 0
    failed = 0

    print("=== MPC Designation Error Tests ===\n")

    with open(csv_file, 'r', encoding='utf-8') as fp:
        for line_num, line in enumerate(fp, 1):
            line = line.strip()

            # Skip empty lines and comments
            if not line or line.startswith('#'):
                continue

            # Skip header
            if line.startswith('category,'):
                continue

            # Parse CSV (simple split, no quotes handling needed for this data)
            parts = line.split(',', 4)
            if len(parts) < 5:
                continue

            category = parts[0]
            subcategory = parts[1]
            input_str = parts[2]
            expected_error = parts[3]
            description = parts[4]

            # Unescape the input string
            input_str = unescape_string(input_str)

            total += 1

            # Run the test
            try:
                output = convert_simple(input_str)
                got_error = False
            except MPCDesignationError as e:
                got_error = True
                error_msg = str(e)
            except Exception as e:
                got_error = True
                error_msg = str(e)

            test_passed = False

            if expected_error == "valid":
                # Expect success
                if not got_error:
                    test_passed = True
                else:
                    print(f"FAIL [{category}/{subcategory}]: '{description}'")
                    print(f"      Expected: valid conversion")
                    print(f"      Got:      {error_msg}")
                    failed += 1
            else:
                # Expect error
                if got_error:
                    test_passed = True
                else:
                    print(f"FAIL [{category}/{subcategory}]: '{description}'")
                    print(f"      Expected: error ({expected_error})")
                    print(f"      Got:      '{output}' (success)")
                    failed += 1

            if test_passed:
                passed += 1

    print("\n=== Error Test Results ===")
    print(f"Total:  {total}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")

    return failed == 0


def main():
    csv_file = sys.argv[1] if len(sys.argv) > 1 else "error_test_cases.csv"

    try:
        success = run_error_tests(csv_file)
        sys.exit(0 if success else 1)
    except FileNotFoundError:
        print(f"Error: Cannot open file: {csv_file}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
