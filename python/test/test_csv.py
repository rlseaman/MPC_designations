#!/usr/bin/env python3
"""
Test mpc_designation.py against CSV file of known conversions.

Usage: python test_csv.py <csv_file> [max_errors]
"""

import sys
import os
import time

# Add src directory to path for importing mpc_designation
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from mpc_designation import convert_simple, MPCDesignationError


def run_tests(csv_file: str, max_errors: int = 100) -> bool:
    """Run tests from CSV file and report results."""
    total = 0
    passed = 0
    failed = 0
    errors = []

    start_time = time.time()

    with open(csv_file, 'r') as fp:
        # Skip header line
        next(fp)

        for line in fp:
            total += 1
            line = line.strip()
            if not line:
                continue

            parts = line.split(',')
            unpacked = parts[0]
            expected_packed = parts[1]

            try:
                got_packed = convert_simple(unpacked)
            except MPCDesignationError as e:
                failed += 1
                if len(errors) < max_errors:
                    errors.append((unpacked, f"ERROR: {e}", expected_packed))
                continue

            if got_packed != expected_packed:
                failed += 1
                if len(errors) < max_errors:
                    errors.append((unpacked, got_packed, expected_packed))
            else:
                passed += 1

            # Progress indicator every 100,000 entries
            if total % 100000 == 0:
                print(f"Processed {total} entries...")

    elapsed = time.time() - start_time

    print()
    print("=== Test Results ===")
    print(f"Total:  {total}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Time:   {elapsed*1000:.0f}ms ({total/elapsed:.1f} entries/sec)")
    print()

    if failed > 0:
        print(f"=== First {len(errors)} failures ===")
        print(f"{'Input':<25} {'Got':<15} {'Expected':<15}")
        print("-" * 60)
        for inp, got, expected in errors:
            print(f"{inp:<25} {got:<15} {expected:<15}")

    return failed == 0


def main():
    if len(sys.argv) < 2:
        print("Usage: test_csv.py <csv_file> [max_errors]", file=sys.stderr)
        sys.exit(1)

    csv_file = sys.argv[1]
    max_errors = int(sys.argv[2]) if len(sys.argv) > 2 else 100

    success = run_tests(csv_file, max_errors)
    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
