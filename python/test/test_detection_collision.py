#!/usr/bin/env python3
"""
Regression tests for packed-provisional detection collisions.

Motivating case: the Catalina Sky Survey assigns temporary tracklet IDs such
as "C03UYWZ" that look like a packed MPC provisional designation. A too-loose
detection regex routed these into the asteroid-provisional branch. The
canonical packed provisional is:

    [I-L] [0-9]{2} [A-HJ-Y] [0-9A-Za-z] [0-9] [A-HJ-Z]
    century  year   half-mon  cycle-hi  cycle-lo  order

Two structural invariants reject the collisions:
  1. Century code is I-L (1800-2199); asteroids have no provisional
     designation before 1892, so A-H (years < 1800) is impossible.
  2. The cycle's units character (position 6) is always a literal digit.
  3. Half-month and order letters skip I.

These strings must NOT be detected as asteroid provisionals; they should fail
detection cleanly. Valid controls must still convert.

Usage: python3 test/test_detection_collision.py
"""

import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from mpc_designation import convert_simple, MPCDesignationError

# Strings that must be REJECTED (not parsed as asteroid provisionals).
REJECT = [
    ('C03UYWZ', 'Catalina survey temp ID: century C = 1200s, impossible for asteroid'),
    ('K03UYWZ', 'valid century but letter W in digits-only cycle-units position'),
    ('A95X00A', 'century A = 1000s, before any asteroid'),
    ('H95X00A', 'century H = 1700s, before first asteroid provisional (1892)'),
    ('K95I00A', 'invalid half-month I in packed provisional'),
    ('K95X0IA', 'invalid order letter I'),
    ('K95Z00A', 'invalid half-month Z'),
    ('1995 XI', 'unpacked: order letter I (pack must reject, not emit J95X00I)'),
    ('A908 CI', 'unpacked old-style: order letter I'),
]

# Valid controls that must still convert correctly.
ACCEPT = [
    ('J95X00A', '1995 XA'),
    ('I92X00A', 'A892 XA'),   # earliest realistic: 1892 -> A-prefix form
    ('K14A00A', '2014 AA'),
    ('SK19S220', 'S/2019 S 22'),
    ('CJ95O010', 'C/1995 O1'),
]


def main():
    total = passed = failed = 0

    print("=== Detection Collision Regression Tests ===\n")

    for s, why in REJECT:
        total += 1
        try:
            out = convert_simple(s)
            print(f"FAIL [reject] {s!r}: expected rejection ({why})")
            print(f"      got: {out!r}")
            failed += 1
        except MPCDesignationError:
            passed += 1
        except Exception as e:  # any clean error is acceptable rejection
            passed += 1

    for s, expected in ACCEPT:
        total += 1
        try:
            out = convert_simple(s)
            if out == expected:
                passed += 1
            else:
                print(f"FAIL [accept] {s!r}: expected {expected!r}, got {out!r}")
                failed += 1
        except Exception as e:
            print(f"FAIL [accept] {s!r}: unexpected error {e}")
            failed += 1

    print(f"\nTotal: {total}, Passed: {passed}, Failed: {failed}")
    sys.exit(0 if failed == 0 else 1)


if __name__ == '__main__':
    main()
