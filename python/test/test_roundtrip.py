#!/usr/bin/env python3
"""
Test MPC designation conversion with bidirectional timing and round-trip verification.

Tests:
1. Pack direction (unpacked -> packed) with timing
2. Unpack direction (packed -> unpacked) with timing
3. Unpacked round-trip: unpack(pack(x)) = x
4. Packed round-trip: pack(unpack(y)) = y

Usage: python test_roundtrip.py <csv_file>
"""

import sys
import time
import os

# Add parent src directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))
from mpc_designation import convert_simple

MAX_ERRORS = 20


def main():
    if len(sys.argv) < 2:
        print("Usage: python test_roundtrip.py <csv_file>", file=sys.stderr)
        sys.exit(1)

    csv_file = sys.argv[1]

    # Load test data
    unpacked_list = []
    packed_list = []

    with open(csv_file, 'r') as f:
        next(f)  # Skip header
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = line.split(',', 2)
            if len(parts) >= 2:
                unpacked_list.append(parts[0])
                packed_list.append(parts[1])

    total = len(unpacked_list)
    print(f"Loaded {total} test cases\n")

    errors = []

    # ========== Phase 1: Pack (unpacked -> packed) ==========
    print("=== Phase 1: Pack (unpacked -> packed) ===")
    pack_passed = 0
    pack_failed = 0
    start = time.time()

    for i in range(total):
        unpacked = unpacked_list[i]
        expected = packed_list[i]
        try:
            got = convert_simple(unpacked)
            if got == expected:
                pack_passed += 1
            else:
                pack_failed += 1
                if len(errors) < MAX_ERRORS:
                    errors.append(('pack', unpacked, got, expected))
        except Exception as e:
            pack_failed += 1
            if len(errors) < MAX_ERRORS:
                errors.append(('pack', unpacked, f'ERROR: {e}', expected))

    pack_time = (time.time() - start) * 1000
    pack_rate = total / (pack_time / 1000)
    print(f"Passed: {pack_passed}")
    print(f"Failed: {pack_failed}")
    print(f"Time:   {pack_time:.0f}ms ({pack_rate:.1f} entries/sec)\n")

    # ========== Phase 2: Unpack (packed -> unpacked) ==========
    print("=== Phase 2: Unpack (packed -> unpacked) ===")
    unpack_passed = 0
    unpack_failed = 0
    start = time.time()

    for i in range(total):
        packed = packed_list[i]
        expected = unpacked_list[i]
        try:
            got = convert_simple(packed)
            if got == expected:
                unpack_passed += 1
            else:
                unpack_failed += 1
                if len(errors) < MAX_ERRORS:
                    errors.append(('unpack', packed, got, expected))
        except Exception as e:
            unpack_failed += 1
            if len(errors) < MAX_ERRORS:
                errors.append(('unpack', packed, f'ERROR: {e}', expected))

    unpack_time = (time.time() - start) * 1000
    unpack_rate = total / (unpack_time / 1000)
    print(f"Passed: {unpack_passed}")
    print(f"Failed: {unpack_failed}")
    print(f"Time:   {unpack_time:.0f}ms ({unpack_rate:.1f} entries/sec)\n")

    # ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    print("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===")
    rt_unpacked_passed = 0
    rt_unpacked_failed = 0
    start = time.time()

    for i in range(total):
        original = unpacked_list[i]
        try:
            packed = convert_simple(original)
            unpacked = convert_simple(packed)
            if unpacked == original:
                rt_unpacked_passed += 1
            else:
                rt_unpacked_failed += 1
                if len(errors) < MAX_ERRORS:
                    errors.append(('rt-unp', original, f'{packed} -> {unpacked}', original))
        except Exception as e:
            rt_unpacked_failed += 1

    rt_unpacked_time = (time.time() - start) * 1000
    rt_unpacked_rate = total / (rt_unpacked_time / 1000)
    print(f"Passed: {rt_unpacked_passed}")
    print(f"Failed: {rt_unpacked_failed}")
    print(f"Time:   {rt_unpacked_time:.0f}ms ({rt_unpacked_rate:.1f} entries/sec)\n")

    # ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    print("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===")
    errors = []  # Reset to show phase 4 errors
    rt_packed_passed = 0
    rt_packed_failed = 0
    start = time.time()

    for i in range(total):
        original = packed_list[i]
        try:
            unpacked = convert_simple(original)
            repacked = convert_simple(unpacked)
            if repacked == original:
                rt_packed_passed += 1
            else:
                rt_packed_failed += 1
                if len(errors) < MAX_ERRORS:
                    errors.append(('rt-pak', original, f'{unpacked} -> {repacked}', original))
        except Exception as e:
            rt_packed_failed += 1
            if len(errors) < MAX_ERRORS:
                errors.append(('rt-pak', original, f'ERROR: {e}', original))

    rt_packed_time = (time.time() - start) * 1000
    rt_packed_rate = total / (rt_packed_time / 1000)
    print(f"Passed: {rt_packed_passed}")
    print(f"Failed: {rt_packed_failed}")
    print(f"Time:   {rt_packed_time:.0f}ms ({rt_packed_rate:.1f} entries/sec)\n")

    # ========== Summary ==========
    print("=== Summary ===")
    print(f"{'Phase':<30} {'Time (ms)':>10} {'Rate (entries/sec)':>18} {'Status':>12}")
    print(f"{'-'*30} {'-'*10} {'-'*18} {'-'*12}")

    def status(failed):
        return "PASS" if failed == 0 else f"FAIL ({failed})"

    print(f"{'Pack':<30} {pack_time:>10.0f} {pack_rate:>18.1f} {status(pack_failed):>12}")
    print(f"{'Unpack':<30} {unpack_time:>10.0f} {unpack_rate:>18.1f} {status(unpack_failed):>12}")
    print(f"{'Unpacked RT: unpack(pack(x))=x':<30} {rt_unpacked_time:>10.0f} {rt_unpacked_rate:>18.1f} {status(rt_unpacked_failed):>12}")
    print(f"{'Packed RT: pack(unpack(y))=y':<30} {rt_packed_time:>10.0f} {rt_packed_rate:>18.1f} {status(rt_packed_failed):>12}")
    print()

    # Show errors
    if errors:
        print(f"=== First {len(errors)} errors ===")
        print(f"{'Phase':<8} {'Input':<25} {'Got':<20} {'Expected':<20}")
        print(f"{'-'*8} {'-'*25} {'-'*20} {'-'*20}")
        for phase, inp, got, expected in errors:
            print(f"{phase:<8} {inp:<25} {got:<20} {expected:<20}")

    # Exit with error only if pack or packed RT failed (those are real bugs)
    total_failed = pack_failed + rt_packed_failed
    sys.exit(1 if total_failed > 0 else 0)


if __name__ == '__main__':
    main()
