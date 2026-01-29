#!/usr/bin/env python3
"""
example_usage.py - Example usage of the MPC designation library

Run: python example_usage.py
"""

import sys
import os

# Add src directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from mpc_designation import convert, convert_simple, detect_format, MPCDesignationError


def example_simple_conversion():
    """Example 1: Simple conversion"""
    print("=== Simple Conversion ===")

    designations = [
        "1995 XA",      # Provisional asteroid
        "J95X00A",      # Packed provisional
        "1",            # Numbered asteroid
        "C/1995 O1",    # Comet
        "S/2019 S 22",  # Natural satellite
    ]

    for des in designations:
        try:
            result = convert_simple(des)
            print(f"  {des} -> {result}")
        except MPCDesignationError as e:
            print(f"  {des} -> Error: {e}")
    print()


def example_with_info():
    """Example 2: Conversion with format information"""
    print("=== Conversion with Info ===")

    result = convert("1995 XA")
    print(f"  Input:   {result['input']}")
    print(f"  Output:  {result['output']}")
    print(f"  Format:  {result['format']}")
    print(f"  Type:    {result['subtype']}")
    print()


def example_error_handling():
    """Example 3: Error handling"""
    print("=== Error Handling ===")

    invalid = [
        "invalid",      # Not a valid designation
        "",             # Empty string
        "99999999999",  # Number too large
    ]

    for inp in invalid:
        try:
            convert_simple(inp)
        except MPCDesignationError as e:
            print(f"  '{inp}' -> {e}")
    print()


def example_bidirectional():
    """Example 4: Bidirectional conversion"""
    print("=== Bidirectional Conversion ===")

    # Pack
    packed = convert_simple("2024 AB")
    print(f"  Pack:   '2024 AB' -> '{packed}'")

    # Unpack
    unpacked = convert_simple(packed)
    print(f"  Unpack: '{packed}' -> '{unpacked}'")
    print()


def example_format_detection():
    """Example 5: Format detection"""
    print("=== Format Detection ===")

    examples = ["1995 XA", "J95X00A", "1P", "00001"]

    for des in examples:
        info = detect_format(des)
        print(f"  {des:12} -> format={info['format']}, type={info['type']}")
    print()


def example_designation_types():
    """Example 6: Different designation types"""
    print("=== Designation Types ===")

    examples = [
        ("1", "Numbered asteroid (Ceres)"),
        ("100001", "High numbered asteroid"),
        ("1995 XA", "Provisional asteroid"),
        ("2040 P-L", "Survey asteroid (Palomar-Leiden)"),
        ("1P", "Numbered comet (Halley)"),
        ("C/1995 O1", "Provisional comet (Hale-Bopp)"),
        ("D/1993 F2-B", "Comet fragment (Shoemaker-Levy 9)"),
        ("S/2019 S 22", "Natural satellite (Saturn moon)"),
    ]

    for inp, description in examples:
        result = convert_simple(inp)
        print(f"  {inp:15} -> {result:12} ({description})")
    print()


def example_batch_processing():
    """Example 7: Batch processing from file"""
    print("=== Batch Processing ===")

    # Simulated batch data
    batch = """1
    1995 XA
    C/1995 O1
    invalid
    S/2019 S 22"""

    success = 0
    failure = 0

    for line in batch.strip().split('\n'):
        des = line.strip()
        try:
            result = convert_simple(des)
            success += 1
        except MPCDesignationError:
            failure += 1

    print(f"  Processed {success + failure} designations")
    print(f"  Success: {success}")
    print(f"  Failure: {failure}")
    print()


def main():
    print("MPC Designation Library - Example Usage")
    print("========================================\n")

    example_simple_conversion()
    example_with_info()
    example_error_handling()
    example_bidirectional()
    example_format_detection()
    example_designation_types()
    example_batch_processing()


if __name__ == '__main__':
    main()
