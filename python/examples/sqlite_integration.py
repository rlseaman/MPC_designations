#!/usr/bin/env python3
"""
sqlite_integration.py - Example of integrating MPC designation converter with SQLite

This example demonstrates:
1. Registering custom SQLite functions for MPC designation conversion
2. Importing data in mixed formats
3. Querying by either packed or unpacked designation
4. Normalizing designations to a consistent format

Usage:
    python sqlite_integration.py
"""

import sqlite3
import sys
import os

# Add the parent src directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from mpc_designation import convert_simple, detect_format, MPCDesignationError


def mpc_convert_udf(designation):
    """
    SQLite UDF: Convert between packed and unpacked formats.
    Returns NULL if the designation is invalid.
    """
    if designation is None:
        return None
    try:
        return convert_simple(designation)
    except MPCDesignationError:
        return None


def mpc_pack_udf(designation):
    """
    SQLite UDF: Convert to packed format.
    If already packed, returns as-is. Returns NULL on error.
    """
    if designation is None:
        return None
    try:
        info = detect_format(designation)
        if info['format'] == 'packed':
            return designation.strip()
        return convert_simple(designation)
    except MPCDesignationError:
        return None


def mpc_unpack_udf(designation):
    """
    SQLite UDF: Convert to unpacked format.
    If already unpacked, returns as-is. Returns NULL on error.
    """
    if designation is None:
        return None
    try:
        info = detect_format(designation)
        if info['format'] == 'unpacked':
            return designation.strip()
        return convert_simple(designation)
    except MPCDesignationError:
        return None


def mpc_format_udf(designation):
    """
    SQLite UDF: Detect format ('packed' or 'unpacked').
    Returns NULL if the designation is invalid.
    """
    if designation is None:
        return None
    try:
        info = detect_format(designation)
        return info['format']
    except MPCDesignationError:
        return None


def mpc_type_udf(designation):
    """
    SQLite UDF: Detect designation type (permanent, provisional, comet, etc.).
    Returns NULL if the designation is invalid.
    """
    if designation is None:
        return None
    try:
        info = detect_format(designation)
        return info['type']
    except MPCDesignationError:
        return None


def register_mpc_functions(conn):
    """Register all MPC designation functions with a SQLite connection."""
    conn.create_function('mpc_convert', 1, mpc_convert_udf)
    conn.create_function('mpc_pack', 1, mpc_pack_udf)
    conn.create_function('mpc_unpack', 1, mpc_unpack_udf)
    conn.create_function('mpc_format', 1, mpc_format_udf)
    conn.create_function('mpc_type', 1, mpc_type_udf)


def demo_basic_conversion():
    """Demonstrate basic conversion functions."""
    print("=== Basic Conversion Demo ===\n")

    conn = sqlite3.connect(':memory:')
    register_mpc_functions(conn)

    # Test various designations
    test_cases = [
        '1995 XA',      # Unpacked provisional asteroid
        'J95X00A',      # Packed provisional asteroid
        '1',            # Unpacked permanent asteroid
        '00001',        # Packed permanent asteroid
        'C/1995 O1',    # Unpacked comet
        'CJ95O010',     # Packed comet
        '1P',           # Unpacked numbered comet
        '0001P',        # Packed numbered comet
        'S/2019 S 22',  # Unpacked satellite
        'SK19S220',     # Packed satellite
    ]

    print("Designation       | Format   | Type       | Converted")
    print("-" * 60)

    for des in test_cases:
        result = conn.execute('''
            SELECT ?, mpc_format(?), mpc_type(?), mpc_convert(?)
        ''', (des, des, des, des)).fetchone()
        print(f"{result[0]:<17} | {result[1] or 'invalid':<8} | {result[2] or 'N/A':<10} | {result[3] or 'N/A'}")

    conn.close()
    print()


def demo_observation_database():
    """Demonstrate a realistic observation database use case."""
    print("=== Observation Database Demo ===\n")

    conn = sqlite3.connect(':memory:')
    register_mpc_functions(conn)

    # Create observations table
    conn.execute('''
        CREATE TABLE observations (
            id INTEGER PRIMARY KEY,
            designation TEXT NOT NULL,
            obs_date TEXT,
            ra REAL,
            dec REAL,
            magnitude REAL
        )
    ''')

    # Insert observations in mixed formats (as might come from different sources)
    observations = [
        ('1995 XA', '1995-12-15', 123.456, 45.678, 18.5),
        ('J95X00B', '1995-12-16', 124.567, 46.789, 19.2),
        ('C/1995 O1', '1995-07-23', 200.123, -10.456, 10.5),
        ('CJ95O010', '1995-07-24', 201.234, -11.567, 10.8),
        ('1', '2024-01-15', 300.111, 20.222, 15.0),
        ('00002', '2024-01-15', 301.222, 21.333, 14.5),
    ]

    conn.executemany('''
        INSERT INTO observations (designation, obs_date, ra, dec, magnitude)
        VALUES (?, ?, ?, ?, ?)
    ''', observations)

    print("Original data (mixed formats):")
    print("-" * 70)
    for row in conn.execute('SELECT * FROM observations'):
        print(row)
    print()

    # Query with both formats shown
    print("Query with format detection:")
    print("-" * 70)
    results = conn.execute('''
        SELECT
            designation,
            mpc_format(designation) as format,
            mpc_convert(designation) as converted,
            magnitude
        FROM observations
    ''').fetchall()

    for row in results:
        print(f"Original: {row[0]:<12} Format: {row[1]:<8} Converted: {row[2]:<12} Mag: {row[3]}")
    print()

    # Search by designation (works with either format)
    print("Search for '1995 XA' (finds both '1995 XA' and 'J95X00A'):")
    print("-" * 70)

    search_term = '1995 XA'
    packed = mpc_pack_udf(search_term)
    unpacked = mpc_unpack_udf(search_term)

    results = conn.execute('''
        SELECT designation, obs_date, magnitude
        FROM observations
        WHERE designation = ? OR designation = ?
    ''', (packed, unpacked)).fetchall()

    for row in results:
        print(f"  {row}")
    print()

    conn.close()


def demo_normalization():
    """Demonstrate normalizing a database to a consistent format."""
    print("=== Normalization Demo ===\n")

    conn = sqlite3.connect(':memory:')
    register_mpc_functions(conn)

    conn.execute('''
        CREATE TABLE asteroids (
            id INTEGER PRIMARY KEY,
            designation TEXT NOT NULL
        )
    ''')

    # Insert data in mixed formats (no duplicates for this demo)
    mixed_data = [
        ('1995 XA',),
        ('1995 XB',),
        ('1998 SQ108',),
        ('2024 AB123',),
        ('00001',),
        ('2',),
    ]
    conn.executemany('INSERT INTO asteroids (designation) VALUES (?)', mixed_data)

    print("Before normalization:")
    for row in conn.execute('SELECT * FROM asteroids'):
        print(f"  {row}")
    print()

    # Normalize all to packed format
    conn.execute('''
        UPDATE asteroids
        SET designation = mpc_pack(designation)
        WHERE mpc_format(designation) = 'unpacked'
    ''')
    conn.commit()

    print("After normalization (all packed):")
    for row in conn.execute('SELECT * FROM asteroids'):
        print(f"  {row}")
    print()

    conn.close()


def demo_error_handling():
    """Demonstrate handling invalid designations."""
    print("=== Error Handling Demo ===\n")

    conn = sqlite3.connect(':memory:')
    register_mpc_functions(conn)

    invalid_cases = [
        'invalid',
        '',
        '1995 IA',        # Invalid half-month (I)
        '1995  XA',       # Double space
        'hello world',
    ]

    print("Testing invalid designations (should return NULL):")
    print("-" * 50)

    for des in invalid_cases:
        result = conn.execute('''
            SELECT ?, mpc_convert(?), mpc_format(?)
        ''', (des, des, des)).fetchone()
        converted = result[1] if result[1] else 'NULL'
        fmt = result[2] if result[2] else 'NULL'
        print(f"Input: '{des}' -> Convert: {converted}, Format: {fmt}")

    conn.close()
    print()


def main():
    """Run all demos."""
    print("=" * 70)
    print("MPC Designation Converter - SQLite Integration Example")
    print("=" * 70)
    print()

    demo_basic_conversion()
    demo_observation_database()
    demo_normalization()
    demo_error_handling()

    print("All demos completed successfully!")


if __name__ == '__main__':
    main()
