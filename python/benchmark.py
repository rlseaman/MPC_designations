#!/usr/bin/env python3
"""
Benchmark script for MPC designation conversion.

Measures performance of convert_simple() over various batch sizes
and designation types.
"""

import time
import sys
import csv
import os
from typing import List, Tuple

# Add src to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from mpc_designation import convert_simple, pack, unpack
from mpc_designation.batch import convert_batch, pack_batch, unpack_batch


def load_test_data(csv_path: str, limit: int = None) -> Tuple[List[str], List[str]]:
    """Load test data from CSV file. Returns (unpacked, packed) lists."""
    unpacked = []
    packed = []

    with open(csv_path, 'r') as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            if limit and i >= limit:
                break
            # Handle different column names
            if 'unpacked' in row:
                unpacked.append(row['unpacked'])
                packed.append(row['packed'])
            else:
                unpacked.append(row['unpacked_provisional_designation'])
                packed.append(row['packed_provisional_designation'])

    return unpacked, packed


def benchmark_scalar(designations: List[str], label: str) -> dict:
    """Benchmark scalar conversion (one at a time)."""
    n = len(designations)

    # Warmup
    for d in designations[:min(1000, n)]:
        convert_simple(d)

    # Timed run
    start = time.perf_counter()
    results = []
    for d in designations:
        results.append(convert_simple(d))
    elapsed = time.perf_counter() - start

    rate = n / elapsed

    return {
        'label': label,
        'count': n,
        'elapsed_sec': elapsed,
        'rate_per_sec': rate,
        'us_per_call': (elapsed / n) * 1_000_000
    }


def benchmark_list_comprehension(designations: List[str], label: str) -> dict:
    """Benchmark using list comprehension."""
    n = len(designations)

    # Warmup
    _ = [convert_simple(d) for d in designations[:min(1000, n)]]

    # Timed run
    start = time.perf_counter()
    results = [convert_simple(d) for d in designations]
    elapsed = time.perf_counter() - start

    rate = n / elapsed

    return {
        'label': label,
        'count': n,
        'elapsed_sec': elapsed,
        'rate_per_sec': rate,
        'us_per_call': (elapsed / n) * 1_000_000
    }


def benchmark_parallel(designations: List[str], func, label: str, workers: int = None) -> dict:
    """Benchmark parallel batch processing."""
    n = len(designations)

    # Warmup with small batch
    _ = func(designations[:min(1000, n)], workers=workers)

    # Timed run
    start = time.perf_counter()
    results = func(designations, workers=workers)
    elapsed = time.perf_counter() - start

    rate = n / elapsed

    return {
        'label': label,
        'count': n,
        'elapsed_sec': elapsed,
        'rate_per_sec': rate,
        'us_per_call': (elapsed / n) * 1_000_000
    }


def print_result(result: dict):
    """Print benchmark result."""
    print(f"  {result['label']:40s}: {result['count']:>10,d} items in {result['elapsed_sec']:.3f}s "
          f"({result['rate_per_sec']:>10,.0f}/sec, {result['us_per_call']:.2f} us/call)")


def main():
    # Find test data
    script_dir = os.path.dirname(os.path.abspath(__file__))
    csv_path = os.path.join(script_dir, '..', 'test-data', 'prov_unpack_to_pack.csv')

    if not os.path.exists(csv_path):
        # Try .gz version
        gz_path = csv_path + '.gz'
        if os.path.exists(gz_path):
            print(f"Decompressing {gz_path}...")
            import gzip
            import shutil
            with gzip.open(gz_path, 'rb') as f_in:
                with open(csv_path, 'wb') as f_out:
                    shutil.copyfileobj(f_in, f_out)
        else:
            print(f"Test data not found: {csv_path}")
            sys.exit(1)

    print("=" * 80)
    print("MPC Designation Conversion Benchmark")
    print("=" * 80)
    print()

    # Load different amounts of test data
    print("Loading test data...")
    unpacked_full, packed_full = load_test_data(csv_path)
    print(f"  Total entries available: {len(unpacked_full):,}")
    print()

    results = []

    # Test different batch sizes
    batch_sizes = [1000, 10000, 100000, 500000, len(unpacked_full)]

    for batch_size in batch_sizes:
        if batch_size > len(unpacked_full):
            batch_size = len(unpacked_full)

        unpacked = unpacked_full[:batch_size]
        packed = packed_full[:batch_size]

        print(f"Batch size: {batch_size:,}")

        # Pack direction (unpacked -> packed)
        r = benchmark_scalar(unpacked, f"pack (unpacked->packed)")
        print_result(r)
        results.append(r)

        # Unpack direction (packed -> unpacked)
        r = benchmark_scalar(packed, f"unpack (packed->unpacked)")
        print_result(r)
        results.append(r)

        print()

    # Parallel benchmarks on full dataset
    print("=" * 80)
    print("PARALLEL PROCESSING BENCHMARKS (full dataset)")
    print("=" * 80)
    print()

    cpu_count = os.cpu_count() or 4

    for workers in [2, 4, min(8, cpu_count)]:
        print(f"Workers: {workers}")

        # Pack direction - parallel
        r = benchmark_parallel(unpacked_full, pack_batch, f"pack_batch (parallel, {workers}w)", workers=workers)
        print_result(r)
        results.append(r)

        # Unpack direction - parallel
        r = benchmark_parallel(packed_full, unpack_batch, f"unpack_batch (parallel, {workers}w)", workers=workers)
        print_result(r)
        results.append(r)

        print()

    # Summary
    print("=" * 80)
    print("SUMMARY (full dataset)")
    print("=" * 80)

    # Find scalar results for full dataset
    scalar_results = [r for r in results if r['count'] == len(unpacked_full) and 'parallel' not in r['label']]
    parallel_results = [r for r in results if r['count'] == len(unpacked_full) and 'parallel' in r['label']]

    print("\nScalar (single-threaded):")
    for r in scalar_results:
        if "unpack" in r['label']:
            direction = "unpack"
        else:
            direction = "pack"
        print(f"  {direction:8s}: {r['rate_per_sec']:>10,.0f} designations/sec ({r['us_per_call']:.2f} us/call)")

    # Group parallel results by worker count
    print("\nParallel (multi-process):")
    for r in parallel_results:
        print(f"  {r['label']:40s}: {r['rate_per_sec']:>10,.0f}/sec")

    # Calculate speedup
    print("\nSpeedup vs scalar:")
    scalar_pack = next((r for r in scalar_results if 'pack' in r['label'] and 'unpack' not in r['label']), None)
    scalar_unpack = next((r for r in scalar_results if 'unpack' in r['label']), None)

    for r in parallel_results:
        if 'pack_batch' in r['label'] and scalar_pack:
            speedup = r['rate_per_sec'] / scalar_pack['rate_per_sec']
            print(f"  {r['label']:40s}: {speedup:.2f}x")
        elif 'unpack_batch' in r['label'] and scalar_unpack:
            speedup = r['rate_per_sec'] / scalar_unpack['rate_per_sec']
            print(f"  {r['label']:40s}: {speedup:.2f}x")

    print()

    # Save results to file for comparison
    results_file = os.path.join(script_dir, 'benchmark_results.txt')
    with open(results_file, 'w') as f:
        f.write("MPC Designation Conversion Benchmark\n")
        f.write("=" * 70 + "\n\n")
        f.write(f"Python version: {sys.version}\n")
        f.write(f"CPU count: {cpu_count}\n")
        f.write(f"Test data: {len(unpacked_full):,} designations\n\n")

        f.write("Scalar (single-threaded) results:\n")
        for r in scalar_results:
            f.write(f"  {r['label']:40s}: {r['rate_per_sec']:>10,.0f}/sec ({r['us_per_call']:.2f} us/call)\n")

        f.write("\nParallel (multi-process) results:\n")
        for r in parallel_results:
            f.write(f"  {r['label']:40s}: {r['rate_per_sec']:>10,.0f}/sec ({r['us_per_call']:.2f} us/call)\n")

        f.write("\nSpeedup summary:\n")
        for r in parallel_results:
            if 'pack_batch' in r['label'] and scalar_pack:
                speedup = r['rate_per_sec'] / scalar_pack['rate_per_sec']
                f.write(f"  {r['label']:40s}: {speedup:.2f}x vs scalar\n")
            elif 'unpack_batch' in r['label'] and scalar_unpack:
                speedup = r['rate_per_sec'] / scalar_unpack['rate_per_sec']
                f.write(f"  {r['label']:40s}: {speedup:.2f}x vs scalar\n")

    print(f"Results saved to: {results_file}")


if __name__ == '__main__':
    main()
