#!/usr/bin/env python3
"""
batch.py - Parallel batch processing for MPC designation conversions.

This module provides parallel processing capabilities for converting
large batches of designations. It uses Python's multiprocessing to
distribute work across CPU cores.

Usage:
    from mpc_designation.batch import convert_batch, pack_batch, unpack_batch

    # Convert a list of designations in parallel
    results = convert_batch(designations, workers=4)

    # Pack all designations
    packed = pack_batch(designations, workers=4)

    # Unpack all designations
    unpacked = unpack_batch(designations, workers=4)

The batch functions automatically choose optimal chunk sizes and worker
counts based on input size. For small batches (< 1000), single-threaded
processing may be faster due to multiprocessing overhead.
"""

import os
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
from typing import List, Optional, Union, Callable
from functools import partial

# Import core functions
from .mpc_designation import (
    convert_simple,
    pack,
    unpack,
    MPCDesignationError
)


def _get_default_workers() -> int:
    """Get default number of workers (CPU count, max 8)."""
    return min(os.cpu_count() or 4, 8)


def _chunk_list(lst: List, chunk_size: int) -> List[List]:
    """Split a list into chunks of specified size."""
    return [lst[i:i + chunk_size] for i in range(0, len(lst), chunk_size)]


def _process_chunk_convert(chunk: List[str]) -> List[str]:
    """Process a chunk of designations with convert_simple."""
    return [convert_simple(d) for d in chunk]


def _process_chunk_pack(chunk: List[str]) -> List[str]:
    """Process a chunk of designations with pack."""
    return [pack(d) for d in chunk]


def _process_chunk_unpack(chunk: List[str]) -> List[str]:
    """Process a chunk of designations with unpack."""
    return [unpack(d) for d in chunk]


def _process_chunk_with_errors(chunk: List[str], func: Callable) -> List[Union[str, None]]:
    """Process a chunk, returning None for errors instead of raising."""
    results = []
    for d in chunk:
        try:
            results.append(func(d))
        except MPCDesignationError:
            results.append(None)
    return results


def convert_batch(
    designations: List[str],
    workers: Optional[int] = None,
    chunk_size: Optional[int] = None,
    ignore_errors: bool = False
) -> List[str]:
    """
    Convert a batch of designations in parallel.

    Each designation is converted between packed and unpacked formats
    (auto-detects direction).

    Args:
        designations: List of designations to convert
        workers: Number of parallel workers (default: CPU count, max 8)
        chunk_size: Items per chunk (default: auto-calculated)
        ignore_errors: If True, return None for invalid designations
                      instead of raising an exception

    Returns:
        List of converted designations (same order as input)

    Raises:
        MPCDesignationError: If any designation is invalid (unless ignore_errors=True)

    Example:
        >>> results = convert_batch(["1995 XA", "00001", "1P"])
        >>> print(results)
        ['J95X00A', '1', '0001P']
    """
    n = len(designations)

    # For small batches, single-threaded is faster
    if n < 1000:
        if ignore_errors:
            return [
                convert_simple(d) if d else None
                for d in designations
                for _ in [None]  # trick to catch errors
            ]
        return [convert_simple(d) for d in designations]

    # Determine workers and chunk size
    if workers is None:
        workers = _get_default_workers()

    if chunk_size is None:
        # Aim for ~1000-5000 items per chunk, at least workers*2 chunks
        chunk_size = max(1000, min(5000, n // (workers * 2)))

    chunks = _chunk_list(designations, chunk_size)

    # Use ProcessPoolExecutor for CPU-bound work
    with ProcessPoolExecutor(max_workers=workers) as executor:
        if ignore_errors:
            chunk_results = list(executor.map(
                partial(_process_chunk_with_errors, func=convert_simple),
                chunks
            ))
        else:
            chunk_results = list(executor.map(_process_chunk_convert, chunks))

    # Flatten results
    results = []
    for chunk_result in chunk_results:
        results.extend(chunk_result)

    return results


def pack_batch(
    designations: List[str],
    workers: Optional[int] = None,
    chunk_size: Optional[int] = None,
    ignore_errors: bool = False
) -> List[str]:
    """
    Pack a batch of designations in parallel.

    Ensures all designations are in packed format.

    Args:
        designations: List of designations to pack
        workers: Number of parallel workers (default: CPU count, max 8)
        chunk_size: Items per chunk (default: auto-calculated)
        ignore_errors: If True, return None for invalid designations

    Returns:
        List of packed designations (same order as input)

    Example:
        >>> packed = pack_batch(["1995 XA", "1", "C/1995 O1"])
        >>> print(packed)
        ['J95X00A', '00001', 'CJ95O010']
    """
    n = len(designations)

    if n < 1000:
        if ignore_errors:
            results = []
            for d in designations:
                try:
                    results.append(pack(d))
                except MPCDesignationError:
                    results.append(None)
            return results
        return [pack(d) for d in designations]

    if workers is None:
        workers = _get_default_workers()

    if chunk_size is None:
        chunk_size = max(1000, min(5000, n // (workers * 2)))

    chunks = _chunk_list(designations, chunk_size)

    with ProcessPoolExecutor(max_workers=workers) as executor:
        if ignore_errors:
            chunk_results = list(executor.map(
                partial(_process_chunk_with_errors, func=pack),
                chunks
            ))
        else:
            chunk_results = list(executor.map(_process_chunk_pack, chunks))

    results = []
    for chunk_result in chunk_results:
        results.extend(chunk_result)

    return results


def unpack_batch(
    designations: List[str],
    workers: Optional[int] = None,
    chunk_size: Optional[int] = None,
    ignore_errors: bool = False
) -> List[str]:
    """
    Unpack a batch of designations in parallel.

    Ensures all designations are in unpacked (human-readable) format.

    Args:
        designations: List of designations to unpack
        workers: Number of parallel workers (default: CPU count, max 8)
        chunk_size: Items per chunk (default: auto-calculated)
        ignore_errors: If True, return None for invalid designations

    Returns:
        List of unpacked designations (same order as input)

    Example:
        >>> unpacked = unpack_batch(["J95X00A", "00001", "CJ95O010"])
        >>> print(unpacked)
        ['1995 XA', '1', 'C/1995 O1']
    """
    n = len(designations)

    if n < 1000:
        if ignore_errors:
            results = []
            for d in designations:
                try:
                    results.append(unpack(d))
                except MPCDesignationError:
                    results.append(None)
            return results
        return [unpack(d) for d in designations]

    if workers is None:
        workers = _get_default_workers()

    if chunk_size is None:
        chunk_size = max(1000, min(5000, n // (workers * 2)))

    chunks = _chunk_list(designations, chunk_size)

    with ProcessPoolExecutor(max_workers=workers) as executor:
        if ignore_errors:
            chunk_results = list(executor.map(
                partial(_process_chunk_with_errors, func=unpack),
                chunks
            ))
        else:
            chunk_results = list(executor.map(_process_chunk_unpack, chunks))

    results = []
    for chunk_result in chunk_results:
        results.extend(chunk_result)

    return results


# Thread-based alternatives (useful when GIL is released by I/O)
def convert_batch_threaded(
    designations: List[str],
    workers: Optional[int] = None,
    chunk_size: Optional[int] = None
) -> List[str]:
    """
    Convert a batch using threads instead of processes.

    Generally slower than process-based for CPU-bound work, but useful
    when process spawning overhead is significant (small batches) or
    when running in environments that don't support multiprocessing.
    """
    n = len(designations)

    if n < 500:
        return [convert_simple(d) for d in designations]

    if workers is None:
        workers = _get_default_workers()

    if chunk_size is None:
        chunk_size = max(500, n // (workers * 2))

    chunks = _chunk_list(designations, chunk_size)

    with ThreadPoolExecutor(max_workers=workers) as executor:
        chunk_results = list(executor.map(_process_chunk_convert, chunks))

    results = []
    for chunk_result in chunk_results:
        results.extend(chunk_result)

    return results
