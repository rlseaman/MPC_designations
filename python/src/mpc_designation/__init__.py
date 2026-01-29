"""
MPC Designation Converter - Convert between packed and unpacked MPC designations

Usage:
    from mpc_designation import convert, convert_simple, MPCDesignationError

    result = convert_simple('1995 XA')  # Returns 'J95X00A'
    result = convert('1995 XA')         # Returns dict with input, output, info
"""

from .mpc_designation import (
    convert,
    convert_simple,
    MPCDesignationError,
    detect_format,
)

__all__ = [
    'convert',
    'convert_simple',
    'MPCDesignationError',
    'detect_format',
]

__version__ = '1.0.0'
