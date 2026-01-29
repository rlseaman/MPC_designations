"""
MPC Designation Converter - Convert between packed and unpacked MPC designations

Usage:
    from mpc_designation import convert, convert_simple, MPCDesignationError

    result = convert_simple('1995 XA')  # Returns 'J95X00A'
    result = convert('1995 XA')         # Returns dict with input, output, info
"""

import os as _os

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

# Read version from root VERSION file
def _get_version():
    """Read version from VERSION file at repository root."""
    # Try multiple locations to find VERSION file
    paths = [
        _os.path.join(_os.path.dirname(__file__), '..', '..', '..', '..', 'VERSION'),  # Installed
        _os.path.join(_os.path.dirname(__file__), '..', '..', '..', 'VERSION'),  # Development
    ]
    for path in paths:
        try:
            with open(path, 'r') as f:
                return f.read().strip()
        except FileNotFoundError:
            continue
    return '1.0.0'  # Fallback

__version__ = _get_version()
