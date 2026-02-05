"""
MPC Designation Converter - Convert between packed and unpacked MPC designations

Usage:
    from mpc_designation import convert_simple, pack, unpack

    # Auto-detect and flip format
    result = convert_simple('1995 XA')  # Returns 'J95X00A'
    result = convert_simple('J95X00A')  # Returns '1995 XA'

    # Ensure specific format (idempotent)
    packed = pack('1995 XA')     # Returns 'J95X00A'
    packed = pack('J95X00A')     # Returns 'J95X00A' (already packed)
    unpacked = unpack('J95X00A') # Returns '1995 XA'
    unpacked = unpack('1995 XA') # Returns '1995 XA' (already unpacked)

    # Validation without exceptions
    if is_valid_designation(user_input):
        result = convert_simple(user_input)

    # Category-specific (for known input types)
    packed = pack_asteroid('1995 XA')
    packed = pack_comet('C/1995 O1')

    # Batch processing (parallel, for large datasets)
    from mpc_designation.batch import convert_batch, pack_batch, unpack_batch
    results = convert_batch(designations, workers=4)
"""

from .mpc_designation import (
    # Core conversion functions
    convert,
    convert_simple,

    # High-level pack/unpack (auto-detect, idempotent)
    pack,
    unpack,

    # Category-specific pack/unpack
    pack_asteroid,
    unpack_asteroid,
    pack_comet,
    unpack_comet,
    pack_satellite_designation,
    unpack_satellite_designation,

    # Validation and hygiene
    detect_format,
    is_valid_designation,
    is_valid_mpc_chars,
    sanitize,

    # Low-level functions (for advanced use)
    pack_permanent,
    unpack_permanent,
    pack_provisional,
    unpack_provisional,
    pack_comet_numbered,
    unpack_comet_numbered,
    pack_comet_provisional,
    unpack_comet_provisional,
    pack_comet_full,
    unpack_comet_full,
    pack_satellite,
    unpack_satellite,

    # Helper functions
    to_report_format,
    from_report_format,
    has_fragment,
    get_fragment,
    get_parent,
    designations_equal,

    # Exception class
    MPCDesignationError,
)

__all__ = [
    # Core
    'convert',
    'convert_simple',

    # High-level
    'pack',
    'unpack',

    # Category-specific
    'pack_asteroid',
    'unpack_asteroid',
    'pack_comet',
    'unpack_comet',
    'pack_satellite_designation',
    'unpack_satellite_designation',

    # Validation
    'detect_format',
    'is_valid_designation',
    'is_valid_mpc_chars',
    'sanitize',

    # Low-level
    'pack_permanent',
    'unpack_permanent',
    'pack_provisional',
    'unpack_provisional',
    'pack_comet_numbered',
    'unpack_comet_numbered',
    'pack_comet_provisional',
    'unpack_comet_provisional',
    'pack_comet_full',
    'unpack_comet_full',
    'pack_satellite',
    'unpack_satellite',

    # Helper functions
    'to_report_format',
    'from_report_format',
    'has_fragment',
    'get_fragment',
    'get_parent',
    'designations_equal',

    # Exception
    'MPCDesignationError',
]

# Version: use importlib.metadata for installed packages, fallback for development
def _get_version():
    """Get package version from installed metadata or fallback."""
    try:
        from importlib.metadata import version
        return version("mpc-designation")
    except Exception:
        # Fallback for development or if package not installed
        return "1.0.0"

__version__ = _get_version()
