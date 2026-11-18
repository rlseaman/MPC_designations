"""
mpc_designation_fast - array conversion of MPC designations, backed by C.

Vectorized companions to the pure-Python ``mpc_designation`` package:

    >>> import numpy as np
    >>> from mpc_designation_fast import convert_array, pack_array, unpack_array
    >>> pack_array(["1995 XA", "2024 AB12", "1P"])
    array(['J95X00A', 'K24A12B', '0001P'], dtype='<U7')

Each function accepts a NumPy array of str (``U``) or bytes (``S``), or any
sequence of str, and returns an array of the same shape. Bytes in give bytes
out; anything else gives str out.

Results are element-for-element identical to the reference functions
``mpc_designation.convert_simple``, ``pack`` and ``unpack`` (enforced by the
test suite on the full MPC-derived corpus and on fuzzed malformed input).

Errors: with ``errors="raise"`` (default) the first invalid element raises
``mpc_designation.MPCDesignationError`` naming its index. With
``errors="coerce"`` invalid elements become empty strings, so
``result == ""`` is the mask of failures.
"""

from typing import Any

import numpy as np
import mpc_designation as _ref
from mpc_designation import MPCDesignationError

from . import _core

__all__ = [
    "convert_array",
    "pack_array",
    "unpack_array",
    "MPCDesignationError",
    "__version__",
]

try:
    from importlib.metadata import version as _version
    __version__ = _version("mpc-designation-fast")
except Exception:  # pragma: no cover - not installed (e.g. source tree)
    __version__ = "1.1.5"

_REFERENCE = {
    _core.CONVERT: _ref.convert_simple,
    _core.PACK: _ref.pack,
    _core.UNPACK: _ref.unpack,
}


def _prepare(values: Any):
    """Return (contiguous 1-D S or native U array, shape, bytes_in, invalid mask or None)."""
    arr = np.asarray(values)
    if arr.size == 0 and arr.dtype.kind not in "SU":
        arr = arr.astype("U1")  # e.g. [] arrives as float64
    kind = arr.dtype.kind
    if kind in "SU":
        if kind == "U" and not arr.dtype.isnative:
            arr = arr.astype(arr.dtype.newbyteorder("="))
        flat = np.ascontiguousarray(arr.ravel())
        if flat.dtype.itemsize == 0:
            flat = flat.astype(kind + "1")
        return flat, arr.shape, kind == "S", None
    if kind != "O":
        raise TypeError(f"expected an array of str or bytes, got dtype {arr.dtype}")

    # Object arrays (e.g. lists mixing str and None): anything that is not a
    # str or bytes is invalid. Mixed str/bytes gives str output.
    flat = arr.ravel()
    invalid = np.array([not isinstance(v, (str, bytes)) for v in flat], dtype=bool)
    bytes_in = bool(flat.size) and all(isinstance(v, bytes) for v in flat)
    items = [v if isinstance(v, str) else v.decode("latin-1") if isinstance(v, bytes) else ""
             for v in flat]
    conv = np.array(items, dtype="U") if items else np.empty(0, dtype="U1")
    if conv.dtype.itemsize == 0:
        conv = conv.astype("U1")
    return np.ascontiguousarray(conv), arr.shape, bytes_in, invalid


def _run(values: Any, mode: int, errors: str):
    if errors not in ("raise", "coerce"):
        raise ValueError("errors must be 'raise' or 'coerce'")

    flat, shape, bytes_in, invalid = _prepare(values)
    char_size = 1 if flat.dtype.kind == "S" else 4
    in_chars = flat.dtype.itemsize // char_size

    out_buf, status_buf, _failures, out_chars = _core.batch(
        mode, flat.tobytes(), in_chars, char_size)
    out = np.frombuffer(out_buf, dtype=f"{'S' if char_size == 1 else 'U'}{out_chars}")
    bad = np.frombuffer(status_buf, dtype=np.int8) != 0
    if invalid is not None and invalid.any():
        bad = bad | invalid
        out[invalid] = ""

    if errors == "raise" and bad.any():
        idx = int(np.flatnonzero(bad)[0])
        original = np.asarray(values).ravel()[idx]
        if isinstance(original, np.generic):
            original = original.item()  # np.str_/np.bytes_ -> str/bytes
        try:
            _REFERENCE[mode](original if isinstance(original, str)
                             else bytes(original).decode("latin-1"))
            detail = "invalid designation"
        except Exception as exc:  # the reference's own message
            detail = str(exc)
        where = (tuple(int(i) for i in np.unravel_index(idx, shape))
                 if len(shape) > 1 else idx)
        raise MPCDesignationError(f"element {where} ({original!r}): {detail}")

    if bytes_in and out.dtype.kind == "U":
        out = out.astype(f"S{out_chars}")
    elif not bytes_in and out.dtype.kind == "S":
        out = out.astype(f"U{out_chars}")
    return out.reshape(shape)


def convert_array(values: Any, *, errors: str = "raise") -> np.ndarray:
    """Convert each designation to the other form (packed <-> unpacked)."""
    return _run(values, _core.CONVERT, errors)


def pack_array(values: Any, *, errors: str = "raise") -> np.ndarray:
    """Return each designation in packed form (packed input passes through)."""
    return _run(values, _core.PACK, errors)


def unpack_array(values: Any, *, errors: str = "raise") -> np.ndarray:
    """Return each designation in unpacked form (unpacked input passes through)."""
    return _run(values, _core.UNPACK, errors)
