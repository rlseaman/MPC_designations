"""
Tests for mpc_designation_fast: element-for-element parity with the
pure-Python reference (mpc_designation), plus API behavior.

Corpus tests need the repository's test-data/ directory and are marked
`corpus`; they are skipped when it is absent (e.g. testing an installed wheel
outside the repository).
"""
import csv
import gzip
import os
import random
import re

import numpy as np
import pytest

import mpc_designation as ref
from mpc_designation_fast import (MPCDesignationError, convert_array,
                                  pack_array, unpack_array)

TEST_DATA = os.path.join(os.path.dirname(__file__), "..", "..", "test-data")
PROV = os.path.join(TEST_DATA, "prov_unpack_to_pack.csv.gz")
NUMBERED = os.path.join(TEST_DATA, "numbered_pack.csv.gz")
ERRORS = os.path.join(TEST_DATA, "error_test_cases.csv")

FUNCS = [
    (convert_array, ref.convert_simple),
    (pack_array, ref.pack),
    (unpack_array, ref.unpack),
]


MAX_RESULT = 31  # longest result the fast package returns (see README)


def reference(fn, s):
    """Reference result, with '' for an error (matching errors='coerce').

    pack()/unpack() return already-converted input unchanged after format
    detection only, so they can echo arbitrarily long strings (e.g. a 40-digit
    "number"); the fast package reports anything longer than MAX_RESULT as an
    error instead.
    """
    try:
        r = fn(s)
    except Exception:
        return ""
    return r if len(r) <= MAX_RESULT else ""


def load_pairs(path):
    with gzip.open(path, "rt") as f:
        rows = list(csv.reader(f))[1:]
    return np.array([r[0] for r in rows]), np.array([r[1] for r in rows])


def assert_parity(values, fast, slow):
    got = fast(np.array(values, dtype=object), errors="coerce")
    want = [reference(slow, v) for v in values]
    diffs = [(v, g, w) for v, g, w in zip(values, got.tolist(), want) if g != w]
    assert not diffs, f"{len(diffs)} mismatches, e.g. {diffs[:10]}"


# --------------------------------------------------------------------------
# Corpus: every MPC-derived pair, both directions
# --------------------------------------------------------------------------

@pytest.mark.corpus
@pytest.mark.parametrize("path", [PROV, NUMBERED], ids=["provisional", "numbered"])
def test_corpus(path):
    if not os.path.exists(path):
        pytest.skip("test-data not available")
    unpacked, packed = load_pairs(path)
    np.testing.assert_array_equal(pack_array(unpacked), packed)
    np.testing.assert_array_equal(convert_array(unpacked), packed)
    np.testing.assert_array_equal(pack_array(packed), packed)  # pass-through
    # Unpacking is not always the identity on the unpacked column (pre-1925
    # A-prefix forms normalize), so compare with the reference instead.
    sample = packed[:: max(1, len(packed) // 200_000)]
    want = np.array([ref.unpack(p) for p in sample])
    np.testing.assert_array_equal(unpack_array(sample), want)
    np.testing.assert_array_equal(convert_array(sample), want)
    np.testing.assert_array_equal(pack_array(unpack_array(packed)), packed)


# --------------------------------------------------------------------------
# Shared error suite and malformed input: identical verdicts and results
# --------------------------------------------------------------------------

def _unescape(s):
    """Same escapes as the reference harness (python/test/test_errors.py)."""
    s = re.sub(r"\\x([0-9a-fA-F]{2})", lambda m: chr(int(m.group(1), 16)), s)
    for esc, ch in (("\\n", "\n"), ("\\r", "\r"), ("\\t", "\t"), ("\\f", "\f"),
                    ("\\v", "\v"), ("\\0", "\0"), ("\\\\", "\\")):
        s = s.replace(esc, ch)
    return s


@pytest.mark.corpus
def test_error_suite():
    if not os.path.exists(ERRORS):
        pytest.skip("test-data not available")
    with open(ERRORS, newline="") as f:
        rows = [r for r in csv.reader(f) if r and not r[0].startswith("#")][1:]
    inputs = [_unescape(r[2]) for r in rows]
    for fast, slow in FUNCS:
        assert_parity(inputs, fast, slow)
    got = convert_array(np.array(inputs, dtype=object), errors="coerce")
    for (cat, sub, _, expected, *_), out in zip(rows, got):
        assert (out != "") == (expected == "valid"), f"{cat}/{sub}"


def mutations(seeds, n_per_seed, rng):
    alphabet = ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "abcdefghijklmnopqrstuvwxyz _~-/.")
    out = []
    for s in seeds:
        out.append(s)
        for _ in range(n_per_seed):
            t = list(s)
            k = rng.randrange(5)
            i = rng.randrange(len(t))
            if k == 0:
                t[i] = rng.choice(alphabet)
            elif k == 1:
                t[i] = t[i].swapcase()
            elif k == 2 and len(t) > 1:
                del t[i]
            elif k == 3:
                t.insert(i, rng.choice(alphabet))
            else:
                t = [rng.choice(["", " ", "  "])] + t + [rng.choice(["", " ", "\t"])]
            out.append("".join(t))
    return out


SEED_DESIGNATIONS = [
    "1995 XA", "J95X00A", "2024 AB12", "K24A12B", "2000 AL591673", "_0Azzzz",
    "2025 OZ695", "_PO00Ud", "1", "00001", "100000", "A0000", "620000", "~0000",
    "2040 P-L", "PLS2040", "3138 T-1", "T1S3138", "A873 OA", "I73O00A",
    "C/1995 O1", "CJ95O010", "P/2019 LD2", "PK19L02D", "1P", "0001P",
    "73P-A", "0073Pa", "1P/1982 U1", "0001PJ82U010", "P/2025 OZ695", "P_PO00Ud",
    "1P/2025 OZ695", "0001P_PO00Ud", "C/-146 P1", "C.53P010", "3I", "0003I",
    "2I-A", "0002Ia", "S/2019 S 22", "SK19S220", "C/1995 O1-B", "CJ95O01b",
    "K06S45a", "C/2019 Q4", "A/2017 U1", "AK17U010", "D/1993 F2-A", "DJ93F02a",
]


@pytest.mark.parametrize("fast,slow", FUNCS, ids=["convert", "pack", "unpack"])
def test_malformed_parity(fast, slow):
    rng = random.Random(20261118)
    assert_parity(mutations(SEED_DESIGNATIONS, 400, rng), fast, slow)


@pytest.mark.corpus
@pytest.mark.parametrize("fast,slow", FUNCS, ids=["convert", "pack", "unpack"])
def test_malformed_parity_corpus(fast, slow):
    if not os.path.exists(PROV):
        pytest.skip("test-data not available")
    rng = random.Random(7)
    seeds = []
    for path in (PROV, NUMBERED):
        u, p = load_pairs(path)
        idx = rng.sample(range(len(u)), 5000)
        seeds += [u[i] for i in idx] + [p[i] for i in idx]
    assert_parity(mutations(seeds, 4, rng), fast, slow)


# --------------------------------------------------------------------------
# API behavior
# --------------------------------------------------------------------------

def test_basic():
    np.testing.assert_array_equal(
        pack_array(["1995 XA", "2024 AB12", "1P", "C/1995 O1"]),
        ["J95X00A", "K24A12B", "0001P", "CJ95O010"])
    np.testing.assert_array_equal(
        unpack_array(["J95X00A", "_0Azzzz", "0003I"]),
        ["1995 XA", "2000 AL591673", "3I"])
    np.testing.assert_array_equal(
        convert_array(["1995 XA", "J95X00A"]), ["J95X00A", "1995 XA"])


def test_pass_through():
    np.testing.assert_array_equal(pack_array(["J95X00A", " 1995 XA "]), ["J95X00A", "J95X00A"])
    np.testing.assert_array_equal(unpack_array(["1995 XA", "J95X00A"]), ["1995 XA", "1995 XA"])


def test_dtypes_and_shape():
    s = pack_array(np.array([b"1995 XA", b"1P"]))
    assert s.dtype.kind == "S"
    assert s.tolist() == [b"J95X00A", b"0001P"]
    u = pack_array(np.array(["1995 XA", "1P"]))
    assert u.dtype.kind == "U" and u.dtype.itemsize // 4 == 7
    two_d = convert_array(np.array([["1995 XA", "J95X00A"], ["3I", "0003I"]]))
    assert two_d.shape == (2, 2)
    assert two_d.tolist() == [["J95X00A", "1995 XA"], ["0003I", "3I"]]
    big_endian = unpack_array(np.array(["J95X00A"], dtype=">U7"))
    assert big_endian.tolist() == ["1995 XA"]
    strided = np.array(["1995 XA", "junk", "1P", "junk"])[::2]
    assert pack_array(strided).tolist() == ["J95X00A", "0001P"]


def test_empty():
    assert pack_array([]).shape == (0,)
    assert pack_array(np.array([], dtype="S")).dtype.kind == "S"
    assert unpack_array(np.empty((0, 3), dtype="U5")).shape == (0, 3)


def test_errors_raise():
    with pytest.raises(MPCDesignationError, match=r"element 1 \('bad'\)"):
        pack_array(["1995 XA", "bad"])
    with pytest.raises(MPCDesignationError, match=r"element \(1, 0\)"):
        pack_array(np.array([["1995 XA", "1P"], ["nope", "1P"]]))
    assert issubclass(MPCDesignationError, ValueError)


def test_errors_coerce():
    out = pack_array(["1995 XA", "bad", "1995 Xé", None, b"1P", "", "1995\x00XA"],
                     errors="coerce")
    assert out.tolist() == ["J95X00A", "", "", "", "0001P", "", ""]


def test_bad_arguments():
    with pytest.raises(ValueError):
        pack_array(["1995 XA"], errors="ignore")
    with pytest.raises(TypeError):
        pack_array(np.array([1, 2, 3]))


def test_long_input_rejected():
    out = convert_array(["1" * 300, "1995 XA" + " " * 300], errors="coerce")
    assert out.tolist() == [reference(ref.convert_simple, "1" * 300),
                            reference(ref.convert_simple, "1995 XA" + " " * 300)]
