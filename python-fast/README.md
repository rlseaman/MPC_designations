# mpc-designation-fast

Fast, vectorized conversion of Minor Planet Center designations for NumPy
arrays, backed by the project's C library. It is a companion to the
pure-Python [`mpc-designation`](https://pypi.org/project/mpc-designation/)
package and gives the same results, element for element, about 100× faster
on large arrays.

```bash
pip install mpc-designation-fast
```

Pre-built wheels cover CPython 3.9+ on Linux, macOS and Windows; one wheel per
platform serves every Python version (stable ABI). NumPy and `mpc-designation`
are installed as dependencies.

## Usage

```python
import numpy as np
from mpc_designation_fast import convert_array, pack_array, unpack_array

pack_array(["1995 XA", "2024 AB12", "1P", "C/1995 O1"])
# array(['J95X00A', 'K24A12B', '0001P', 'CJ95O010'], dtype='<U8')

unpack_array(np.array([b"J95X00A", b"_0Azzzz"]))
# array([b'1995 XA', b'2000 AL591673'], dtype='|S13')

convert_array(["1995 XA", "J95X00A"])        # auto-detects each direction
# array(['J95X00A', '1995 XA'], dtype='<U7')
```

| Function | Like (per element) | Already-converted input |
|---|---|---|
| `convert_array(a)` | `mpc_designation.convert_simple` | converted the other way |
| `pack_array(a)` | `mpc_designation.pack` | packed input passes through |
| `unpack_array(a)` | `mpc_designation.unpack` | unpacked input passes through |

- **Input:** a NumPy array of `str` (`U`) or `bytes` (`S`), or any sequence of
  `str`. Any shape works, and the result has the same shape.
- **Output:** bytes in gives bytes out; anything else gives `str`. The dtype is
  trimmed to the longest result.
- **Errors:** `errors="raise"` (the default) raises
  `mpc_designation.MPCDesignationError` for the first invalid element, giving
  its index and the reference implementation's message. `errors="coerce"`
  turns invalid elements into `""`, so `result == ""` is the failure mask:

  ```python
  out = pack_array(designations, errors="coerce")
  bad = designations[out == ""]
  ```

The conversion loop releases the GIL, so threads can convert separate chunks
in parallel.

Limits: results longer than 31 characters are reported as errors. That only
affects `pack_array`/`unpack_array` echoing absurd already-converted input
(for example a 40-digit "number"), which `mpc_designation.pack`/`unpack`
return unchanged.

## Performance

Measured on Apple M1 Max, converting the project's 2,110,817-designation
provisional corpus (`U` arrays in and out):

| | Time | Per designation |
|---|---:|---:|
| `mpc_designation.pack` in a Python loop | ≈ 9 s | ≈ 4.3 µs |
| `pack_array` (`U` array) | 0.13 s | 63 ns |
| `unpack_array` (`U` array) | 0.10 s | 47 ns |
| `pack_array` / `unpack_array` (`S` array) | ≈ 0.09 s | 43–45 ns |

## Correctness

The test suite checks `mpc_designation_fast` against the pure-Python
reference on the full MPC-derived corpus (2.1M provisional and 0.9M numbered
pairs, both directions), the shared 140-case error suite, and about 100,000
mutated malformed inputs in each mode. The underlying C library was also
checked against the reference on 5.5 million mutated and systematically
generated inputs.

## License

CC0-1.0, like the rest of
[MPC_designations](https://github.com/rlseaman/MPC_designations).
