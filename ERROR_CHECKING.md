# MPC Designation Error Checking

This document describes the error checking and validation performed by the MPC designation converter implementations (C, Python, TCL).

## Overview

The converters validate input designations to ensure they conform to Minor Planet Center standards. Invalid inputs are rejected with descriptive error messages rather than producing garbage output.

## Validation Categories

### 1. Whitespace Validation

All implementations check for invalid whitespace characters:

| Check | C | Python | TCL | Description |
|-------|---|--------|-----|-------------|
| Tabs in designation | Yes | Yes | Yes | Tab characters (0x09) are rejected |
| Non-printable ASCII | Yes | Yes | Yes | Characters < 32 or > 126 are rejected |
| Consecutive spaces | Partial | Yes | Partial | Multiple spaces where single expected |
| Missing required space | Yes | Yes | Yes | Enforced by regex patterns |

**Note:** Leading/trailing whitespace is trimmed before validation in all implementations. The `validate_whitespace()` function checks the trimmed input.

### 2. Character Validation

| Check | C | Python | TCL | Description |
|-------|---|--------|-----|-------------|
| Control characters | Yes | Yes | Yes | ASCII 0-31 rejected |
| Delete character | Yes | Yes | Yes | ASCII 127 rejected |
| Extended ASCII (128-255) | Yes | Yes | Yes | Should be rejected |
| Null bytes | Partial | Yes | Yes | C strings truncate at null |
| Unicode characters | No | No | No | Currently accepted (known gap) |

### 3. Numeric Range Validation

| Check | C | Python | TCL | Description |
|-------|---|--------|-----|-------------|
| Asteroid number < 1 | Yes | Yes | Yes | Rejects 0 and negative |
| Asteroid number > 15,396,335 | Yes | Yes | Yes | Maximum encodable number |
| Year bounds | Partial | Partial | Partial | Validated by century codes |
| Cycle count overflow | No | Yes | Yes | C allows overflow |
| Comet order number = 0 | No | No | No | Should reject (known gap) |

**Maximum asteroid number:** 15,396,335 = 620,000 + 62^4 - 1 (tilde format maximum)

### 4. Format Validation

| Check | C | Python | TCL | Description |
|-------|---|--------|-----|-------------|
| Invalid century code | Yes | Yes | Yes | Only A-L valid |
| Invalid comet type | Yes | Yes | Yes | Only P, C, D, X, A, I valid |
| Invalid planet code | Yes | Yes | Yes | Only J, S, U, N valid |
| Invalid half-month letter | No | No | No | Letter I accepted (known gap) |
| Invalid survey code | Partial | Yes | Yes | Varies by implementation |
| Comet fragment validation | No | Partial | Partial | Numeric fragments accepted |

### 5. Pattern Matching

All implementations use strict regular expressions to match valid designation formats:

- **Packed permanent:** 5 or 6 characters (00001, A0001, ~aaaa)
- **Unpacked permanent:** Integer number
- **Packed provisional:** 7 characters (J95X00A)
- **Unpacked provisional:** "YYYY LL" or "YYYY LLnn" format
- **Packed comet:** 8 or 12 characters
- **Unpacked comet:** "T/YYYY Ln" format
- **Survey:** Number + survey code (2040 P-L)
- **Satellite:** S/YYYY P n format

## Test Infrastructure

### Test Files

| File | Purpose |
|------|---------|
| `error_test_cases.csv` | Error test cases with expected results |
| `test_errors.c` | C error test runner |
| `test_errors.py` | Python error test runner |
| `test_errors.tcl` | TCL error test runner |
| `test_csv.c` | C conversion test runner |
| `test_csv.py` | Python conversion test runner |
| `test_csv.tcl` | TCL conversion test runner |

### Running Tests

```bash
# Build and run C tests
make test-all

# Run individual error tests
./test_errors_c error_test_cases.csv
python3 test_errors.py error_test_cases.csv
tclsh test_errors.tcl error_test_cases.csv

# Run conversion tests (requires prov_unpack_to_pack.csv)
make test
python3 test_csv.py prov_unpack_to_pack.csv
tclsh test_csv.tcl prov_unpack_to_pack.csv
```

### Error Test Case Format

The `error_test_cases.csv` file contains test cases in this format:

```
category,subcategory,input,expected_error,description
```

- `category`: Error category (whitespace, invalid_char, out_of_bounds, format, edge_case, boundary)
- `subcategory`: Specific test identifier
- `input`: Test input (supports escape sequences like \t, \n, \x00)
- `expected_error`: "valid" for valid inputs, "format" or "range" for expected errors
- `description`: Human-readable description

## Known Validation Gaps

The following validations are currently missing or incomplete:

1. **Unicode handling:** Unicode characters outside ASCII are not consistently rejected
2. **Half-month letter I:** The letter 'I' is not used as a half-month designation but is currently accepted
3. **Comet order zero:** `C/1995 A0` should be rejected (order numbers start at 1)
4. **Satellite zero:** `S/2019 J 0` should be rejected (numbers start at 1)
5. **Survey zero:** `0 P-L` should be rejected (survey numbers are positive)
6. **Null bytes in C:** C string handling truncates at null bytes rather than rejecting

## Error Messages

### C Implementation

```c
const char *mpc_strerror(int err);
```

Error codes:
- `MPC_OK` (0): Success
- `MPC_ERR_NULL` (-1): Null pointer argument
- `MPC_ERR_FORMAT` (-2): Invalid format
- `MPC_ERR_RANGE` (-3): Number out of range
- `MPC_ERR_BUFFER` (-4): Buffer too small

### Python Implementation

```python
class MPCDesignationError(Exception):
    pass
```

Raises `MPCDesignationError` with descriptive message.

### TCL Implementation

Uses standard `error` command with descriptive message.

## Adding New Error Tests

When you encounter a new type of error that should be tested:

1. Add a line to `error_test_cases.csv` with the appropriate category
2. Use escape sequences for non-printable characters:
   - `\t` for tab
   - `\n` for newline
   - `\x00` for null byte
   - `\xFF` for hex value FF
3. Set `expected_error` to "valid", "format", or "range"
4. Run the error tests to verify

Example:
```csv
format,new_error_type,invalid input,format,Description of what this tests
```

## Performance Considerations

Error checking adds minimal overhead:
- Single-pass whitespace validation
- Regex pattern matching (compiled once)
- Range checks are simple integer comparisons

Benchmark results (2M entries):
- C: ~1.7M entries/sec
- Python: ~212K entries/sec
- TCL: ~51K entries/sec

## References

- [MPC Packed Designation Format](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
- [MPC Observatory Code Format](https://www.minorplanetcenter.net/iau/lists/ObsCodesF.html)
