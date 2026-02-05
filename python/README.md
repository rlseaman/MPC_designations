# MPC Designation Converter - Python Implementation

Python 3 library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```python
from mpc_designation import convert_simple

result = convert_simple('1995 XA')  # Returns 'J95X00A'
result = convert_simple('J95X00A')  # Returns '1995 XA'
```

## Installation

### From Source

```bash
cd python
pip install -e .
```

Or simply copy `src/mpc_designation/` to your project.

## CLI Usage

```bash
# Single conversion
python -m mpc_designation '1995 XA'
# Output: J95X00A

# Multiple designations
python -m mpc_designation 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
python -m mpc_designation -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

Or run directly:

```bash
python src/mpc_designation/mpc_designation.py '1995 XA'
```

## Library Usage

### Simple Conversion

```python
from mpc_designation import convert_simple, MPCDesignationError

try:
    packed = convert_simple('1995 XA')  # Returns 'J95X00A'
    unpacked = convert_simple('J95X00A')  # Returns '1995 XA'
except MPCDesignationError as e:
    print(f"Error: {e}")
```

### With Format Information

```python
from mpc_designation import convert

result = convert('1995 XA')
print(result['input'])   # '1995 XA'
print(result['output'])  # 'J95X00A'
print(result['format'])  # 'unpacked'
print(result['subtype']) # 'provisional asteroid'
```

### Format Detection

```python
from mpc_designation import detect_format

info = detect_format('1995 XA')
print(info['format'])  # 'unpacked'
print(info['type'])    # 'provisional'
```

### Batch Processing

```python
from mpc_designation import convert_simple, MPCDesignationError

designations = ['1', '1995 XA', 'C/1995 O1']
for des in designations:
    try:
        result = convert_simple(des)
        print(f"{des} -> {result}")
    except MPCDesignationError as e:
        print(f"{des}: Error - {e}")
```

## API Reference

### Functions

#### `convert_simple(designation: str) -> str`

Convert a designation, returning just the result string.

- **Parameters:** `designation` - The MPC designation to convert
- **Returns:** The converted designation
- **Raises:** `MPCDesignationError` if the input is invalid

#### `convert(designation: str) -> dict`

Convert a designation, returning detailed information.

- **Parameters:** `designation` - The MPC designation to convert
- **Returns:** Dictionary with keys:
  - `input`: Original input
  - `output`: Converted designation
  - `format`: 'packed' or 'unpacked'
  - `subtype`: Human-readable description

#### `detect_format(designation: str) -> dict`

Detect the format of a designation without converting.

- **Parameters:** `designation` - The MPC designation to analyze
- **Returns:** Dictionary with format information

### Exceptions

#### `MPCDesignationError`

Raised when a designation cannot be converted.

```python
from mpc_designation import MPCDesignationError

try:
    convert_simple('invalid')
except MPCDesignationError as e:
    print(f"Invalid designation: {e}")
```

## Comet Fragment Handling

The library supports comet fragment designations:

### Numbered Comets with Fragments

Numbered comets (like 73P) can have fragments:

```python
convert_simple('73P-A')    # Returns '0073Pa'
convert_simple('73P-AA')   # Returns '0073Paa'
convert_simple('0073Pa')   # Returns '73P-A'
convert_simple('0073Paa')  # Returns '73P-AA'
```

### Provisional Comets with Fragments

Provisional comets can also have fragments:

```python
convert_simple('P/1930 J1-A')   # Returns 'PJ30J01a'
convert_simple('P/1930 J1-AA')  # Returns 'PJ30J01aa'
convert_simple('PJ30J01aa')     # Returns 'P/1930 J1-AA'
```

Fragment letters include all A-Z (including I, per MPC data).

## Pre-1925 Designations

For years before 1925, the library outputs A-prefix format per MPC convention:

```python
convert_simple('I01A00A')  # Returns 'A801 AA' (not '1801 AA')
convert_simple('J08C00J')  # Returns 'A908 CJ' (not '1908 CJ')
convert_simple('A908 CJ')  # Returns 'J08C00J'
```

The A-prefix format is the MPC-assigned primary designation for pre-1925 objects.

## Century Code Validation

The library validates century codes:

- **Asteroids**: Only I-L (years 1800-2199)
- **Comets**: A-L (years 1000-2199, including historical comets)

```python
convert_simple('1800 AA')    # Valid - minimum asteroid year
convert_simple('C/1014 C1')  # Valid - historical comet from 1014 CE
convert_simple('1700 AA')    # Error - year before 1800 for asteroids
```

## Testing

```bash
# Run error handling tests
python test/test_errors.py ../test-data/error_test_cases.csv

# Run fragment handling tests
python test/test_fragments.py

# Run conversion tests (requires decompressing test data)
gunzip -k ../test-data/prov_unpack_to_pack.csv.gz
python test/test_csv.py ../test-data/prov_unpack_to_pack.csv
```

## Examples

See `examples/example_usage.py` for more detailed usage examples.

## Requirements

- Python 3.9+
- No external dependencies
