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

## Testing

```bash
# Run error handling tests
python test/test_errors.py ../test-data/error_test_cases.csv

# Run conversion tests (requires decompressing test data)
gunzip -k ../test-data/prov_unpack_to_pack.csv.gz
python test/test_csv.py ../test-data/prov_unpack_to_pack.csv
```

## Examples

See `examples/example_usage.py` for more detailed usage examples.

## Requirements

- Python 3.9+
- No external dependencies
