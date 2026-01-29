# Test Data

This directory contains shared test data used by all language implementations.

## Files

### prov_unpack_to_pack.csv.gz

Compressed CSV file containing 2,021,090 known-good designation conversion pairs.

Format: `unpacked,packed`

Example:
```
1995 XA,J95X00A
2024 AB,K24A00B
```

To decompress:
```bash
gunzip -k prov_unpack_to_pack.csv.gz
```

### error_test_cases.csv

Test cases for error handling validation. Contains both valid edge cases and invalid inputs that should produce errors.

Format: `category,subcategory,input,expected_error,description`

Example:
```csv
whitespace,tab,1995\tXA,format,Tab character in designation
boundary,max_asteroid,15396335,valid,Maximum encodable asteroid number
```

The `expected_error` field is either:
- `valid` - The input should convert successfully
- `format` - The input should produce a format error
- `range` - The input should produce a range error

Escape sequences in the `input` field:
- `\t` - tab
- `\n` - newline
- `\r` - carriage return
- `\x00` - null byte
- `\xFF` - hex value FF

## Usage

All test runners accept the CSV file path as a command-line argument:

```bash
# C
cd c && ./test_csv ../test-data/prov_unpack_to_pack.csv
cd c && ./test_errors ../test-data/error_test_cases.csv

# Python
cd python && python test/test_csv.py ../test-data/prov_unpack_to_pack.csv
cd python && python test/test_errors.py ../test-data/error_test_cases.csv

# TCL
cd tcl && tclsh test/test_csv.tcl ../test-data/prov_unpack_to_pack.csv
cd tcl && tclsh test/test_errors.tcl ../test-data/error_test_cases.csv
```
