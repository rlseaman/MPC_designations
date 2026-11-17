# Test Data

This directory contains shared test data used by all language implementations.

## Files

### prov_unpack_to_pack.csv.gz

Compressed CSV file containing 2,110,817 known-good provisional designation
conversion pairs: asteroid provisionals (standard, extended `_YHbbbb`,
pre-1925 A-prefix), survey designations, and comet provisionals (modern,
ancient, BCE, fragments, and asteroid-style including the extended form, e.g.
`P/2025 OZ695,P_PO00Ud`). Since 1.1.3 it also carries the numbered
interstellar objects (`1I,0001I` … `3I,0003I`, `1I/2017 U1,0001IK17U010`),
whose packed forms were confirmed from MPC observation records.

Format: `unpacked,packed` (one header row)

Example:
```
1995 XA,J95X00A
2024 AB,K24A00B
```

To decompress:
```bash
gunzip -k prov_unpack_to_pack.csv.gz
```

### numbered_pack.csv.gz

Compressed CSV file containing 899,365 numbered-asteroid pairs: every numbered
minor planet in the MPC catalog at the time of the refresh (1-895,910) plus
synthetic boundary cases across all three encoding tiers up to the maximum
15,396,335 (`~zzzz`). Same `unpacked,packed` format.

### Provenance and refresh

Both corpora were refreshed on 2026-11-17 from a replica of the MPC
PostgreSQL database (`mpc_sbn`), using the `primary_objects`,
`current_identifications`, `numbered_identifications`, and `comet_names`
tables. The provisional file is the union of the new export and the previous
(2026-02-04) corpus; 237 older pairs no longer present in the MPC tables were
retained. Every pair was verified against the Python reference in both
directions before inclusion.

The source extracts (`primary_objects.csv`, `secondary_designations.csv`,
`comet_names.csv`) are kept locally and are git-ignored.

Test harnesses run the pack direction (unpacked to packed). To exercise the
unpack direction, swap the two columns.

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
