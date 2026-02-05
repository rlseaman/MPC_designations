# MPC Designation Converter - Bash Implementation

A pure Bash implementation for converting between packed and unpacked Minor Planet Center (MPC) designations.

## Requirements

- Bash 3.2+ (macOS default)

## Usage

### Command Line

```bash
./src/mpc_designation.sh <designation> [designation ...]
```

### Examples

```bash
# Asteroids
./src/mpc_designation.sh 00001        # -> 1
./src/mpc_designation.sh '1995 XA'    # -> J95X00A
./src/mpc_designation.sh A0001        # -> 100001

# Surveys
./src/mpc_designation.sh '2040 P-L'   # -> PLS2040

# Comets
./src/mpc_designation.sh 1P           # -> 0001P
./src/mpc_designation.sh 'C/1995 O1'  # -> CJ95O010

# Comets with fragments
./src/mpc_designation.sh '73P-A'      # -> 0073Pa
./src/mpc_designation.sh 0073Paa      # -> 73P-AA

# Natural satellites
./src/mpc_designation.sh 'S/2019 S 22' # -> SK19S220
```

### As a Library

```bash
source src/mpc_designation.sh

result=$(convert_simple "1995 XA")
echo "$result"  # J95X00A
```

## Supported Formats

| Type | Packed | Unpacked |
|------|--------|----------|
| Permanent asteroid (< 100K) | 00001-99999 | 1-99999 |
| Permanent asteroid (100K-620K) | A0001-z9999 | 100001-619999 |
| Permanent asteroid (620K+) | ~0000+ | 620000+ |
| Provisional asteroid | J95X00A | 1995 XA |
| Pre-1925 provisional | J08C00J | A908 CJ |
| Survey | PLS2040 | 2040 P-L |
| Numbered comet | 0001P | 1P |
| Numbered comet with fragment | 0073Pa, 0073Paa | 73P-A, 73P-AA |
| Provisional comet | CJ95O010 | C/1995 O1 |
| Provisional comet with fragment | DJ93F02b | D/1993 F2-B |
| Natural satellite | SK19S220 | S/2019 S 22 |

## API

### Main Function

```bash
convert_simple "designation"
# Outputs converted designation to stdout
```

### Direct Functions

```bash
pack_permanent "1"           # -> 00001
unpack_permanent "00001"     # -> 1
pack_provisional "1995 XA"   # -> J95X00A
unpack_provisional "J95X00A" # -> 1995 XA
pack_survey "2040 P-L"       # -> PLS2040
unpack_survey "PLS2040"      # -> 2040 P-L
pack_numbered_comet "1P"     # -> 0001P
pack_numbered_comet "73P-A"  # -> 0073Pa
unpack_numbered_comet "0001P" # -> 1P
unpack_numbered_comet "0073Pa" # -> 73P-A
pack_provisional_comet "C/1995 O1" # -> CJ95O010
unpack_provisional_comet "CJ95O010" # -> C/1995 O1
pack_satellite "S/2019 S 22" # -> SK19S220
unpack_satellite "SK19S220"  # -> S/2019 S 22
```

### Helper Functions

```bash
# Convert minimal packed format to 12-character MPC report format
to_report_format "0073Pa"      # -> "0073P      a"
to_report_format "00001"       # -> "       00001"

# Convert 12-character report format to minimal packed format
from_report_format "0073P      a"  # -> "0073Pa"
from_report_format "       00001"  # -> "00001"

# Check if designation has a comet fragment suffix
has_fragment "73P-A"    # returns 0 (true)
has_fragment "73P"      # returns 1 (false)

# Extract fragment suffix (uppercase, e.g., "A", "AA")
get_fragment "73P-A"    # -> "A"
get_fragment "0073Paa"  # -> "AA"

# Get parent comet designation without fragment suffix
get_parent "73P-A"      # -> "73P"
get_parent "0073Pa"     # -> "0073P"

# Check if two designations refer to the same object
designations_equal "73P" "0073P"     # returns 0 (true)
designations_equal "73P-A" "0073Pa"  # returns 0 (true)
designations_equal "73P" "74P"       # returns 1 (false)
```

## Testing

```bash
# Run error handling tests (36 cases)
make test-errors

# Run helper function tests (77 cases)
make test-helpers

# Run full CSV test suite (2M+ tests)
make test-csv

# Run round-trip tests
make test-roundtrip

# Run all tests
make test-all
```

## Limitations

- Extended provisional format (cycles >= 620) not implemented
- Ancient/BCE comet dates not implemented
- Limited error handling for malformed input

## Files

- `src/mpc_designation.sh` - Main library and CLI
- `test/test_errors.sh` - Error handling tests (36 cases)
- `test/test_helpers.sh` - Helper function tests (77 cases)
- `test/test_csv.sh` - CSV test suite
- `test/test_roundtrip.sh` - Round-trip tests

## License

CC0 1.0 Universal - Public Domain Dedication
