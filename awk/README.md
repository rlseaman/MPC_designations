# MPC Designation Converter - AWK Implementation

A pure AWK implementation for converting between packed and unpacked Minor Planet Center (MPC) designations.

## Requirements

- AWK (gawk, mawk, or POSIX awk)

## Usage

### Command Line

```bash
./src/mpc_designation_cli.sh <designation> [designation ...]
```

Or with AWK directly:

```bash
echo "1995 XA" | awk -f src/mpc_designation.awk -f src/mpc_designation_main.awk
```

### Examples

```bash
# Asteroids
./src/mpc_designation_cli.sh 00001        # -> 1
./src/mpc_designation_cli.sh '1995 XA'    # -> J95X00A
./src/mpc_designation_cli.sh A0001        # -> 100001

# Surveys
./src/mpc_designation_cli.sh '2040 P-L'   # -> PLS2040

# Comets
./src/mpc_designation_cli.sh 1P           # -> 0001P
./src/mpc_designation_cli.sh 'C/1995 O1'  # -> CJ95O010

# Natural satellites
./src/mpc_designation_cli.sh 'S/2019 S 22' # -> SK19S220
```

### As a Library

```awk
# Include in your AWK script
@include "mpc_designation.awk"

{
    result = convert_simple($1)
    print result
}
```

## Supported Formats

| Type | Packed | Unpacked |
|------|--------|----------|
| Permanent asteroid (< 100K) | 00001-99999 | 1-99999 |
| Permanent asteroid (100K-620K) | A0001-z9999 | 100001-619999 |
| Permanent asteroid (620K+) | ~0000+ | 620000+ |
| Provisional asteroid | J95X00A | 1995 XA |
| Survey | PLS2040 | 2040 P-L |
| Numbered comet | 0001P | 1P |
| Provisional comet | CJ95O010 | C/1995 O1 |
| Natural satellite | SK19S220 | S/2019 S 22 |

## API

### Main Function

```awk
convert_simple(designation)
# Returns converted designation string
```

### Direct Functions

```awk
pack_permanent(num)           # 1 -> "00001"
unpack_permanent(packed)      # "00001" -> 1
pack_provisional(desig)       # "1995 XA" -> "J95X00A"
unpack_provisional(packed)    # "J95X00A" -> "1995 XA"
pack_survey(desig)            # "2040 P-L" -> "PLS2040"
unpack_survey(packed)         # "PLS2040" -> "2040 P-L"
pack_numbered_comet(desig)    # "1P" -> "0001P", "73P-A" -> "0073Pa"
unpack_numbered_comet(packed) # "0001P" -> "1P", "0073Pa" -> "73P-A"
pack_provisional_comet(desig) # "C/1995 O1" -> "CJ95O010"
unpack_provisional_comet(packed) # "CJ95O010" -> "C/1995 O1"
pack_satellite(desig)         # "S/2019 S 22" -> "SK19S220"
unpack_satellite(packed)      # "SK19S220" -> "S/2019 S 22"
```

### Helper Functions

```awk
to_report_format(minimal)     # "0073Pa" -> "0073P      a"
from_report_format(report)    # "0073P      a" -> "0073Pa"
has_fragment(desig)           # "73P-A" -> 1, "73P" -> 0
get_fragment(desig)           # "73P-A" -> "A", "73P-AA" -> "AA"
get_parent(desig)             # "73P-A" -> "73P"
designations_equal(d1, d2)    # "73P-A", "0073Pa" -> 1
```

## Helper Function Examples

### Format Conversion

```awk
to_report_format("0073Pa")   # "0073P      a"
to_report_format("00001")    # "       00001"

from_report_format("0073P      a")  # "0073Pa"
from_report_format("       00001")  # "00001"
```

### Fragment Handling

```awk
has_fragment("73P-A")   # 1 (true)
has_fragment("73P")     # 0 (false)

get_fragment("73P-A")   # "A"
get_fragment("73P-AA")  # "AA"

get_parent("73P-A")     # "73P"
get_parent("0073Pa")    # "0073P"
```

### Designation Comparison

```awk
designations_equal("1995 XA", "J95X00A")  # 1 (same object)
designations_equal("73P-A", "0073Pa")     # 1 (same object)
designations_equal("73P-A", "73P-B")      # 0 (different)
```

## Testing

```bash
make test
```

## Performance

AWK provides good performance (~200k entries/sec) while remaining portable across Unix systems.

## Limitations

- Extended provisional format (cycles >= 620) not implemented

## Files

- `src/mpc_designation.awk` - Main library (functions only)
- `src/mpc_designation_main.awk` - Main block for CLI use
- `src/mpc_designation_cli.sh` - Shell wrapper for CLI
- `test/test_helpers.awk` - Helper function tests (77 test cases)
- `test/test_errors.sh` - Error handling tests
- `test/test_csv.awk` - CSV benchmark test (2M+ entries)
- `test/test_roundtrip.awk` - Roundtrip verification test
