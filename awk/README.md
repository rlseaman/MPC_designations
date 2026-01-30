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
pack_numbered_comet(desig)    # "1P" -> "0001P"
unpack_numbered_comet(packed) # "0001P" -> "1P"
pack_provisional_comet(desig) # "C/1995 O1" -> "CJ95O010"
unpack_provisional_comet(packed) # "CJ95O010" -> "C/1995 O1"
pack_satellite(desig)         # "S/2019 S 22" -> "SK19S220"
unpack_satellite(packed)      # "SK19S220" -> "S/2019 S 22"
```

## Testing

```bash
make test
```

## Performance

AWK provides good performance (~200k entries/sec) while remaining portable across Unix systems.

## Limitations

- Extended provisional format (cycles >= 620) not implemented
- Some comet fragment edge cases may not be handled
- Ancient/BCE comet dates not implemented

## Files

- `src/mpc_designation.awk` - Main library (functions only)
- `src/mpc_designation_main.awk` - Main block for CLI use
- `src/mpc_designation_cli.sh` - Shell wrapper for CLI
- `test/test_errors.sh` - Test suite (36 test cases)
- `test/test_csv.awk` - CSV benchmark test
- `test/test_roundtrip.awk` - Roundtrip verification test
