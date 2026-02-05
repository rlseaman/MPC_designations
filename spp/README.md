# MPC Designation Converter - SPP/IRAF Implementation

Convert between packed and unpacked Minor Planet Center (MPC) designations
for asteroids, comets, and natural satellites using IRAF's SPP language.

## Quick Start

```spp
include "mpc_designation.x"

char    input[80], output[80]

call strcpy ("K24KG7S", input, 80)
call mpc_convert (input, output, 80)
# output now contains "2024 KS167"
```

## About SPP

SPP (Subset PreProcessor) is the systems programming language used in the
Image Reduction and Analysis Facility (IRAF). Key features:

- Fortran-like semantics with C-like syntax
- EOS-terminated character arrays (similar to C strings)
- 1-based array indexing
- Preprocessed to Ratfor, then to Fortran
- Designed for astronomical data reduction pipelines

## Files

```
spp/
├── src/
│   ├── mpc_designation.x    # Main library implementation (~1700 lines)
│   └── mpc_designation.h    # Header file with constants
├── cl/
│   ├── t_mpcdes.x           # CL task implementation
│   ├── mpcdes.par           # CL parameter file
│   ├── mpcpkg.cl            # Package definition script
│   └── README.md            # CL interface documentation
├── test/
│   ├── test_mpc.x           # Unit test suite (32 tests)
│   ├── test_errors.x        # Error handling tests (94 tests)
│   ├── test_helpers.x       # Helper function tests (77 tests)
│   └── test_csv.x           # CSV benchmark tests (2M+ tests)
├── build/                   # Build directory (generated)
├── example_usage.x          # Interactive example task
├── ARCHITECTURE.txt         # Code organization documentation
└── README.md                # This file
```

## Building within IRAF

SPP files are compiled using the IRAF `xc` compiler. From within the IRAF
cl environment:

```
cl> cd path/to/spp
cl> xc -c src/mpc_designation.x
```

To build a standalone task:

```
cl> xc example_usage.x src/mpc_designation.x -o mpcdes.e
```

Or using the mkpkg system (create a mkpkg file):

```
# mkpkg file example
libmpc.a:
    src/mpc_designation.x    <ctype.h>
    ;
```

## Usage

### As an IRAF CL Task (with parameters)

Build and install the CL task:

```bash
cd spp/cl
cp ../src/mpc_designation.x .
xc t_mpcdes.x mpc_designation.x -o mpcdes.e
```

From the CL environment:

```
cl> task mpcdes = "path/to/cl/mpcdes.e"

cl> mpcdes "1995 XA"
J95X00A

cl> mpcdes "J95X00A"
1995 XA

cl> mpcdes "C/1995 O1" verbose+
  Input:    C/1995 O1
  Detected: unpacked format, full comet
  Action:   packing to MPC compact form
  Output:   CJ95O010

cl> mpcdes @input.txt output=results.txt
```

See `cl/README.md` for full CL task documentation.

### Interactive Example Task

Build the interactive example:

```
cl> xc example_usage.x src/mpc_designation.x -o mpcdes_interactive.e
cl> task mpcdes_i = "path/to/mpcdes_interactive.e"
cl> mpcdes_i
MPC Designation Converter
Enter a designation to convert, 'q' to quit.

des> K24KG7S
Packed 'K24KG7S' -> Unpacked '2024 KS167'

des> q
Goodbye.
```

### In SPP Source Code

Include the library in your SPP task:

```spp
include "mpc_designation.x"

procedure my_task ()

char    input[80], output[80]

begin
    call strcpy ("K24KG7S", input, 80)
    call mpc_convert (input, output, 80)
    # output now contains "2024 KS167"

    call printf ("Result: %s\n")
        call pargstr (output)
end
```

### Main Entry Point

**mpc_convert** - Auto-detect format and convert:
```spp
call mpc_convert (input, output, maxch)
```

### Detection Routine

**mpc_detect** - Detect format and type:
```spp
call mpc_detect (des, format, dtype)
# format: 1=packed, 2=unpacked, 0=unknown
# dtype: 1=permanent, 2=provisional, 3=survey,
#        4=comet_numbered, 5=comet_provisional,
#        6=comet_full, 7=satellite, 8=extended
```

### Individual Conversion Functions

```spp
# Permanent (numbered) asteroids
call mpc_unperm (packed, unpacked, maxch)
call mpc_pkperm (unpacked, packed, maxch)

# Provisional designations
call mpc_unprov (packed, unpacked, maxch)
call mpc_pkprov (unpacked, packed, maxch)

# Extended provisional (cycle >= 620)
call mpc_unext (packed, unpacked, maxch)
call mpc_pkext (year, halfmo, seclet, cycle, packed, maxch)

# Survey designations (P-L, T-1, T-2, T-3)
call mpc_unsurv (packed, unpacked, maxch)
call mpc_pksurv (unpacked, packed, maxch)

# Numbered comets (with optional fragment)
call mpc_uncnum (packed, unpacked, maxch)
call mpc_pkcnum (unpacked, packed, maxch)

# Comet provisional
call mpc_uncprv (packed, unpacked, maxch)

# Full comet designations
call mpc_uncful (packed, unpacked, maxch)
call mpc_pkcful (unpacked, packed, maxch)

# Natural satellites
call mpc_unsat (packed, unpacked, maxch)
call mpc_pksat (unpacked, packed, maxch)
```

### Helper Functions

```spp
# Convert minimal packed format to 12-character report format
call mpc_torep (minimal, report, maxch)
# "0073Pa" -> "0073P      a"
# "00001"  -> "       00001"

# Convert 12-character report format to minimal packed format
call mpc_fromrep (report, minimal, maxch)
# "0073P      a" -> "0073Pa"
# "       00001" -> "00001"

# Check if designation has a comet fragment
bool mpc_hasfrag (desig)
# "73P-A" -> true, "73P" -> false

# Extract fragment suffix (returns uppercase)
call mpc_getfrag (desig, frag, maxch)
# "73P-A" -> "A", "73P-AA" -> "AA"

# Get parent comet without fragment
call mpc_getpar (desig, parent, maxch)
# "73P-A" -> "73P", "0073Pa" -> "0073P"

# Check if two designations refer to the same object
bool mpc_deseq (d1, d2)
# "73P-A", "0073Pa" -> true (same object)
# "73P-A", "73P-B" -> false (different)
```

## Supported Formats

### Asteroids

| Type | Packed | Unpacked |
|------|--------|----------|
| Numbered (< 100000) | `00001` | `1` |
| Numbered (100000-619999) | `A0000` | `100000` |
| Numbered (>= 620000) | `~0000` | `620000` |
| Provisional | `K24KG7S` | `2024 KS167` |
| Extended provisional | `_OA004S` | `2024 AB631` |

### Surveys

| Type | Packed | Unpacked |
|------|--------|----------|
| Palomar-Leiden | `PLS2040` | `2040 P-L` |
| Trojan-1 | `T1S2040` | `2040 T-1` |
| Trojan-2 | `T2S3138` | `3138 T-2` |
| Trojan-3 | `T3S1000` | `1000 T-3` |

### Comets

| Type | Packed | Unpacked |
|------|--------|----------|
| Numbered | `0001P` | `1P` |
| Numbered with fragment | `0073Pa` | `73P-A` |
| Numbered with 2-letter fragment | `0073Paa` | `73P-AA` |
| Provisional | `CJ95O010` | `C/1995 O1` |
| With fragment | `DJ93F02b` | `D/1993 F2-B` |
| Two-letter fragment | `PJ30J01aa` | `P/1930 J1-AA` |
| Ancient (year < 1000) | `C574A010` | `C/574 A1` |
| BCE (year < 0) | `C/49A010` | `C/-50 A1` |

### Natural Satellites

| Packed | Unpacked |
|--------|----------|
| `SK19S220` | `S/2019 S 22` |
| `SK00J010` | `S/2000 J 1` |

## Testing

### Building the Tests

From the build directory:

```bash
cd spp/build
cp ../src/mpc_designation.x .
cp ../test/test_mpc.x .
cp ../test/test_errors.x .
cp ../test/test_helpers.x .
cp ../test/test_csv.x .

xc test_mpc.x mpc_designation.x -o test_mpc.e
xc test_errors.x mpc_designation.x -o test_errors.e
xc test_helpers.x mpc_designation.x -o test_helpers.e
xc test_csv.x mpc_designation.x -o test_csv.e
```

### Running the Tests

Unit tests (32 tests):
```bash
./test_mpc.e test_mpc
```

Error handling tests (94 tests, paths are hardcoded relative to build/):
```bash
./test_errors.e test_errors
```

Helper function tests (77 tests):
```bash
./test_helpers.e test_helpers
```

CSV benchmark (2M+ conversions, path hardcoded relative to build/):
```bash
./test_csv.e test_csv
```

### Test Results

- **Unit tests**: 32 passed, 0 failed
- **Helper tests**: 77 passed, 0 failed
- **Error tests**: 92 passed, 2 failed
  - The 2 failures are null byte tests (documented limitation of EOS-terminated strings)
- **CSV benchmark**: 2,022,404 passed, 0 failed
  - Execution time: ~1.0 second for 2M+ conversions

All conversion tests pass, including:
- Standard provisional designations
- Old-style provisional designations (A/B prefix from 1800s-1900s)
- Ancient comets (year < 1000 CE)
- BCE comets (year < 0)
- Two-letter comet fragments (e.g., -AA, -AB)

Note: Embedded null bytes cannot be reliably detected in EOS-terminated (null-terminated)
strings since the null terminates the string before scanning can reach it.

## Performance

- **CSV benchmark**: 2,022,404 conversions in ~1.0 second
- **Throughput**: ~2 million conversions/second

SPP compiles to Fortran 77, giving performance comparable to compiled Fortran.

## Notes

- This implementation follows the MPC specification at:
  https://www.minorplanetcenter.net/iau/info/PackedDes.html

- Compiled with IRAF's xc compiler (preprocessor chain: xpp -> rpp -> f77)

- Character arrays use EOS (null) termination

- All array indices are 1-based per SPP/Fortran convention

- The "identifier mapping not unique" warnings during compilation are
  expected due to Fortran 77's 6-character identifier limit

## Requirements

- IRAF 2.16+ or iraf-community distribution
- xc compiler (included with IRAF)
- No external dependencies

## Architecture

This implementation is a standalone SPP library (no CL parameter interface).
See `ARCHITECTURE.txt` for detailed documentation of the code organization,
exported procedures, and design decisions.

## See Also

- [IRAF SPP Reference Manual](https://iraf.readthedocs.io/en/latest/spp.html)
- [MPC Packed Designation Format](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
- [IRAF Community](https://iraf-community.github.io/)
