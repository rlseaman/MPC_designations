# MPC Designation Converter - Forth Implementation

A full-featured Forth (gforth) implementation for converting between packed and unpacked Minor Planet Center (MPC) designations.

## Requirements

- gforth 0.7.3 or later

## Usage

### Command Line

```bash
gforth src/mpc_designation_cli.fs <designation> [designation ...]
```

### Examples

```bash
# Packed permanent asteroid -> unpacked
gforth src/mpc_designation_cli.fs 00001
# Output: 1

# Unpacked permanent asteroid -> packed
gforth src/mpc_designation_cli.fs 620000
# Output: ~0000

# Packed provisional -> unpacked
gforth src/mpc_designation_cli.fs J95X00A
# Output: 1995 XA

# Unpacked provisional -> packed
gforth src/mpc_designation_cli.fs '1995 XA'
# Output: J95X00A

# Survey designations
gforth src/mpc_designation_cli.fs '2040 P-L'
# Output: PLS2040

# Comets
gforth src/mpc_designation_cli.fs 1P 'C/1995 O1'
# Output: 0001P
# Output: CJ95O010

# Satellites
gforth src/mpc_designation_cli.fs 'S/2019 S 22'
# Output: SK19S220
```

### Interactive Use

```forth
require src/mpc_designation.fs

s" 00001" convert-simple type cr    \ prints: 1
s" 1995 XA" convert-simple type cr  \ prints: J95X00A
s" 1P" convert-simple type cr       \ prints: 0001P
```

## Supported Formats

| Type | Packed | Unpacked |
|------|--------|----------|
| Permanent (< 100K) | 00001-99999 | 1-99999 |
| Permanent (100K-620K) | A0001-z9999 | 100001-619999 |
| Permanent (620K+) | ~0000+ | 620000+ |
| Provisional | J95X00A | 1995 XA |
| Extended provisional | _OA004S | 2024 AB631 |
| Survey | PLS2040 | 2040 P-L |
| Numbered comet | 0001P | 1P |
| Comet provisional | CJ95O010 | C/1995 O1 |
| Comet fragment | DJ93F02b | D/1993 F2-B |
| Satellite | SK19S220 | S/2019 S 22 |

## API

### Main Function

```forth
convert-simple ( addr len -- addr' len' )
```

Converts between packed and unpacked formats. Auto-detects input format.

### Direct Functions

```forth
pack-perm ( addr len -- addr' len' )           \ "1" -> "00001"
unpack-perm ( addr len -- addr' len' )         \ "00001" -> "1"
pack-prov ( addr len -- addr' len' )           \ "1995 XA" -> "J95X00A"
unpack-prov ( addr len -- addr' len' )         \ "J95X00A" -> "1995 XA"
pack-comet-numbered ( addr len -- addr' len' ) \ "1P" -> "0001P"
unpack-comet-numbered ( addr len -- addr' len' ) \ "0001P" -> "1P"
pack-satellite ( addr len -- addr' len' )      \ "S/2019 S 22" -> "SK19S220"
unpack-satellite ( addr len -- addr' len' )    \ "SK19S220" -> "S/2019 S 22"
```

## Testing

```bash
make test
```

## Files

- `src/mpc_designation.fs` - Main library
- `src/mpc_designation_cli.fs` - Command-line interface
- `test/test_errors.fs` - Basic tests
