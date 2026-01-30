# MPC Designation Converter - Forth Implementation

A minimal Forth (gforth) implementation for converting between packed and unpacked Minor Planet Center (MPC) designations.

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
```

### Interactive Use

```forth
require src/mpc_designation.fs

s" 00001" convert-simple type cr    \ prints: 1
s" 1995 XA" convert-simple type cr  \ prints: J95X00A
```

## Supported Formats

This minimal implementation supports:

| Type | Packed | Unpacked |
|------|--------|----------|
| Permanent (< 100K) | 00001-99999 | 1-99999 |
| Permanent (100K-620K) | A0001-z9999 | 100001-619999 |
| Permanent (620K+) | ~0000+ | 620000+ |
| Provisional | J95X00A | 1995 XA |

## API

### Main Function

```forth
convert-simple ( addr len -- addr' len' )
```

Converts between packed and unpacked formats. Auto-detects input format.

### Direct Functions

```forth
pack-perm ( addr len -- addr' len' )      \ "1" -> "00001"
unpack-perm ( addr len -- addr' len' )    \ "00001" -> "1"
pack-prov ( addr len -- addr' len' )      \ "1995 XA" -> "J95X00A"
unpack-prov ( addr len -- addr' len' )    \ "J95X00A" -> "1995 XA"
```

## Testing

```bash
make test
```

## Limitations

This is a minimal implementation focused on core functionality:
- Numbered permanent asteroids (all ranges including ~xxxx format)
- Basic provisional designations with cycle counts

Not currently implemented:
- Survey designations (P-L, T-1, T-2, T-3)
- Comets
- Natural satellites
- Error handling for malformed input

## Files

- `src/mpc_designation.fs` - Main library
- `src/mpc_designation_cli.fs` - Command-line interface
- `test/test_errors.fs` - Basic tests
