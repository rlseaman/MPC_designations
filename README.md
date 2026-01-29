# MPC Designations

Convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

Based on the MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html

## Implementations

| Language | Directory | Status |
|----------|-----------|--------|
| **C** | [`c/`](c/) | Production |
| **Python** | [`python/`](python/) | Production |
| **TCL** | [`tcl/`](tcl/) | Production |

All implementations pass the same test suite and produce identical results.

## Quick Start

### C
```bash
cd c && make
./mpc_designation '1995 XA'    # Output: J95X00A
```

### Python
```python
from mpc_designation import convert_simple
convert_simple('1995 XA')  # Returns 'J95X00A'
```

### TCL
```tcl
source mpc_designation.tcl
MPCDesignation::convertSimple "1995 XA"  ;# Returns "J95X00A"
```

## Examples

```bash
# Asteroids - permanent (numbered)
1             -> 00001
100001        -> A0001
620000        -> ~0000

# Asteroids - provisional
1995 XA       -> J95X00A
2024 AB631    -> _4AMu1B

# Asteroids - survey
2040 P-L      -> PLS2040

# Comets
1P            -> 0001P
C/1995 O1     -> CJ95O010
D/1993 F2-B   -> DJ93F02b

# Natural satellites
S/2019 S 22   -> SK19S220
```

## Project Structure

```
MPC_designations/
├── README.md           # This file
├── VERSION             # Version number (unified across languages)
├── LICENSE             # Public domain
├── CONTRIBUTING.md     # How to add new languages
├── docs/
│   ├── SPECIFICATION.md    # MPC format reference
│   ├── FORMATS.md          # Quick reference tables
│   └── ERROR_CHECKING.md   # Validation documentation
├── test-data/
│   ├── prov_unpack_to_pack.csv.gz  # 2M+ test cases
│   └── error_test_cases.csv         # Error handling tests
├── c/
│   ├── README.md       # C documentation
│   ├── Makefile
│   ├── src/            # Source code
│   ├── test/           # Test files
│   └── examples/       # Usage examples
├── python/
│   ├── README.md       # Python documentation
│   ├── pyproject.toml
│   ├── src/            # Source code
│   ├── test/           # Test files
│   └── examples/       # Usage examples
└── tcl/
    ├── README.md       # TCL documentation
    ├── src/            # Source code
    ├── test/           # Test files
    └── examples/       # Usage examples
```

## Testing

Each implementation includes tests against 2+ million known-good conversions and error handling tests.

```bash
# C
cd c && make test-all

# Python
cd python
python test/test_errors.py ../test-data/error_test_cases.csv

# TCL
cd tcl
tclsh test/test_errors.tcl ../test-data/error_test_cases.csv
```

## Sparse Checkout

Download only the language you need:

```bash
git clone --filter=blob:none --sparse https://github.com/rlseaman/MPC_designations.git
cd MPC_designations
git sparse-checkout set python test-data docs
```

## Supported Formats

### Asteroids
| Type | Unpacked | Packed |
|------|----------|--------|
| Permanent (< 100K) | 1 - 99999 | 00001 - 99999 |
| Permanent (100K-620K) | 100001 - 619999 | A0001 - z9999 |
| Permanent (620K+) | 620000+ | ~0000+ |
| Provisional | 1995 XA | J95X00A |
| Survey | 2040 P-L | PLS2040 |

### Comets
| Type | Unpacked | Packed |
|------|----------|--------|
| Numbered | 1P | 0001P |
| Provisional | C/1995 O1 | CJ95O010 |
| Fragment | D/1993 F2-B | DJ93F02b |
| Ancient | C/240 V1 | C240V010 |
| BCE | C/-146 P1 | C.53P010 |

### Natural Satellites
| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |

## Documentation

- [Specification](docs/SPECIFICATION.md) - MPC format specification reference
- [Format Tables](docs/FORMATS.md) - Quick reference for all formats
- [Error Checking](docs/ERROR_CHECKING.md) - Validation documentation

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on adding new language implementations.

## License

Public domain. See [LICENSE](LICENSE).
