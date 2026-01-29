# MPC Designations

TCL routines to convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

Based on the MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html

## Usage

```bash
./mpc_designation.tcl <designation> [designation ...]
./mpc_designation.tcl -v <designation>   # verbose output
```

The program auto-detects whether input is packed or unpacked and converts to the other format.

## Examples

```bash
# Asteroids - permanent (numbered)
./mpc_designation.tcl 1             # -> 00001
./mpc_designation.tcl A0001         # -> 100001
./mpc_designation.tcl '~0000'       # -> 620000

# Asteroids - provisional
./mpc_designation.tcl '1995 XA'     # -> J95X00A
./mpc_designation.tcl '2024 AA631'  # -> _4AMu1A (extended format)
./mpc_designation.tcl J95X00A       # -> 1995 XA

# Asteroids - survey
./mpc_designation.tcl '2040 P-L'    # -> 2040PLS
./mpc_designation.tcl '3138 T-1'    # -> 3138T1S

# Asteroids - old style (pre-1925)
./mpc_designation.tcl 'A908 CJ'     # -> J08C00J

# Comets
./mpc_designation.tcl 1P            # -> 0001P
./mpc_designation.tcl 'C/1995 O1'   # -> CJ95O010
./mpc_designation.tcl 'P/2019 A4'   # -> PK19A040
./mpc_designation.tcl 'D/1993 F2-B' # -> DJ93F02b (fragment)
./mpc_designation.tcl 'P/1930 J1-AA'# -> PJ30J01aa (2-letter fragment)

# Comets - ancient and BCE
./mpc_designation.tcl 'C/240 V1'    # -> C240V010
./mpc_designation.tcl 'C/-146 P1'   # -> C/46P010

# Natural satellites
./mpc_designation.tcl 'S/2019 S 22' # -> SK19S220

# Verbose mode
./mpc_designation.tcl -v '2024 AB'
#   Input:    2024 AB
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   K24A00B
```

## Supported Formats

### Asteroids
| Type | Unpacked | Packed | Range |
|------|----------|--------|-------|
| Permanent | 1 - 99999 | 00001 - 99999 | < 100,000 |
| Permanent | 100001 - 619999 | A0001 - z9999 | 100K - 620K |
| Permanent | 620000+ | ~0000+ | >= 620,000 |
| Provisional | 1995 XA | J95X00A | Standard |
| Provisional | 2024 AA631 | _4AMu1A | Cycle >= 620 |
| Survey | 2040 P-L | 2040PLS | P-L, T-1, T-2, T-3 |
| Old style | A908 CJ | J08C00J | Pre-1925 |

### Comets
| Type | Unpacked | Packed |
|------|----------|--------|
| Numbered periodic | 1P, 354P | 0001P, 0354P |
| Provisional | C/1995 O1 | CJ95O010 |
| With fragment | D/1993 F2-B | DJ93F02b |
| 2-letter fragment | P/1930 J1-AA | PJ30J01aa |
| Ancient (year < 1000) | C/240 V1 | C240V010 |
| BCE | C/-146 P1 | C/46P010 |

Comet type prefixes: P (periodic), C (non-periodic), D (defunct), X (uncertain), A (asteroid-like), I (interstellar)

### Natural Satellites
| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |

Planet codes: J (Jupiter), S (Saturn), U (Uranus), N (Neptune)

## Testing

A test dataset with 2,021,090 designation pairs is included:

```bash
gunzip -k prov_unpack_to_pack.csv.gz
tclsh test_csv.tcl prov_unpack_to_pack.csv
```

Expected output:
```
Total:  2021090
Passed: 2021090
Failed: 0
```

## License

Public domain.
