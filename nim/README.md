# MPC Designation Converter - Nim Implementation

High-performance Nim implementation of the MPC designation converter.

## Building

```bash
make build
```

## Running

```bash
./mpc_designation <designation> [designation ...]
```

Examples:
```bash
./mpc_designation 00001          # -> 1
./mpc_designation '1995 XA'      # -> J95X00A
./mpc_designation J95X00A        # -> 1995 XA
./mpc_designation 1P             # -> 0001P
```

## Testing

```bash
make test          # Run unit tests
make test-csv CSV=../test-data/prov_unpack_to_pack.csv    # Run CSV benchmark
make test-roundtrip CSV=../test-data/prov_unpack_to_pack.csv  # Run roundtrip test
```

## Performance

Tested with 2,021,090 entries:

| Operation | Time | Rate |
|-----------|------|------|
| Pack | 634ms | ~3,200,000/sec |
| Unpack | 261ms | ~7,700,000/sec |

## Supported Formats

- Permanent asteroids (packed/unpacked)
- Provisional asteroids (standard and old-style A/B prefix)
- Extended provisional (underscore format for cycle >= 620)
- Survey designations (P-L, T-1, T-2, T-3)
- Numbered comets
- Provisional comets (including fragments)
- Ancient comets (year < 1000)
- Natural satellites

## Limitations

- BCE years not supported (12 entries in test data)
- Old-style unpacked format (A908 CJ) outputs as modern format (1908 CJ)
