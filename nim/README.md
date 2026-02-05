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
# Run all tests
make test-all

# Run specific tests
make test-errors    # Error handling tests (94 cases)
make test-helpers   # Helper function tests (77 cases)
make test-csv CSV=../test-data/prov_unpack_to_pack.csv    # Conversion tests (2M+ entries)
make test-roundtrip CSV=../test-data/prov_unpack_to_pack.csv  # Roundtrip verification
```

## Performance

Tested with 2,022,404 entries:

| Operation | Time | Rate |
|-----------|------|------|
| Pack | 634ms | ~3,200,000/sec |
| Unpack | 261ms | ~7,700,000/sec |

## Supported Formats

- Permanent asteroids (packed/unpacked)
- Provisional asteroids (standard and old-style A/B prefix)
- Extended provisional (underscore format for cycle >= 620)
- Survey designations (P-L, T-1, T-2, T-3)
- Numbered comets (including fragments like 73P-A, 73P-AA)
- Provisional comets (including fragments)
- Ancient comets (year < 1000)
- Natural satellites

## Helper Functions

### Format Conversion

```nim
# Convert minimal packed format to 12-character MPC report format
toReportFormat("0073Pa")   # "0073P      a"
toReportFormat("00001")    # "       00001"

# Convert 12-character MPC report format to minimal packed format
fromReportFormat("0073P      a")  # "0073Pa"
fromReportFormat("       00001")  # "00001"
```

### Fragment Handling

```nim
# Check if designation has a comet fragment
hasFragment("73P-A")   # true
hasFragment("73P")     # false

# Extract fragment suffix (returns uppercase)
getFragment("73P-A")   # "A"
getFragment("73P-AA")  # "AA"

# Get parent comet without fragment
getParent("73P-A")     # "73P"
getParent("0073Pa")    # "0073P"
```

### Designation Comparison

```nim
# Compare designations (normalizes to packed format)
designationsEqual("1995 XA", "J95X00A")  # true (same object)
designationsEqual("73P-A", "0073Pa")     # true (same object)
designationsEqual("73P-A", "73P-B")      # false (different fragments)
```

## Limitations

- BCE years not supported (12 entries in test data)
