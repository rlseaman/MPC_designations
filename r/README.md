# MPC Designation Converter - R

Convert between packed and unpacked Minor Planet Center (MPC) designations.

## Quick Start

```r
source("src/mpc_designation.R")

# Simple conversion (auto-detects format)
convert_simple("1995 XA")     # Returns "J95X00A"
convert_simple("J95X00A")     # Returns "1995 XA"

# Ensure packed format
pack("1995 XA")               # Returns "J95X00A"
pack("J95X00A")               # Returns "J95X00A" (already packed)

# Ensure unpacked format
unpack("J95X00A")             # Returns "1995 XA"
unpack("1995 XA")             # Returns "1995 XA" (already unpacked)
```

## Command-Line Usage

```bash
Rscript src/mpc_designation_cli.R '1995 XA'
# Output: J95X00A

Rscript src/mpc_designation_cli.R J95X00A
# Output: 1995 XA

Rscript src/mpc_designation_cli.R --verbose '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## API Reference

### High-Level Functions

| Function | Description |
|----------|-------------|
| `convert_simple(des)` | Auto-detect and convert, return string |
| `convert(des)` | Auto-detect and convert, return list with metadata |
| `pack(des)` | Ensure packed format |
| `unpack(des)` | Ensure unpacked format |
| `is_valid_designation(des)` | Check if valid (returns TRUE/FALSE) |
| `detect_format(des)` | Return format info without converting |

### Type-Specific Functions

| Function | Description |
|----------|-------------|
| `pack_permanent(number)` | Pack numbered asteroid (integer) |
| `unpack_permanent(packed)` | Unpack to asteroid number |
| `pack_provisional(unpacked)` | Pack provisional asteroid |
| `unpack_provisional(packed)` | Unpack provisional asteroid |
| `pack_comet_numbered(unpacked)` | Pack numbered comet |
| `unpack_comet_numbered(packed)` | Unpack numbered comet |
| `pack_comet_full(unpacked)` | Pack comet with type prefix |
| `unpack_comet_full(packed)` | Unpack comet with type prefix |
| `pack_satellite(unpacked)` | Pack satellite designation |
| `unpack_satellite(packed)` | Unpack satellite designation |

## Examples

```r
source("src/mpc_designation.R")

# Permanent (numbered) asteroids
pack_permanent(1)           # "00001"
pack_permanent(100001)      # "A0001"
pack_permanent(620000)      # "~0000"
unpack_permanent("A0001")   # 100001

# Provisional asteroids
pack_provisional("1995 XA")     # "J95X00A"
pack_provisional("2024 AB631")  # "_OA004S" (extended format)
pack_provisional("2040 P-L")    # "PLS2040" (survey)

# Comets
pack_comet_numbered("1P")           # "0001P"
pack_comet_full("C/1995 O1")        # "CJ95O010"
pack_comet_full("D/1993 F2-B")      # "DJ93F02b"

# Satellites
pack_satellite("S/2019 S 22")       # "SK19S220"
unpack_satellite("SK19S220")        # "S/2019 S 22"

# Format detection
info <- detect_format("1995 XA")
# info$format = "unpacked"
# info$type = "provisional"
# info$subtype = "provisional"

# Validation
is_valid_designation("1995 XA")     # TRUE
is_valid_designation("invalid")     # FALSE

# Error handling
tryCatch({
  convert_simple("invalid")
}, error = function(e) {
  message("Error: ", conditionMessage(e))
})
```

## Batch Processing

```r
source("src/mpc_designation.R")

# Process multiple designations
designations <- c("1995 XA", "2024 AB1", "C/1995 O1", "1P", "S/2019 S 22")

results <- sapply(designations, function(des) {
  tryCatch(convert_simple(des), error = function(e) NA)
})

print(results)
# 1995 XA       2024 AB1    C/1995 O1           1P  S/2019 S 22
# "J95X00A"    "K24A01B"   "CJ95O010"      "0001P"   "SK19S220"

# Filter valid designations from mixed input
mixed_input <- c("1995 XA", "temp_id_123", "J95X00A", "invalid")
valid <- mixed_input[sapply(mixed_input, is_valid_designation)]
print(valid)
# [1] "1995 XA" "J95X00A"
```

## Data Frame Integration

```r
source("src/mpc_designation.R")

# Create a data frame with designations
df <- data.frame(
  unpacked = c("1995 XA", "2024 AB1", "C/1995 O1"),
  stringsAsFactors = FALSE
)

# Add packed column
df$packed <- sapply(df$unpacked, pack)

print(df)
#    unpacked    packed
# 1   1995 XA   J95X00A
# 2  2024 AB1   K24A01B
# 3 C/1995 O1  CJ95O010
```

## Testing

```bash
# Quick validation tests
make test-quick

# Error handling tests
make test-errors

# Full CSV test suite (2M+ tests)
make test-csv

# All tests
make test
```

## Supported Formats

### Asteroids
- **Permanent (numbered)**: 1 - 15,396,335
- **Provisional**: 1995 XA, 2024 AB631
- **Survey**: 2040 P-L, 3138 T-1

### Comets
- **Numbered**: 1P, 354P
- **Provisional**: C/1995 O1, P/2019 A4
- **Fragments**: D/1993 F2-B
- **Ancient/BCE**: C/240 V1, C/-146 P1

### Natural Satellites
- **Provisional**: S/2019 S 22, S/2003 J 2

## Requirements

- R 3.6 or later (uses base R only, no external packages required)

## License

CC0 1.0 Universal - Public Domain Dedication
