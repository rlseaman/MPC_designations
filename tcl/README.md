# MPC Designation Converter - TCL Implementation

TCL library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```tcl
source src/mpc_designation.tcl

set packed [MPCDesignation::convertSimple "1995 XA"]  ;# Returns "J95X00A"
set unpacked [MPCDesignation::convertSimple "J95X00A"]  ;# Returns "1995 XA"
```

## Installation

Simply source the file in your TCL script:

```tcl
source /path/to/mpc_designation.tcl
```

## CLI Usage

```bash
# Single conversion
tclsh src/mpc_designation.tcl '1995 XA'
# Output: J95X00A

# Multiple designations
tclsh src/mpc_designation.tcl 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
tclsh src/mpc_designation.tcl -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## Library Usage

### Simple Conversion

```tcl
source mpc_designation.tcl

# Convert designation
if {[catch {set result [MPCDesignation::convertSimple "1995 XA"]} err]} {
    puts "Error: $err"
} else {
    puts "Result: $result"  ;# J95X00A
}
```

### With Format Information

```tcl
source mpc_designation.tcl

set info [MPCDesignation::convert "1995 XA"]
puts "Input:  [dict get $info input]"
puts "Output: [dict get $info output]"
puts "Format: [dict get $info format]"
puts "Type:   [dict get $info subtype]"
```

### Batch Processing

```tcl
source mpc_designation.tcl

set designations {1 "1995 XA" "C/1995 O1"}
foreach des $designations {
    if {[catch {set result [MPCDesignation::convertSimple $des]} err]} {
        puts "$des: Error - $err"
    } else {
        puts "$des -> $result"
    }
}
```

## API Reference

### Namespace

All functions are in the `MPCDesignation` namespace.

### Procedures

#### `MPCDesignation::convertSimple designation`

Convert a designation, returning just the result string.

- **Parameters:** `designation` - The MPC designation to convert
- **Returns:** The converted designation
- **Throws:** Error if the input is invalid

#### `MPCDesignation::convert designation`

Convert a designation, returning detailed information.

- **Parameters:** `designation` - The MPC designation to convert
- **Returns:** Dictionary with keys:
  - `input`: Original input
  - `output`: Converted designation
  - `format`: "packed" or "unpacked"
  - `subtype`: Human-readable description

#### `MPCDesignation::detectFormat designation`

Detect the format of a designation without converting.

- **Parameters:** `designation` - The MPC designation to analyze
- **Returns:** Dictionary with format information

## Comet Fragment Handling

The library supports comet fragment designations:

### Numbered Comets with Fragments

Numbered comets (like 73P) can have fragments:

```tcl
MPCDesignation::convertSimple "73P-A"    ;# Returns "0073Pa"
MPCDesignation::convertSimple "73P-AA"   ;# Returns "0073Paa"
MPCDesignation::convertSimple "0073Pa"   ;# Returns "73P-A"
MPCDesignation::convertSimple "0073Paa"  ;# Returns "73P-AA"
```

### Provisional Comets with Fragments

Provisional comets can also have fragments:

```tcl
MPCDesignation::convertSimple "P/1930 J1-A"   ;# Returns "PJ30J01a"
MPCDesignation::convertSimple "P/1930 J1-AA"  ;# Returns "PJ30J01aa"
MPCDesignation::convertSimple "PJ30J01aa"     ;# Returns "P/1930 J1-AA"
```

Fragment letters include all A-Z (including I, per MPC data).

## Error Handling

The library uses TCL's standard error mechanism:

```tcl
if {[catch {set result [MPCDesignation::convertSimple $input]} err]} {
    puts "Invalid designation: $err"
}
```

## Testing

```bash
# Run error handling tests
tclsh test/test_errors.tcl ../test-data/error_test_cases.csv

# Run fragment handling tests
tclsh test/test_fragments.tcl

# Run conversion tests (requires decompressing test data)
gunzip -k ../test-data/prov_unpack_to_pack.csv.gz
tclsh test/test_csv.tcl ../test-data/prov_unpack_to_pack.csv
```

## Examples

See `examples/example_usage.tcl` for more detailed usage examples.

## Requirements

- TCL 8.5+
- No external packages required
