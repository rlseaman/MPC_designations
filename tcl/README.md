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

#### `MPCDesignation::toReportFormat minimal`

Convert minimal packed format to 12-character MPC report format.

- **Parameters:** `minimal` - Minimal packed designation (e.g., "0073Pa")
- **Returns:** 12-character report format (e.g., "0073P      a")

#### `MPCDesignation::fromReportFormat report`

Convert 12-character MPC report format to minimal packed format.

- **Parameters:** `report` - 12-character report format
- **Returns:** Minimal packed designation

#### `MPCDesignation::hasFragment desig`

Check if a designation has a comet fragment suffix.

- **Parameters:** `desig` - Designation to check (packed or unpacked)
- **Returns:** 1 if has fragment, 0 if not

#### `MPCDesignation::getFragment desig`

Extract the fragment suffix from a comet designation.

- **Parameters:** `desig` - Designation (packed or unpacked)
- **Returns:** Fragment in uppercase (e.g., "A", "AA"), or empty string

#### `MPCDesignation::getParent desig`

Get the parent comet designation without fragment suffix.

- **Parameters:** `desig` - Designation (packed or unpacked)
- **Returns:** Parent designation in same format as input

#### `MPCDesignation::designationsEqual desig1 desig2`

Check if two designations refer to the same object.

- **Parameters:** `desig1`, `desig2` - Designations to compare
- **Returns:** 1 if same object, 0 if different

## Helper Functions

### Format Conversion (Minimal ↔ 12-Character Report Format)

Convert between minimal packed format and the 12-character MPC observation report format:

```tcl
# Minimal to 12-char report format
set report [MPCDesignation::toReportFormat "0073Pa"]  ;# "0073P      a"

# 12-char report format to minimal
set minimal [MPCDesignation::fromReportFormat "0073P      a"]  ;# "0073Pa"
```

### Fragment Extraction

```tcl
# Check if designation has a fragment
if {[MPCDesignation::hasFragment "73P-A"]} {
    # Extract fragment (uppercase)
    set frag [MPCDesignation::getFragment "73P-A"]  ;# "A"

    # Get parent comet
    set parent [MPCDesignation::getParent "73P-A"]  ;# "73P"
}
```

### Designation Comparison

Compare designations across different formats:

```tcl
# Same object, different formats
MPCDesignation::designationsEqual "1995 XA" "J95X00A"  ;# Returns 1
MPCDesignation::designationsEqual "73P-A" "0073Pa"     ;# Returns 1

# Different objects
MPCDesignation::designationsEqual "73P-A" "73P-B"      ;# Returns 0
```

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
# Run error handling tests (94 test cases)
tclsh test/test_errors.tcl ../test-data/error_test_cases.csv

# Run fragment handling tests
tclsh test/test_fragments.tcl

# Run helper function tests (77 test cases)
tclsh test/test_helpers.tcl

# Run conversion tests (requires decompressing test data first)
gunzip -k ../test-data/prov_unpack_to_pack.csv.gz
tclsh test/test_csv.tcl ../test-data/prov_unpack_to_pack.csv

# Run roundtrip tests (verifies pack(unpack(x)) == x and unpack(pack(x)) == x)
tclsh test/test_roundtrip.tcl ../test-data/prov_unpack_to_pack.csv
```

## Examples

See `examples/example_usage.tcl` for more detailed usage examples.

## Requirements

- TCL 8.5+
- No external packages required
