# MPC Designation Converter - Swift Implementation

A Swift implementation of the Minor Planet Center designation converter.

## Requirements

- Swift 5.0+ (included with Xcode on macOS)
- macOS, Linux, or Windows with Swift toolchain

## Building

```bash
# Build the CLI tool
make

# Or compile directly
swiftc -O -o mpc_designation src/MPCDesignation.swift
```

## Usage

### Command Line

```bash
# Convert a designation
./mpc_designation "1995 XA"      # Output: J95X00A
./mpc_designation J95X00A        # Output: 1995 XA

# Verbose output
./mpc_designation -v "C/1995 O1"

# Multiple designations
./mpc_designation 1 2 3 "1995 XA"
```

### As a Library

```swift
import Foundation

// Include MPCDesignation.swift in your project

// Convert between formats
let packed = try pack("1995 XA")        // "J95X00A"
let unpacked = try unpack("J95X00A")    // "1995 XA"

// Auto-detect and convert
let result = try convert("1995 XA")
print(result.output)  // "J95X00A"
print(result.info.format)  // "unpacked"
print(result.info.type)    // "provisional"

// Simple conversion
let converted = try convertSimple("00001")  // "1"

// Detect format without converting
let info = try detectFormat("C/1995 O1")
print(info.type)     // "comet_full"
print(info.subtype)  // "comet provisional (non-periodic)"
```

## Testing

```bash
# Run all tests
make test-all

# Run specific tests
make test-errors    # Error handling tests (94 cases)
make test-helpers   # Helper function tests (77 cases)
make test-csv       # Conversion tests (2M+ entries)
make test-roundtrip # Roundtrip verification
```

## Supported Designations

- **Numbered asteroids**: `1` ↔ `00001`, `100000` ↔ `A0000`
- **Provisional asteroids**: `1995 XA` ↔ `J95X00A`
- **Survey designations**: `2040 P-L` ↔ `PLS2040`
- **Numbered comets**: `1P` ↔ `0001P`
- **Comet provisionals**: `C/1995 O1` ↔ `CJ95O010`
- **Natural satellites**: `S/2019 S 22` ↔ `SK19S220`

## Platform Notes

### macOS
Swift is included with Xcode. The implementation uses only Foundation framework.

### Linux
Install Swift from [swift.org](https://swift.org/download/). The code is compatible
with Swift on Linux.

### Windows
Swift for Windows is available from [swift.org](https://swift.org/download/).
The implementation should work with the Windows Swift toolchain.

## API Reference

### High-Level Functions

- `pack(_ designation: String) throws -> String` - Ensure packed format
- `unpack(_ designation: String) throws -> String` - Ensure unpacked format
- `convert(_ designation: String) throws -> ConversionResult` - Convert with metadata
- `convertSimple(_ designation: String) throws -> String` - Simple conversion

### Detection

- `detectFormat(_ designation: String) throws -> FormatInfo` - Detect format and type

### Helper Functions

- `toReportFormat(_ minimal: String) throws -> String` - Convert minimal packed to 12-char MPC report format
- `fromReportFormat(_ report: String) throws -> String` - Convert 12-char report format to minimal packed
- `hasFragment(_ designation: String) -> Bool` - Check if designation has a comet fragment suffix
- `getFragment(_ designation: String) -> String` - Extract fragment suffix (uppercase, e.g., "A", "AA")
- `getParent(_ designation: String) -> String` - Get parent comet without fragment
- `designationsEqual(_ d1: String, _ d2: String) -> Bool` - Check if same object (compares packed forms)

```swift
// Helper function examples
let report = try toReportFormat("0073Pa")  // "0073P      a"
let minimal = try fromReportFormat("0073P      a")  // "0073Pa"

let hasFrag = hasFragment("73P-A")  // true
let frag = getFragment("73P-A")  // "A"
let parent = getParent("73P-A")  // "73P"

let same = designationsEqual("73P-A", "0073Pa")  // true
```

### Error Handling

All functions throw `MPCDesignationError` on invalid input:
- `.invalidCharacter(Character)` - Non-ASCII or control character
- `.consecutiveSpaces` - Multiple spaces in designation
- `.emptyDesignation` - Empty string
- `.invalidFormat(String)` - Unrecognized format
- `.outOfRange(String)` - Value out of valid range
