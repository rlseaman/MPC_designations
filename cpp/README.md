# MPC Designation Converter - C++

C++ implementation of the Minor Planet Center (MPC) designation converter.

## Requirements

- C++17 compatible compiler (GCC 7+, Clang 5+, MSVC 2017+)
- Make

## Quick Start

### Build

```bash
make
```

### CLI Usage

```bash
# Convert designations
./mpc_designation '1995 XA'
# Output: J95X00A

./mpc_designation J95X00A
# Output: 1995 XA

# Multiple designations
./mpc_designation 1 J95X00A '1995 XA'
# Output:
# 1 -> 00001
# J95X00A -> 1995 XA
# 1995 XA -> J95X00A

# Verbose mode
./mpc_designation -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

### Library Usage

```cpp
#include "mpc_designation.hpp"
#include <iostream>

int main() {
    // Simple conversion (auto-detects format)
    std::string packed = mpc::MPCDesignation::convertSimple("1995 XA");
    // Returns "J95X00A"

    std::string unpacked = mpc::MPCDesignation::convertSimple("J95X00A");
    // Returns "1995 XA"

    // Explicit pack/unpack
    packed = mpc::MPCDesignation::pack("1995 XA");    // Returns "J95X00A"
    unpacked = mpc::MPCDesignation::unpack("J95X00A"); // Returns "1995 XA"

    // Full conversion with metadata
    auto result = mpc::MPCDesignation::convert("1995 XA");
    std::cout << "Input: " << result.input << "\n";
    std::cout << "Output: " << result.output << "\n";
    std::cout << "Format: " << (result.info.format == mpc::Format::Packed ? "packed" : "unpacked") << "\n";
    std::cout << "Subtype: " << result.info.subtype << "\n";

    // Format detection
    auto info = mpc::MPCDesignation::detectFormat("1995 XA");
    // info.format == Format::Unpacked
    // info.type == Type::Provisional

    // Validation
    if (mpc::MPCDesignation::isValid("1995 XA")) {
        std::cout << "Valid designation\n";
    }

    // Error handling
    try {
        mpc::MPCDesignation::convert("invalid");
    } catch (const mpc::MPCDesignationError& e) {
        std::cerr << "Error: " << e.what() << "\n";
    }

    return 0;
}
```

## API Reference

### Namespace `mpc`

#### Constants

- `VERSION` - Library version string
- `MAX_ASTEROID_NUMBER` - Maximum supported asteroid number (15396335)

#### Enums

```cpp
enum class Format { Unknown, Packed, Unpacked };

enum class Type {
    Unknown, Permanent, Provisional, ProvisionalExtended, Survey,
    CometNumbered, CometProvisional, CometFull, CometAncient, CometBCE,
    Satellite
};
```

#### Structures

```cpp
struct FormatInfo {
    Format format;
    Type type;
    std::string subtype;
};

struct ConversionResult {
    std::string input;
    std::string output;
    FormatInfo info;
};
```

#### Class `MPCDesignation`

Static methods:

- `convert(designation) -> ConversionResult` - Full conversion with metadata
- `convertSimple(designation) -> std::string` - Simple conversion, output only
- `pack(designation) -> std::string` - Convert to packed (no-op if already packed)
- `unpack(designation) -> std::string` - Convert to unpacked (no-op if already unpacked)
- `detectFormat(designation) -> FormatInfo` - Detect format without converting
- `isValid(designation) -> bool` - Check if valid (noexcept)
- `packPermanent(number) -> std::string` - Pack asteroid number
- `unpackPermanent(packed) -> long` - Unpack to asteroid number

Helper functions:

- `toReportFormat(minimal) -> std::string` - Convert to 12-char MPC report format
- `fromReportFormat(report) -> std::string` - Convert from 12-char report format
- `hasFragment(designation) -> bool` - Check if has comet fragment suffix
- `getFragment(designation) -> std::string` - Extract fragment (uppercase, e.g., "A", "AA")
- `getParent(designation) -> std::string` - Get parent comet without fragment
- `designationsEqual(d1, d2) -> bool` - Check if same object (compares packed forms)

```cpp
// Helper function examples
std::string report = mpc::MPCDesignation::toReportFormat("0073Pa");  // "0073P      a"
std::string minimal = mpc::MPCDesignation::fromReportFormat("0073P      a");  // "0073Pa"

bool hasFrag = mpc::MPCDesignation::hasFragment("73P-A");  // true
std::string frag = mpc::MPCDesignation::getFragment("73P-A");  // "A"
std::string parent = mpc::MPCDesignation::getParent("73P-A");  // "73P"

bool same = mpc::MPCDesignation::designationsEqual("73P-A", "0073Pa");  // true
```

#### Exception Class `MPCDesignationError`

Thrown for invalid designations. Inherits from `std::runtime_error`.

## Supported Formats

### Asteroids

| Type | Unpacked | Packed |
|------|----------|--------|
| Permanent (< 100K) | 1 - 99999 | 00001 - 99999 |
| Permanent (100K-620K) | 100001 - 619999 | A0001 - z9999 |
| Permanent (620K+) | 620000+ | ~0000+ |
| Provisional | 1995 XA | J95X00A |
| Extended provisional | 2024 AB631 | _OA004S |
| Survey | 2040 P-L | PLS2040 |

### Comets

| Type | Unpacked | Packed |
|------|----------|--------|
| Numbered | 1P | 0001P |
| Provisional | C/1995 O1 | CJ95O010 |
| With fragment | D/1993 F2-B | DJ93F02b |
| Ancient | C/240 V1 | C240V010 |
| BCE | C/-146 P1 | C.53P010 |

### Natural Satellites

| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |

## Testing

```bash
# Run all tests
make test-all

# Run specific tests
make test-errors    # Error handling tests (94 cases)
make test-helpers   # Helper function tests (77 cases)
make test-csv       # Conversion tests (2M+ entries)
make test-roundtrip # Roundtrip verification

# Help
make help
```

## Files

```
cpp/
├── README.md               # This file
├── Makefile                # Build and test commands
├── src/
│   ├── mpc_designation.hpp # Header file
│   ├── mpc_designation.cpp # Implementation
│   └── mpc_designation_cli.cpp # CLI tool
└── test/
    ├── test_csv.cpp        # CSV conversion tests
    ├── test_errors.cpp     # Error handling tests
    ├── test_helpers.cpp    # Helper function tests
    └── test_roundtrip.cpp  # Roundtrip tests
```

## License

CC0 1.0 Universal - Public Domain Dedication
