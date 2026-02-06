# MPC Designation Converter - PHP

PHP implementation of the Minor Planet Center (MPC) designation converter.

## Requirements

- PHP 8.0 or later (uses `str_starts_with`, match expressions, named arguments)
- BCMath extension (for handling large number comparisons)

## Quick Start

### Library Usage

```php
<?php
require_once 'src/MPCDesignation.php';

use MPC\MPCDesignation;
use MPC\MPCDesignationException;

// Simple conversion (auto-detects format)
$packed = MPCDesignation::convertSimple('1995 XA');  // Returns 'J95X00A'
$unpacked = MPCDesignation::convertSimple('J95X00A');  // Returns '1995 XA'

// Explicit pack/unpack
$packed = MPCDesignation::pack('1995 XA');    // Returns 'J95X00A'
$unpacked = MPCDesignation::unpack('J95X00A');  // Returns '1995 XA'

// Full conversion with metadata
$result = MPCDesignation::convert('1995 XA');
echo $result->input;   // '1995 XA'
echo $result->output;  // 'J95X00A'
echo $result->info->format;   // 'unpacked'
echo $result->info->type;     // 'provisional'
echo $result->info->subtype;  // 'provisional'

// Format detection
$info = MPCDesignation::detectFormat('1995 XA');
echo $info->format;  // 'unpacked'
echo $info->type;    // 'provisional'

// Validation
if (MPCDesignation::isValidDesignation('1995 XA')) {
    echo "Valid designation";
}

// Error handling
try {
    MPCDesignation::convert('invalid');
} catch (MPCDesignationException $e) {
    echo "Error: " . $e->getMessage();
}
```

### CLI Usage

```bash
# Convert designations
php src/mpc_designation_cli.php '1995 XA'
# Output: J95X00A

php src/mpc_designation_cli.php J95X00A
# Output: 1995 XA

# Multiple designations
php src/mpc_designation_cli.php 1 J95X00A '1995 XA'
# Output:
# 1 -> 00001
# J95X00A -> 1995 XA
# 1995 XA -> J95X00A

# Verbose mode
php src/mpc_designation_cli.php -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## API Reference

### Classes

#### `MPCDesignation`

Main converter class with static methods.

**Constants:**
- `VERSION` - Library version
- `MAX_ASTEROID_NUMBER` - Maximum supported asteroid number (15396335)

**Static Methods:**
- `convert(string $designation): ConversionResult` - Full conversion with metadata
- `convertSimple(string $designation): string` - Simple conversion, returns output only
- `pack(string $designation): string` - Convert to packed format (no-op if already packed)
- `unpack(string $designation): string` - Convert to unpacked format (no-op if already unpacked)
- `detectFormat(string $designation): FormatInfo` - Detect format without converting
- `isValidDesignation(string $designation): bool` - Check if valid designation

**Helper Methods:**
- `toReportFormat(string $minimal): string` - Convert minimal packed to 12-char report format
- `fromReportFormat(string $report): string` - Convert 12-char report format to minimal packed
- `hasFragment(string $desig): bool` - Check if designation has comet fragment
- `getFragment(string $desig): string` - Extract fragment suffix (uppercase)
- `getParent(string $desig): string` - Get parent designation without fragment
- `designationsEqual(string $d1, string $d2): bool` - Compare designations across formats

#### `ConversionResult`

Result of `convert()`:
- `input: string` - Original input
- `output: string` - Converted output
- `info: FormatInfo` - Format information

#### `FormatInfo`

Format detection result:
- `format: string` - 'packed' or 'unpacked'
- `type: string` - Designation type (e.g., 'permanent', 'provisional', 'comet_full')
- `subtype: string` - Human-readable description

#### `MPCDesignationException`

Exception thrown for invalid designations.

## Helper Function Examples

### Format Conversion (Minimal ↔ 12-Character Report Format)

```php
use MPC\MPCDesignation;

// Minimal to 12-char report format
MPCDesignation::toReportFormat('0073Pa');   // '0073P      a'
MPCDesignation::toReportFormat('00001');    // '       00001'

// 12-char report format to minimal
MPCDesignation::fromReportFormat('0073P      a');  // '0073Pa'
MPCDesignation::fromReportFormat('       00001');  // '00001'
```

### Fragment Extraction

```php
use MPC\MPCDesignation;

// Check if designation has a fragment
MPCDesignation::hasFragment('73P-A');   // true
MPCDesignation::hasFragment('73P');     // false
MPCDesignation::hasFragment('0073Pa');  // true (packed)

// Extract fragment (uppercase)
MPCDesignation::getFragment('73P-A');    // 'A'
MPCDesignation::getFragment('73P-AA');   // 'AA'
MPCDesignation::getFragment('0073Paa');  // 'AA'

// Get parent comet
MPCDesignation::getParent('73P-A');   // '73P'
MPCDesignation::getParent('0073Pa');  // '0073P'
```

### Designation Comparison

```php
use MPC\MPCDesignation;

// Same object, different formats
MPCDesignation::designationsEqual('1995 XA', 'J95X00A');  // true
MPCDesignation::designationsEqual('73P-A', '0073Pa');     // true

// Different objects
MPCDesignation::designationsEqual('73P-A', '73P-B');      // false
```

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
php/
├── README.md           # This file
├── Makefile            # Build and test commands
├── src/
│   ├── MPCDesignation.php      # Main library
│   └── mpc_designation_cli.php # CLI tool
└── test/
    ├── test_csv.php       # CSV conversion tests
    ├── test_errors.php    # Error handling tests
    ├── test_helpers.php   # Helper function tests
    └── test_roundtrip.php # Roundtrip tests
```

## License

CC0 1.0 Universal - Public Domain Dedication
