# MPC Designation Converter - Kotlin Implementation

Convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

## Requirements

- Kotlin 1.9+ (with kotlinc and kotlin commands)
- Java 11+ (for running compiled JARs)

## Building

```bash
make build
```

This creates `build/mpc_designation.jar` with the library and CLI.

## Usage

### Command Line

```bash
# Using the kotlin command
make run ARGS='1995 XA'
# Output: J95X00A

# With verbose output
make run ARGS='-v J95X00A'

# Or directly:
kotlin -cp build/mpc_designation.jar mpc.MainKt '1995 XA'

# Or with java:
java -jar build/mpc_designation.jar '1995 XA'
```

### Library Usage

```kotlin
import mpc.MPCDesignation

fun main() {
    // Simple conversion
    val packed = MPCDesignation.convertSimple("1995 XA")  // Returns "J95X00A"
    val unpacked = MPCDesignation.convertSimple("J95X00A")  // Returns "1995 XA"

    // Full result with format info
    val result = MPCDesignation.convert("1995 XA")
    println("Input: ${result.input}")
    println("Output: ${result.output}")
    println("Format: ${result.info.format}")
    println("Type: ${result.info.type}")

    // Ensure specific format
    val ensurePacked = MPCDesignation.pack("1995 XA")      // Returns "J95X00A"
    val ensureUnpacked = MPCDesignation.unpack("J95X00A")  // Returns "1995 XA"

    // Validation
    val isValid = MPCDesignation.isValidDesignation("1995 XA")  // Returns true

    // Error handling
    try {
        MPCDesignation.convert("invalid")
    } catch (e: MPCDesignation.MPCDesignationException) {
        println("Error: ${e.message}")
    }
}
```

## API Reference

### MPCDesignation Object

Main converter object with all conversion functions.

#### Functions

| Function | Description |
|----------|-------------|
| `convert(designation: String): Result` | Convert with full result info |
| `convertSimple(designation: String): String` | Convert and return just the output |
| `pack(designation: String): String` | Ensure packed format |
| `unpack(designation: String): String` | Ensure unpacked format |
| `detectFormat(designation: String): Info` | Detect format without converting |
| `isValidDesignation(designation: String?): Boolean` | Check if valid |

#### Helper Functions

| Function | Description |
|----------|-------------|
| `toReportFormat(s: String): String` | Convert minimal packed to 12-char MPC report format |
| `fromReportFormat(s: String): String` | Convert 12-char report format to minimal packed |
| `hasFragment(s: String): Boolean` | Check if designation has a comet fragment suffix |
| `getFragment(s: String): String` | Extract fragment suffix (uppercase, e.g., "A", "AA") |
| `getParent(s: String): String` | Get parent comet without fragment |
| `designationsEqual(d1: String, d2: String): Boolean` | Check if same object (compares packed forms) |

```kotlin
// Helper function examples
val report = MPCDesignation.toReportFormat("0073Pa")  // "0073P      a"
val minimal = MPCDesignation.fromReportFormat("0073P      a")  // "0073Pa"

val hasFrag = MPCDesignation.hasFragment("73P-A")  // true
val frag = MPCDesignation.getFragment("73P-A")  // "A"
val parent = MPCDesignation.getParent("73P-A")  // "73P"

val same = MPCDesignation.designationsEqual("73P-A", "0073Pa")  // true
```

#### Data Classes

```kotlin
data class Info(
    val format: FormatType?,  // PACKED or UNPACKED
    val type: String,         // "permanent", "provisional", etc.
    val subtype: String       // Detailed description
)

data class Result(
    val input: String,
    val output: String,
    val info: Info
)
```

#### Constants

```kotlin
MPCDesignation.VERSION          // "1.0.0"
MPCDesignation.MAX_ASTEROID_NUMBER  // 15396335
```

## Testing

```bash
# Run all tests
make test-all

# Run specific test suites
make test-errors    # Error handling tests (94 cases)
make test-helpers   # Helper function tests (77 cases)
make test-csv       # Conversion tests (2M+ cases)
make test-roundtrip # Round-trip tests
```

## Examples

```kotlin
// Asteroids - permanent (numbered)
MPCDesignation.convertSimple("1")           // "00001"
MPCDesignation.convertSimple("100001")      // "A0001"
MPCDesignation.convertSimple("620000")      // "~0000"

// Asteroids - provisional
MPCDesignation.convertSimple("1995 XA")     // "J95X00A"
MPCDesignation.convertSimple("2024 AB631")  // "_OA004S"

// Asteroids - survey
MPCDesignation.convertSimple("2040 P-L")    // "PLS2040"

// Comets
MPCDesignation.convertSimple("1P")          // "0001P"
MPCDesignation.convertSimple("C/1995 O1")   // "CJ95O010"

// Natural satellites
MPCDesignation.convertSimple("S/2019 S 22") // "SK19S220"
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
| Numbered (fragment) | 73P-A | 0073Pa |
| Numbered (2-letter fragment) | 73P-AA | 0073Paa |
| Provisional | C/1995 O1 | CJ95O010 |
| Provisional (fragment) | D/1993 F2-B | DJ93F02b |

### Natural Satellites

| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |

## License

CC0 1.0 Universal - Public Domain Dedication
