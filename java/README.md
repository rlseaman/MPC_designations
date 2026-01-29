# MPC Designation Converter - Java

Java implementation of the MPC designation converter.

## Requirements

- Java 8 or later
- Make (optional, for build automation)

## Building

```bash
make build
```

Or manually:
```bash
mkdir -p classes
javac -d classes -sourcepath src src/mpc/*.java
```

## Usage

### Command Line

```bash
# Build first
make build

# Convert designations
java -cp classes mpc.MPCDesignationCLI '1995 XA'      # Output: J95X00A
java -cp classes mpc.MPCDesignationCLI J95X00A        # Output: 1995 XA
java -cp classes mpc.MPCDesignationCLI 1              # Output: 00001
java -cp classes mpc.MPCDesignationCLI 'C/1995 O1'    # Output: CJ95O010

# Verbose output
java -cp classes mpc.MPCDesignationCLI -v '1995 XA'

# Multiple designations
java -cp classes mpc.MPCDesignationCLI 1 2 3 'C/1995 O1'
```

### Library API

```java
import mpc.MPCDesignation;

// Simple conversion (auto-detects format)
String packed = MPCDesignation.convertSimple("1995 XA");  // Returns "J95X00A"
String unpacked = MPCDesignation.convertSimple("J95X00A"); // Returns "1995 XA"

// Full result with metadata
MPCDesignation.Result result = MPCDesignation.convert("1995 XA");
System.out.println(result.output);        // "J95X00A"
System.out.println(result.info.format);   // "packed" or "unpacked"
System.out.println(result.info.type);     // "provisional", "permanent", etc.
System.out.println(result.info.subtype);  // Detailed description

// Force specific direction
String packed = MPCDesignation.pack("1995 XA");    // Always returns packed
String unpacked = MPCDesignation.unpack("J95X00A"); // Always returns unpacked

// Validation
boolean valid = MPCDesignation.isValidDesignation("1995 XA"); // true
boolean invalid = MPCDesignation.isValidDesignation("invalid"); // false

// Low-level functions
int number = MPCDesignation.unpackPermanent("A0001"); // Returns 100001
String packed = MPCDesignation.packPermanent(100001);  // Returns "A0001"
```

## Testing

```bash
# Run all tests
make test-all

# Run specific tests
make test-errors    # Error handling tests
make test-csv       # Conversion tests (2M+ cases)
make test-roundtrip # Round-trip tests
```

## Project Structure

```
java/
├── README.md
├── Makefile
├── src/
│   └── mpc/
│       ├── MPCDesignation.java     # Core library
│       └── MPCDesignationCLI.java  # CLI tool
├── test/
│   ├── TestCSV.java        # Conversion tests
│   ├── TestErrors.java     # Error handling tests
│   └── TestRoundtrip.java  # Round-trip tests
└── classes/                # Compiled classes (generated)
```

## API Reference

### High-Level Functions

| Function | Description |
|----------|-------------|
| `convert(String)` | Convert and return full result |
| `convertSimple(String)` | Convert and return just the output |
| `pack(String)` | Ensure result is packed |
| `unpack(String)` | Ensure result is unpacked |
| `isValidDesignation(String)` | Check if valid (no exception) |

### Low-Level Functions

| Function | Description |
|----------|-------------|
| `packPermanent(int)` | Pack numbered asteroid |
| `unpackPermanent(String)` | Unpack numbered asteroid |
| `packProvisional(String)` | Pack provisional asteroid |
| `unpackProvisional(String)` | Unpack provisional asteroid |
| `packCometFull(String)` | Pack full comet designation |
| `unpackCometFull(String)` | Unpack full comet designation |
| `packSatellite(String)` | Pack satellite designation |
| `unpackSatellite(String)` | Unpack satellite designation |

### Exception Handling

```java
try {
    String result = MPCDesignation.convertSimple("invalid");
} catch (MPCDesignation.MPCDesignationException e) {
    System.err.println("Error: " + e.getMessage());
}
```

## Examples

### Asteroids

```java
// Numbered asteroids
MPCDesignation.convertSimple("1");        // "00001"
MPCDesignation.convertSimple("100001");   // "A0001"
MPCDesignation.convertSimple("620000");   // "~0000"

// Provisional asteroids
MPCDesignation.convertSimple("1995 XA");      // "J95X00A"
MPCDesignation.convertSimple("2024 AB631");   // "_4AMu1B"

// Survey designations
MPCDesignation.convertSimple("2040 P-L");     // "PLS2040"
```

### Comets

```java
// Numbered comets
MPCDesignation.convertSimple("1P");           // "0001P"
MPCDesignation.convertSimple("0001P");        // "1P"

// Provisional comets
MPCDesignation.convertSimple("C/1995 O1");    // "CJ95O010"
MPCDesignation.convertSimple("D/1993 F2-B");  // "DJ93F02b"
```

### Satellites

```java
MPCDesignation.convertSimple("S/2019 S 22");  // "SK19S220"
MPCDesignation.convertSimple("SK19S220");     // "S/2019 S 22"
```
