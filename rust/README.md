# MPC Designation Converter - Rust Implementation

Rust library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```rust
use mpc_designation::convert_simple;

let result = convert_simple("1995 XA").unwrap();  // Returns "J95X00A"
let back = convert_simple("J95X00A").unwrap();    // Returns "1995 XA"
```

## Installation

### From Source

```bash
cd rust
cargo build --release
```

The binaries will be in `target/release/`.

### As a Dependency

Add to your `Cargo.toml`:

```toml
[dependencies]
mpc_designation = { path = "path/to/rust" }
```

## CLI Usage

```bash
# Single conversion
./target/release/mpc_designation '1995 XA'
# Output: J95X00A

# Multiple designations
./target/release/mpc_designation 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
./target/release/mpc_designation -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## Library Usage

### Simple Conversion

```rust
use mpc_designation::{convert_simple, MPCDesignationError};

fn main() -> Result<(), MPCDesignationError> {
    let packed = convert_simple("1995 XA")?;    // Returns "J95X00A"
    let unpacked = convert_simple("J95X00A")?;  // Returns "1995 XA"
    Ok(())
}
```

### With Format Information

```rust
use mpc_designation::convert;

fn main() {
    let result = convert("1995 XA").unwrap();
    println!("Input: {}", result.input);           // "1995 XA"
    println!("Output: {}", result.output);         // "J95X00A"
    println!("Format: {}", result.info.format);    // "unpacked"
    println!("Type: {}", result.info.subtype);     // "provisional"
}
```

### Format Detection

```rust
use mpc_designation::detect_format;

fn main() {
    let info = detect_format("1995 XA").unwrap();
    println!("Format: {}", info.format);  // "unpacked"
    println!("Type: {}", info.designation_type);    // "provisional"
}
```

### Validation

```rust
use mpc_designation::is_valid_designation;

fn main() {
    println!("{}", is_valid_designation("1995 XA"));  // true
    println!("{}", is_valid_designation("invalid"));  // false
}
```

### Pack/Unpack

```rust
use mpc_designation::{pack, unpack};

fn main() {
    // Ensure packed format
    let p = pack("1995 XA").unwrap();    // "J95X00A"
    let p2 = pack("J95X00A").unwrap();   // "J95X00A" (already packed)

    // Ensure unpacked format
    let u = unpack("J95X00A").unwrap();  // "1995 XA"
    let u2 = unpack("1995 XA").unwrap(); // "1995 XA" (already unpacked)
}
```

## API Reference

### Types

#### `MPCDesignationError`

Error type for invalid designations.

#### `FormatInfo`

Information about a designation's format:
- `format`: "packed" or "unpacked"
- `designation_type`: e.g., "permanent", "provisional", "comet_full"
- `subtype`: Human-readable description

#### `ConvertResult`

Result of a conversion:
- `input`: Original input string
- `output`: Converted designation
- `info`: FormatInfo

### Functions

#### `convert_simple(designation: &str) -> Result<String>`

Convert a designation, returning just the result string.

#### `convert(designation: &str) -> Result<ConvertResult>`

Convert a designation with detailed information.

#### `detect_format(designation: &str) -> Result<FormatInfo>`

Detect the format without converting.

#### `pack(designation: &str) -> Result<String>`

Ensure a designation is in packed format.

#### `unpack(designation: &str) -> Result<String>`

Ensure a designation is in unpacked format.

#### `is_valid_designation(designation: &str) -> bool`

Check if a string is a valid MPC designation.

## Testing

```bash
# Build and run conversion tests
make test

# Run round-trip tests
make test-roundtrip

# Run unit tests
make test-unit

# Run all tests
make test-all
```

## Performance

Rust is expected to be one of the fastest implementations, comparable to C and Go.

## Requirements

- Rust 1.70.0 or later
- Dependencies: `regex`, `lazy_static` (handled by Cargo)
