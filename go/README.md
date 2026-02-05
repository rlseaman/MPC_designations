# MPC Designation Converter - Go Implementation

Go library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```go
import "github.com/rlseaman/mpc_designations/go/mpc"

result, _ := mpc.ConvertSimple("1995 XA")  // Returns "J95X00A"
result, _ = mpc.ConvertSimple("J95X00A")   // Returns "1995 XA"
```

## Installation

```bash
cd go
make
# Binary will be at ./mpc_designation
```

## CLI Usage

```bash
# Single conversion
./mpc_designation '1995 XA'
# Output: J95X00A

# Multiple designations
./mpc_designation 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
./mpc_designation -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## Library Usage

### Simple Conversion

```go
import "github.com/rlseaman/mpc_designations/go/mpc"

func main() {
    packed, err := mpc.ConvertSimple("1995 XA")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(packed)  // J95X00A
}
```

### With Format Information

```go
result, err := mpc.Convert("1995 XA")
if err != nil {
    log.Fatal(err)
}
fmt.Println("Input:", result.Input)
fmt.Println("Output:", result.Output)
fmt.Println("Format:", result.Info.Format)
fmt.Println("Type:", result.Info.Subtype)
```

### Pack/Unpack (Idempotent)

```go
// Pack always returns packed format
packed, _ := mpc.Pack("1995 XA")   // Returns "J95X00A"
packed, _ = mpc.Pack("J95X00A")    // Returns "J95X00A" (already packed)

// Unpack always returns unpacked format
unpacked, _ := mpc.Unpack("J95X00A")  // Returns "1995 XA"
unpacked, _ = mpc.Unpack("1995 XA")   // Returns "1995 XA" (already unpacked)
```

## API Reference

### Types

```go
type FormatType int

const (
    FormatPacked FormatType = iota
    FormatUnpacked
)

type Info struct {
    Format  FormatType
    Type    string
    Subtype string
}

type Result struct {
    Input  string
    Output string
    Info   Info
}
```

### Functions

```go
// Core conversion
func Convert(designation string) (Result, error)
func ConvertSimple(designation string) (string, error)

// Idempotent pack/unpack
func Pack(designation string) (string, error)
func Unpack(designation string) (string, error)

// Format detection
func DetectFormat(designation string) (Info, error)

// Helper functions
func ToReportFormat(minimal string) (string, error)
func FromReportFormat(report string) (string, error)
func HasFragment(desig string) bool
func GetFragment(desig string) (string, error)
func GetParent(desig string) (string, error)
func DesignationsEqual(desig1, desig2 string) bool
```

### Error Types

```go
var (
    ErrInvalidFormat    = errors.New("invalid format")
    ErrOutOfRange       = errors.New("out of range")
    ErrInvalidCharacter = errors.New("invalid character")
    ErrEmptyDesignation = errors.New("empty designation")
)
```

## Helper Functions

### Format Conversion (Minimal ↔ 12-Character Report Format)

Convert between minimal packed format and the 12-character MPC observation report format:

```go
// Minimal to 12-char report format
report, _ := mpc.ToReportFormat("0073Pa")   // "0073P      a"

// 12-char report format to minimal
minimal, _ := mpc.FromReportFormat("0073P      a")  // "0073Pa"
```

### Fragment Extraction

```go
// Check if designation has a fragment
if mpc.HasFragment("73P-A") {  // Returns true
    // Extract fragment (uppercase)
    frag, _ := mpc.GetFragment("73P-A")   // "A"
    frag, _ = mpc.GetFragment("73P-AA")   // "AA"

    // Get parent comet
    parent, _ := mpc.GetParent("73P-A")   // "73P"
    parent, _ = mpc.GetParent("0073Pa")   // "0073P"
}
```

### Designation Comparison

Compare designations across different formats:

```go
// Same object, different formats
mpc.DesignationsEqual("1995 XA", "J95X00A")  // Returns true
mpc.DesignationsEqual("73P-A", "0073Pa")     // Returns true

// Different objects
mpc.DesignationsEqual("73P-A", "73P-B")      // Returns false
```

## Comet Fragment Handling

The library supports comet fragment designations:

### Numbered Comets with Fragments

Numbered comets (like 73P) can have fragments:

```go
mpc.ConvertSimple("73P-A")    // Returns "0073Pa"
mpc.ConvertSimple("73P-AA")   // Returns "0073Paa"
mpc.ConvertSimple("0073Pa")   // Returns "73P-A"
mpc.ConvertSimple("0073Paa")  // Returns "73P-AA"
```

### Provisional Comets with Fragments

Provisional comets can also have fragments:

```go
mpc.ConvertSimple("P/1930 J1-A")   // Returns "PJ30J01a"
mpc.ConvertSimple("P/1930 J1-AA")  // Returns "PJ30J01aa"
mpc.ConvertSimple("PJ30J01aa")     // Returns "P/1930 J1-AA"
```

Fragment letters include all A-Z (including I, per MPC data).

## Testing

```bash
# Build all
make

# Run error handling tests (94 test cases)
make test-errors

# Run helper function tests (77 test cases)
make test-helpers

# Run conversion tests (2M+ test cases)
make test

# Run roundtrip tests (verifies pack(unpack(x)) == x and unpack(pack(x)) == x)
make test-roundtrip

# Run all tests
make test-all
```

## Cross-Compilation

```bash
# Build for multiple platforms
make build-all

# Individual platforms
make build-linux
make build-windows
make build-darwin
```

## Requirements

- Go 1.18+
- No external dependencies
