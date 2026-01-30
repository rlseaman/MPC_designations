# MPC Designation Converter - C#

C# implementation of the Minor Planet Center (MPC) designation converter.

## Requirements

- .NET 8.0 SDK or later

## Quick Start

### Build

```bash
dotnet build -c Release
```

### CLI Usage

```bash
# Convert designations
dotnet run -- '1995 XA'
# Output: J95X00A

dotnet run -- J95X00A
# Output: 1995 XA

# Multiple designations
dotnet run -- 1 J95X00A '1995 XA'
# Output:
# 1 -> 00001
# J95X00A -> 1995 XA
# 1995 XA -> J95X00A

# Verbose mode
dotnet run -- -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

### Library Usage

```csharp
using MPC;

// Simple conversion (auto-detects format)
string packed = MPCDesignation.ConvertSimple("1995 XA");  // Returns "J95X00A"
string unpacked = MPCDesignation.ConvertSimple("J95X00A");  // Returns "1995 XA"

// Explicit pack/unpack
packed = MPCDesignation.Pack("1995 XA");    // Returns "J95X00A"
unpacked = MPCDesignation.Unpack("J95X00A");  // Returns "1995 XA"

// Full conversion with metadata
var result = MPCDesignation.Convert("1995 XA");
Console.WriteLine($"Input: {result.Input}");
Console.WriteLine($"Output: {result.Output}");
Console.WriteLine($"Format: {result.Info.Format}");
Console.WriteLine($"Subtype: {result.Info.Subtype}");

// Format detection
var info = MPCDesignation.DetectFormat("1995 XA");
// info.Format == Format.Unpacked
// info.Type == DesignationType.Provisional

// Validation
if (MPCDesignation.IsValid("1995 XA"))
{
    Console.WriteLine("Valid designation");
}

// Error handling
try
{
    MPCDesignation.Convert("invalid");
}
catch (MPCDesignationException e)
{
    Console.WriteLine($"Error: {e.Message}");
}
```

## API Reference

### Namespace `MPC`

#### Constants

- `MPCDesignation.VERSION` - Library version string
- `MPCDesignation.MAX_ASTEROID_NUMBER` - Maximum supported asteroid number (15396335)

#### Enums

```csharp
public enum Format { Unknown, Packed, Unpacked }

public enum DesignationType
{
    Unknown, Permanent, Provisional, ProvisionalExtended, Survey,
    CometNumbered, CometProvisional, CometFull, CometAncient, CometBCE,
    Satellite
}
```

#### Classes

```csharp
public class FormatInfo
{
    public Format Format { get; set; }
    public DesignationType Type { get; set; }
    public string Subtype { get; set; }
}

public class ConversionResult
{
    public string Input { get; set; }
    public string Output { get; set; }
    public FormatInfo Info { get; set; }
}
```

#### Static Class `MPCDesignation`

Methods:

- `Convert(designation) -> ConversionResult` - Full conversion with metadata
- `ConvertSimple(designation) -> string` - Simple conversion, output only
- `Pack(designation) -> string` - Convert to packed (no-op if already packed)
- `Unpack(designation) -> string` - Convert to unpacked (no-op if already unpacked)
- `DetectFormat(designation) -> FormatInfo` - Detect format without converting
- `IsValid(designation) -> bool` - Check if valid
- `PackPermanent(number) -> string` - Pack asteroid number
- `UnpackPermanent(packed) -> long` - Unpack to asteroid number

#### Exception Class `MPCDesignationException`

Thrown for invalid designations. Inherits from `System.Exception`.

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
make test

# Run specific tests
make test-errors    # Error handling tests (94 cases)
make test-csv       # Conversion tests (2M+ entries)
make test-roundtrip # Roundtrip verification

# Help
make help
```

## Files

```
csharp/
├── README.md               # This file
├── Makefile                # Build and test commands
├── MpcDesignation.csproj   # Main project file
├── src/
│   ├── MPCDesignation.cs   # Core library
│   └── Program.cs          # CLI tool
└── test/
    ├── TestCsv.cs          # CSV conversion tests
    ├── TestCsv.csproj
    ├── TestErrors.cs       # Error handling tests
    ├── TestErrors.csproj
    ├── TestRoundtrip.cs    # Roundtrip tests
    └── TestRoundtrip.csproj
```

## License

CC0 1.0 Universal - Public Domain Dedication
