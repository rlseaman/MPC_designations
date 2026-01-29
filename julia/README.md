# MPC Designation Converter - Julia

Julia implementation of the MPC designation converter.

## Requirements

- Julia 1.6 or later

## Usage

### Command Line

```bash
# Convert designations
julia src/mpc_designation_cli.jl '1995 XA'      # Output: J95X00A
julia src/mpc_designation_cli.jl J95X00A        # Output: 1995 XA
julia src/mpc_designation_cli.jl 1              # Output: 00001
julia src/mpc_designation_cli.jl 'C/1995 O1'    # Output: CJ95O010

# Verbose output
julia src/mpc_designation_cli.jl -v '1995 XA'

# Multiple designations
julia src/mpc_designation_cli.jl 1 2 3 'C/1995 O1'
```

### Library API

```julia
# Add the src directory to load path
push!(LOAD_PATH, "path/to/julia/src")
using MPCDesignation

# Simple conversion (auto-detects format)
packed = convert_simple("1995 XA")   # Returns "J95X00A"
unpacked = convert_simple("J95X00A") # Returns "1995 XA"

# Full result with metadata
result = convert_designation("1995 XA")
println(result.output)           # "J95X00A"
println(result.info.format)      # :packed or :unpacked
println(result.info.type)        # "provisional", "permanent", etc.
println(result.info.subtype)     # Detailed description

# Force specific direction
packed = pack("1995 XA")      # Always returns packed
unpacked = unpack("J95X00A")  # Always returns unpacked

# Validation
is_valid_designation("1995 XA")   # true
is_valid_designation("invalid")   # false

# Low-level functions
number = unpack_permanent("A0001")  # Returns 100001
packed = pack_permanent(100001)      # Returns "A0001"
```

## Testing

```bash
# Run all tests
make test-all

# Run specific tests
make test-errors     # Error handling tests
make test-csv        # Conversion tests (2M+ cases)
make test-roundtrip  # Round-trip tests
```

## Project Structure

```
julia/
├── README.md
├── Makefile
├── src/
│   ├── MPCDesignation.jl       # Core module
│   └── mpc_designation_cli.jl  # CLI tool
└── test/
    ├── test_csv.jl        # Conversion tests
    ├── test_errors.jl     # Error handling tests
    └── test_roundtrip.jl  # Round-trip tests
```

## API Reference

### High-Level Functions

| Function | Description |
|----------|-------------|
| `convert_designation(s)` | Convert and return full result |
| `convert_simple(s)` | Convert and return just the output |
| `pack(s)` | Ensure result is packed |
| `unpack(s)` | Ensure result is unpacked |
| `detect_format(s)` | Detect format without converting |
| `is_valid_designation(s)` | Check if valid (no exception) |

### Low-Level Functions

| Function | Description |
|----------|-------------|
| `pack_permanent(n)` | Pack numbered asteroid |
| `unpack_permanent(s)` | Unpack numbered asteroid |
| `pack_provisional(s)` | Pack provisional asteroid |
| `unpack_provisional(s)` | Unpack provisional asteroid |
| `pack_comet_full(s)` | Pack full comet designation |
| `unpack_comet_full(s)` | Unpack full comet designation |
| `pack_satellite(s)` | Pack satellite designation |
| `unpack_satellite(s)` | Unpack satellite designation |

### Types

```julia
# Format information
struct FormatInfo
    format::Symbol   # :packed or :unpacked
    type::String     # "permanent", "provisional", etc.
    subtype::String  # Detailed description
end

# Conversion result
struct ConversionResult
    input::String
    output::String
    info::FormatInfo
end

# Exception type
struct MPCDesignationError <: Exception
    msg::String
end
```

### Exception Handling

```julia
try
    result = convert_simple("invalid")
catch e
    if e isa MPCDesignationError
        println("Error: ", e.msg)
    end
end
```

## Examples

### Asteroids

```julia
# Numbered asteroids
convert_simple("1")          # "00001"
convert_simple("100001")     # "A0001"
convert_simple("620000")     # "~0000"

# Provisional asteroids
convert_simple("1995 XA")    # "J95X00A"

# Survey designations
convert_simple("2040 P-L")   # "PLS2040"
```

### Comets

```julia
# Numbered comets
convert_simple("1P")         # "0001P"
convert_simple("0001P")      # "1P"

# Provisional comets
convert_simple("C/1995 O1")  # "CJ95O010"
convert_simple("D/1993 F2-B") # "DJ93F02b"
```

### Satellites

```julia
convert_simple("S/2019 S 22")  # "SK19S220"
convert_simple("SK19S220")     # "S/2019 S 22"
```
