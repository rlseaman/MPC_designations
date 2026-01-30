# MPC Designation Converter - Haskell Implementation

Convert between packed and unpacked Minor Planet Center (MPC) designations.

## Requirements

- GHC (Glasgow Haskell Compiler) 8.0 or later

Install on macOS:
```bash
brew install ghc
```

## Building

```bash
make build
```

This creates:
- `build/mpc_designation` - Command-line tool
- `build/test_csv` - CSV test runner
- `build/test_errors` - Error handling tests
- `build/test_roundtrip` - Round-trip verification tests

## Usage

### Command Line

```bash
./build/mpc_designation '1995 XA'      # Output: J95X00A
./build/mpc_designation J95X00A        # Output: 1995 XA
./build/mpc_designation -v '1995 XA'   # Verbose output
```

### As a Library

```haskell
import MPCDesignation

-- Simple conversion (auto-detect and flip format)
case convertSimple "1995 XA" of
    Right result -> putStrLn result  -- "J95X00A"
    Left err -> print err

-- Ensure packed format
case pack "1995 XA" of
    Right packed -> putStrLn packed  -- "J95X00A"
    Left err -> print err

-- Ensure unpacked format
case unpack "J95X00A" of
    Right unpacked -> putStrLn unpacked  -- "1995 XA"
    Left err -> print err

-- Format detection
case detectFormat "J95X00A" of
    Right info -> print info  -- FormatInfo Packed Provisional "provisional"
    Left err -> print err
```

## API

### Core Functions

```haskell
-- Auto-detect format and convert
convertSimple :: String -> Either MPCError String

-- Ensure packed format (idempotent)
pack :: String -> Either MPCError String

-- Ensure unpacked format (idempotent)
unpack :: String -> Either MPCError String

-- Detect format without converting
detectFormat :: String -> Either MPCError FormatInfo
```

### Types

```haskell
data MPCError = MPCError String

data Format = Packed | Unpacked

data DesignationType
    = Permanent
    | Provisional
    | ProvisionalExtended
    | Survey
    | CometNumbered
    | CometProvisional
    | CometFull
    | CometAncient
    | CometBCE
    | Satellite

data FormatInfo = FormatInfo
    { formatType   :: Format
    , designation  :: DesignationType
    , subtype      :: String
    }
```

## Testing

```bash
make test          # Run CSV conversion tests (2M+ cases)
make test-errors   # Run error handling tests (94 cases)
make test-roundtrip # Run round-trip verification
make test-all      # Run all tests
```

## Examples

```haskell
-- Asteroids
convertSimple "1"           -- Right "00001"
convertSimple "100001"      -- Right "A0001"
convertSimple "1995 XA"     -- Right "J95X00A"
convertSimple "J95X00A"     -- Right "1995 XA"

-- Comets
convertSimple "1P"          -- Right "0001P"
convertSimple "C/1995 O1"   -- Right "CJ95O010"

-- Satellites
convertSimple "S/2019 S 22" -- Right "SK19S220"
```

## License

CC0 1.0 Universal - Public Domain Dedication
