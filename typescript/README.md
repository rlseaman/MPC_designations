# MPC Designation Converter - TypeScript Implementation

Convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

## Requirements

- Node.js 14+
- npm or yarn

## Installation

```bash
npm install
```

## Building

```bash
npm run build
# or
make build
```

This compiles TypeScript to `dist/` with type declarations.

## Usage

### Command Line

```bash
# Using make
make run ARGS='1995 XA'
# Output: J95X00A

# With verbose output
make run ARGS='-v J95X00A'

# Or directly after building:
node dist/src/mpc_designation_cli.js '1995 XA'
```

### Library Usage (TypeScript)

```typescript
import {
    convert,
    convertSimple,
    pack,
    unpack,
    detectFormat,
    isValidDesignation,
    MPCDesignationError,
    ConversionResult,
    FormatInfo
} from 'mpc-designation-ts';

// Simple conversion
const packed: string = convertSimple('1995 XA');  // Returns 'J95X00A'
const unpacked: string = convertSimple('J95X00A');  // Returns '1995 XA'

// Full result with format info
const result: ConversionResult = convert('1995 XA');
console.log(`Input: ${result.input}`);
console.log(`Output: ${result.output}`);
console.log(`Format: ${result.info.format}`);  // 'unpacked'
console.log(`Type: ${result.info.type}`);      // 'provisional'

// Ensure specific format
const ensurePacked: string = pack('1995 XA');      // Returns 'J95X00A'
const ensureUnpacked: string = unpack('J95X00A');  // Returns '1995 XA'

// Format detection
const info: FormatInfo = detectFormat('1995 XA');
console.log(info.format);   // 'unpacked'
console.log(info.type);     // 'provisional'
console.log(info.subtype);  // 'provisional'

// Validation
const isValid: boolean = isValidDesignation('1995 XA');  // true

// Error handling
try {
    convert('invalid');
} catch (e) {
    if (e instanceof MPCDesignationError) {
        console.error(`Error: ${e.message}`);
    }
}
```

### Library Usage (JavaScript)

```javascript
const { convertSimple, pack, unpack } = require('mpc-designation-ts');

const packed = convertSimple('1995 XA');  // Returns 'J95X00A'
const unpacked = convertSimple('J95X00A');  // Returns '1995 XA'
```

## API Reference

### Types

```typescript
type FormatType = 'packed' | 'unpacked';

type DesignationType =
    | 'permanent'
    | 'provisional'
    | 'provisional_extended'
    | 'survey'
    | 'comet_numbered'
    | 'comet_provisional'
    | 'comet_full'
    | 'comet_ancient'
    | 'comet_bce'
    | 'satellite';

interface FormatInfo {
    format: FormatType;
    type: DesignationType;
    subtype: string;
}

interface ConversionResult {
    input: string;
    output: string;
    info: FormatInfo;
}
```

### Functions

| Function | Description |
|----------|-------------|
| `convert(designation: string): ConversionResult` | Convert with full result info |
| `convertSimple(designation: string): string` | Convert and return just the output |
| `pack(designation: string): string` | Ensure packed format |
| `unpack(designation: string): string` | Ensure unpacked format |
| `detectFormat(designation: string): FormatInfo` | Detect format without converting |
| `isValidDesignation(designation: string): boolean` | Check if valid |

### Constants

```typescript
const VERSION: string;           // '1.0.0'
const MAX_ASTEROID_NUMBER: number;  // 15396335
```

### Error Class

```typescript
class MPCDesignationError extends Error {
    name: 'MPCDesignationError';
}
```

## Testing

```bash
# Run all tests
make test-all
# or
npm run test:all

# Run specific test suites
make test-errors    # Error handling tests (94 cases)
make test-csv       # Conversion tests (2M+ cases)
make test-roundtrip # Round-trip tests
```

## Examples

```typescript
// Asteroids - permanent (numbered)
convertSimple('1');           // '00001'
convertSimple('100001');      // 'A0001'
convertSimple('620000');      // '~0000'

// Asteroids - provisional
convertSimple('1995 XA');     // 'J95X00A'
convertSimple('2024 AB631');  // '_4AMu1B'

// Asteroids - survey
convertSimple('2040 P-L');    // 'PLS2040'

// Comets
convertSimple('1P');          // '0001P'
convertSimple('C/1995 O1');   // 'CJ95O010'

// Natural satellites
convertSimple('S/2019 S 22'); // 'SK19S220'
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
| Provisional | C/1995 O1 | CJ95O010 |
| Fragment | D/1993 F2-B | DJ93F02b |

### Natural Satellites

| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |

## License

CC0 1.0 Universal - Public Domain Dedication
