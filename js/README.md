# MPC Designation Converter - JavaScript Implementation

Node.js library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```javascript
const { convertSimple } = require('./src/mpc_designation');

const result = convertSimple('1995 XA');  // Returns 'J95X00A'
const back = convertSimple('J95X00A');    // Returns '1995 XA'
```

## Installation

Copy `src/mpc_designation.js` to your project, or use the package directly:

```bash
cd js
npm install  # (no dependencies required)
```

## CLI Usage

```bash
# Single conversion
node src/mpc_designation_cli.js '1995 XA'
# Output: J95X00A

# Multiple designations
node src/mpc_designation_cli.js 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
node src/mpc_designation_cli.js -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## Library Usage

### Simple Conversion

```javascript
const { convertSimple, MPCDesignationError } = require('./src/mpc_designation');

try {
    const packed = convertSimple('1995 XA');    // Returns 'J95X00A'
    const unpacked = convertSimple('J95X00A');  // Returns '1995 XA'
} catch (e) {
    if (e instanceof MPCDesignationError) {
        console.error(`Error: ${e.message}`);
    }
}
```

### With Format Information

```javascript
const { convert } = require('./src/mpc_designation');

const result = convert('1995 XA');
console.log(result.input);        // '1995 XA'
console.log(result.output);       // 'J95X00A'
console.log(result.info.format);  // 'unpacked'
console.log(result.info.subtype); // 'provisional'
```

### Format Detection

```javascript
const { detectFormat } = require('./src/mpc_designation');

const info = detectFormat('1995 XA');
console.log(info.format);  // 'unpacked'
console.log(info.type);    // 'provisional'
```

### Validation

```javascript
const { isValidDesignation } = require('./src/mpc_designation');

console.log(isValidDesignation('1995 XA'));   // true
console.log(isValidDesignation('invalid'));   // false
```

### Batch Processing

```javascript
const { convertSimple, MPCDesignationError } = require('./src/mpc_designation');

const designations = ['1', '1995 XA', 'C/1995 O1'];
for (const des of designations) {
    try {
        const result = convertSimple(des);
        console.log(`${des} -> ${result}`);
    } catch (e) {
        console.log(`${des}: Error - ${e.message}`);
    }
}
```

## API Reference

### Functions

#### `convertSimple(designation: string): string`

Convert a designation, returning just the result string.

- **Parameters:** `designation` - The MPC designation to convert
- **Returns:** The converted designation
- **Throws:** `MPCDesignationError` if the input is invalid

#### `convert(designation: string): object`

Convert a designation, returning detailed information.

- **Parameters:** `designation` - The MPC designation to convert
- **Returns:** Object with properties:
  - `input`: Original input
  - `output`: Converted designation
  - `info`: Object with `format`, `type`, and `subtype`

#### `detectFormat(designation: string): object`

Detect the format of a designation without converting.

- **Parameters:** `designation` - The MPC designation to analyze
- **Returns:** Object with format information

#### `pack(designation: string): string`

Ensure a designation is in packed format.

#### `unpack(designation: string): string`

Ensure a designation is in unpacked format.

#### `isValidDesignation(designation: string): boolean`

Check if a string is a valid MPC designation.

### Helper Functions

#### `toReportFormat(minimal: string): string`

Convert minimal packed format to 12-character MPC report format.

```javascript
const { toReportFormat } = require('./src/mpc_designation');

toReportFormat('0073Pa');   // '0073P      a'
toReportFormat('00001');    // '       00001'
```

#### `fromReportFormat(report: string): string`

Convert 12-character MPC report format to minimal packed format.

```javascript
const { fromReportFormat } = require('./src/mpc_designation');

fromReportFormat('0073P      a');  // '0073Pa'
fromReportFormat('       00001');  // '00001'
```

#### `hasFragment(desig: string): boolean`

Check if a designation has a comet fragment suffix.

```javascript
const { hasFragment } = require('./src/mpc_designation');

hasFragment('73P-A');   // true
hasFragment('73P');     // false
hasFragment('0073Pa');  // true (packed)
```

#### `getFragment(desig: string): string`

Extract the fragment suffix from a comet designation (uppercase).

```javascript
const { getFragment } = require('./src/mpc_designation');

getFragment('73P-A');    // 'A'
getFragment('0073Paa');  // 'AA'
getFragment('73P');      // ''
```

#### `getParent(desig: string): string`

Get the parent comet designation without fragment suffix.

```javascript
const { getParent } = require('./src/mpc_designation');

getParent('73P-A');    // '73P'
getParent('0073Paa');  // '0073P'
```

#### `designationsEqual(desig1: string, desig2: string): boolean`

Check if two designations refer to the same object.

```javascript
const { designationsEqual } = require('./src/mpc_designation');

designationsEqual('1995 XA', 'J95X00A');  // true
designationsEqual('73P-A', '0073Pa');     // true
designationsEqual('73P-A', '73P-B');      // false
```

### Exceptions

#### `MPCDesignationError`

Thrown when a designation cannot be converted.

```javascript
const { convertSimple, MPCDesignationError } = require('./src/mpc_designation');

try {
    convertSimple('invalid');
} catch (e) {
    if (e instanceof MPCDesignationError) {
        console.error(`Invalid designation: ${e.message}`);
    }
}
```

## Testing

```bash
# Run all tests
make test-all

# Run specific tests
make test-errors    # Error handling tests (94 cases)
make test-helpers   # Helper function tests (77 cases)
make test-csv       # Conversion tests (2M+ cases)
make test-roundtrip # Round-trip tests

# Or run directly with node
node test/test_helpers.js
node test/test_errors.js ../test-data/error_test_cases.csv
node test/test_csv.js ../test-data/prov_unpack_to_pack.csv
```

## Performance

JavaScript is one of the fastest implementations:

- **Pack:** ~2.4 million designations/second
- **Unpack:** ~4.4 million designations/second

## Requirements

- Node.js 14.0.0 or later
- No external dependencies
