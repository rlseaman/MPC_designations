# MPC Designation Converter - Octave/MATLAB

Convert between packed and unpacked Minor Planet Center (MPC) designations.

Compatible with GNU Octave and MATLAB.

## Quick Start

```matlab
% Add the src directory to your path
addpath('src');

% Or source the file directly (Octave)
source('src/mpc_designation.m');

% Simple conversion (auto-detects format)
mpc_convert_simple('1995 XA')     % Returns 'J95X00A'
mpc_convert_simple('J95X00A')     % Returns '1995 XA'

% Ensure packed format
mpc_pack('1995 XA')               % Returns 'J95X00A'
mpc_pack('J95X00A')               % Returns 'J95X00A' (already packed)

% Ensure unpacked format
mpc_unpack('J95X00A')             % Returns '1995 XA'
mpc_unpack('1995 XA')             % Returns '1995 XA' (already unpacked)
```

## Command-Line Usage (Octave)

```bash
octave --no-gui src/mpc_designation_cli.m '1995 XA'
# Output: J95X00A

octave --no-gui src/mpc_designation_cli.m J95X00A
# Output: 1995 XA

octave --no-gui src/mpc_designation_cli.m --verbose '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## API Reference

### High-Level Functions

| Function | Description |
|----------|-------------|
| `mpc_convert_simple(des)` | Auto-detect and convert, return string |
| `mpc_convert(des)` | Auto-detect and convert, return struct with metadata |
| `mpc_pack(des)` | Ensure packed format |
| `mpc_unpack(des)` | Ensure unpacked format |
| `mpc_is_valid(des)` | Check if valid (returns true/false) |
| `mpc_detect_format(des)` | Return format info without converting |

### Type-Specific Functions

| Function | Description |
|----------|-------------|
| `mpc_pack_permanent(number)` | Pack numbered asteroid |
| `mpc_unpack_permanent(packed)` | Unpack to asteroid number |
| `mpc_pack_provisional(unpacked)` | Pack provisional asteroid |
| `mpc_unpack_provisional(packed)` | Unpack provisional asteroid |
| `mpc_pack_comet_numbered(unpacked)` | Pack numbered comet |
| `mpc_unpack_comet_numbered(packed)` | Unpack numbered comet |
| `mpc_pack_comet_full(unpacked)` | Pack comet with type prefix |
| `mpc_unpack_comet_full(packed)` | Unpack comet with type prefix |
| `mpc_pack_satellite(unpacked)` | Pack satellite designation |
| `mpc_unpack_satellite(packed)` | Unpack satellite designation |

## Examples

```matlab
source('src/mpc_designation.m');

% Permanent (numbered) asteroids
mpc_pack_permanent(1)           % '00001'
mpc_pack_permanent(100001)      % 'A0001'
mpc_pack_permanent(620000)      % '~0000'
mpc_unpack_permanent('A0001')   % 100001

% Provisional asteroids
mpc_pack_provisional('1995 XA')     % 'J95X00A'
mpc_pack_provisional('2024 AB631')  % '_OA004S' (extended format)
mpc_pack_provisional('2040 P-L')    % 'PLS2040' (survey)

% Comets
mpc_pack_comet_numbered('1P')           % '0001P'
mpc_pack_comet_full('C/1995 O1')        % 'CJ95O010'
mpc_pack_comet_full('D/1993 F2-B')      % 'DJ93F02b'

% Satellites
mpc_pack_satellite('S/2019 S 22')       % 'SK19S220'
mpc_unpack_satellite('SK19S220')        % 'S/2019 S 22'

% Format detection
info = mpc_detect_format('1995 XA');
% info.format = 'unpacked'
% info.type = 'provisional'

% Validation
mpc_is_valid('1995 XA')     % true
mpc_is_valid('invalid')     % false
```

## Batch Processing

```matlab
source('src/mpc_designation.m');

% Process multiple designations
designations = {'1995 XA', '2024 AB1', 'C/1995 O1', '1P', 'S/2019 S 22'};

for i = 1:length(designations)
    try
        result = mpc_convert_simple(designations{i});
        fprintf('%s -> %s\n', designations{i}, result);
    catch err
        fprintf('%s -> Error: %s\n', designations{i}, err.message);
    end
end
```

## Testing

```bash
# Quick validation tests
make test-quick

# Error handling tests
make test-errors

# Full CSV test suite (2M+ tests)
make test-csv

# All tests
make test
```

## Supported Formats

### Asteroids
- **Permanent (numbered)**: 1 - 15,396,335
- **Provisional**: 1995 XA, 2024 AB631
- **Survey**: 2040 P-L, 3138 T-1

### Comets
- **Numbered**: 1P, 354P
- **Provisional**: C/1995 O1, P/2019 A4
- **Fragments**: D/1993 F2-B
- **Ancient/BCE**: C/240 V1, C/-146 P1

### Natural Satellites
- **Provisional**: S/2019 S 22, S/2003 J 2

## Requirements

- GNU Octave 4.0+ or MATLAB R2016a+
- No external toolboxes required

## MATLAB Compatibility

The code is compatible with both Octave and MATLAB. For MATLAB:

```matlab
% Run the script to load functions
run('src/mpc_designation.m');

% Or add to path
addpath('src');
```

Note: The CLI script uses Octave-specific features. For MATLAB command-line use, call the functions directly from a script.

## License

CC0 1.0 Universal - Public Domain Dedication
