# MPC Designation Converter - Ruby Implementation

Ruby library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```ruby
require_relative 'src/mpc_designation'

result = MPCDesignation.convert_simple('1995 XA')  # Returns 'J95X00A'
back = MPCDesignation.convert_simple('J95X00A')    # Returns '1995 XA'
```

## Requirements

- Ruby 2.7 or later (uses `frozen_string_literal`)

## CLI Usage

```bash
# Single conversion
ruby src/mpc_designation_cli.rb '1995 XA'
# Output: J95X00A

# Multiple designations
ruby src/mpc_designation_cli.rb 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
ruby src/mpc_designation_cli.rb -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## Library Usage

### Simple Conversion

```ruby
require_relative 'src/mpc_designation'

# Auto-detect and convert
packed = MPCDesignation.convert_simple('1995 XA')    # Returns 'J95X00A'
unpacked = MPCDesignation.convert_simple('J95X00A')  # Returns '1995 XA'
```

### With Format Information

```ruby
require_relative 'src/mpc_designation'

result = MPCDesignation.convert('1995 XA')
puts result['input']              # '1995 XA'
puts result['output']             # 'J95X00A'
puts result['info']['format']     # 'unpacked'
puts result['info']['subtype']    # 'provisional'
```

### Format Detection

```ruby
require_relative 'src/mpc_designation'

info = MPCDesignation.detect_format('1995 XA')
puts info['format']  # 'unpacked'
puts info['type']    # 'provisional'
```

### Validation

```ruby
require_relative 'src/mpc_designation'

puts MPCDesignation.valid_designation?('1995 XA')  # true
puts MPCDesignation.valid_designation?('invalid')  # false
```

### Pack/Unpack

```ruby
require_relative 'src/mpc_designation'

# Ensure packed format
p = MPCDesignation.pack('1995 XA')    # 'J95X00A'
p2 = MPCDesignation.pack('J95X00A')   # 'J95X00A' (already packed)

# Ensure unpacked format
u = MPCDesignation.unpack('J95X00A')  # '1995 XA'
u2 = MPCDesignation.unpack('1995 XA') # '1995 XA' (already unpacked)
```

## API Reference

### Module Methods

#### `MPCDesignation.convert_simple(designation) -> String`

Convert a designation, returning just the result string.

#### `MPCDesignation.convert(designation) -> Hash`

Convert a designation with detailed information. Returns a hash with keys:
- `'input'`: Original input string
- `'output'`: Converted designation
- `'info'`: Hash with format information

#### `MPCDesignation.detect_format(designation) -> Hash`

Detect the format without converting. Returns a hash with keys:
- `'format'`: 'packed' or 'unpacked'
- `'type'`: e.g., 'permanent', 'provisional', 'comet_full'
- `'subtype'`: Human-readable description

#### `MPCDesignation.pack(designation) -> String`

Ensure a designation is in packed format.

#### `MPCDesignation.unpack(designation) -> String`

Ensure a designation is in unpacked format.

#### `MPCDesignation.valid_designation?(designation) -> Boolean`

Check if a string is a valid MPC designation.

### Helper Functions

#### `MPCDesignation.to_report_format(minimal) -> String`

Convert minimal packed format to 12-character MPC report format.

#### `MPCDesignation.from_report_format(report) -> String`

Convert 12-character MPC report format to minimal packed format.

#### `MPCDesignation.has_fragment?(desig) -> Boolean`

Check if a designation has a comet fragment suffix.

#### `MPCDesignation.get_fragment(desig) -> String`

Extract fragment suffix (uppercase) from a comet designation.

#### `MPCDesignation.get_parent(desig) -> String`

Get parent designation without fragment.

#### `MPCDesignation.designations_equal?(d1, d2) -> Boolean`

Compare designations across different formats.

## Helper Function Examples

### Format Conversion (Minimal ↔ 12-Character Report Format)

```ruby
require_relative 'src/mpc_designation'

# Minimal to 12-char report format
MPCDesignation.to_report_format('0073Pa')   # '0073P      a'
MPCDesignation.to_report_format('00001')    # '       00001'

# 12-char report format to minimal
MPCDesignation.from_report_format('0073P      a')  # '0073Pa'
MPCDesignation.from_report_format('       00001')  # '00001'
```

### Fragment Extraction

```ruby
require_relative 'src/mpc_designation'

# Check if designation has a fragment
MPCDesignation.has_fragment?('73P-A')   # true
MPCDesignation.has_fragment?('73P')     # false
MPCDesignation.has_fragment?('0073Pa')  # true (packed)

# Extract fragment (uppercase)
MPCDesignation.get_fragment('73P-A')    # 'A'
MPCDesignation.get_fragment('73P-AA')   # 'AA'
MPCDesignation.get_fragment('0073Paa')  # 'AA'

# Get parent comet
MPCDesignation.get_parent('73P-A')   # '73P'
MPCDesignation.get_parent('0073Pa')  # '0073P'
```

### Designation Comparison

```ruby
require_relative 'src/mpc_designation'

# Same object, different formats
MPCDesignation.designations_equal?('1995 XA', 'J95X00A')  # true
MPCDesignation.designations_equal?('73P-A', '0073Pa')     # true

# Different objects
MPCDesignation.designations_equal?('73P-A', '73P-B')      # false
```

### Exceptions

#### `MPCDesignation::Error`

Raised for invalid designations. Inherits from `StandardError`.

## Testing

```bash
# Run conversion tests
make test

# Run round-trip tests
make test-roundtrip

# Run all tests
make test-all
```

## Performance

Ruby is an interpreted language and will be slower than compiled implementations, but should still handle millions of conversions efficiently.
