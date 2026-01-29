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
