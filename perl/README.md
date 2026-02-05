# MPC Designation Converter - Perl Implementation

Perl library for converting between packed and unpacked MPC designations.

## Quick Start

```perl
use MPC::Designation qw(pack unpack convert_simple);

my $packed = pack('1995 XA');      # Returns 'J95X00A'
my $unpacked = unpack('J95X00A');  # Returns '1995 XA'
```

## Requirements

- Perl 5.10 or later

## Installation

Add the `src` directory to your Perl library path:

```perl
use lib '/path/to/perl/src';
use MPC::Designation qw(pack unpack convert_simple);
```

## CLI Usage

```bash
# Single conversion
perl src/MPC/mpc_designation_cli.pl '1995 XA'
# Output: J95X00A

# Multiple designations
perl src/MPC/mpc_designation_cli.pl 1 '1995 XA' 'C/1995 O1'

# Verbose mode
perl src/MPC/mpc_designation_cli.pl -v '1995 XA'
```

## Library Usage

### Basic Functions

```perl
use MPC::Designation qw(pack unpack convert_simple detect_format);

# Auto-detect and convert
my $packed = convert_simple('1995 XA');    # Returns 'J95X00A'
my $unpacked = convert_simple('J95X00A');  # Returns '1995 XA'

# Ensure specific format
my $p = pack('1995 XA');      # Returns 'J95X00A'
my $u = unpack('J95X00A');    # Returns '1995 XA'

# Format detection
my $info = detect_format('1995 XA');
print $info->{format};  # 'unpacked'
print $info->{type};    # 'provisional'
```

### Helper Functions

```perl
use MPC::Designation qw(
    to_report_format from_report_format
    has_fragment get_fragment get_parent designations_equal
);

# Convert between minimal and 12-char report format
my $report = to_report_format('0073Pa');    # '0073P      a'
my $minimal = from_report_format('0073P      a');  # '0073Pa'

# Check for fragment
if (has_fragment('73P-A')) {
    my $frag = get_fragment('73P-A');    # 'A'
    my $parent = get_parent('73P-A');    # '73P'
}

# Compare designations across formats
if (designations_equal('1995 XA', 'J95X00A')) {
    print "Same object!\n";
}
```

## API Reference

### Core Functions

| Function | Description |
|----------|-------------|
| `pack($designation)` | Convert to packed format |
| `unpack($designation)` | Convert to unpacked format |
| `convert_simple($designation)` | Auto-detect and convert |
| `convert($designation)` | Convert with full result info |
| `detect_format($designation)` | Detect format without converting |

### Helper Functions

| Function | Description |
|----------|-------------|
| `to_report_format($minimal)` | Convert minimal packed to 12-char report format |
| `from_report_format($report)` | Convert 12-char report format to minimal packed |
| `has_fragment($desig)` | Check if designation has comet fragment (returns 1/0) |
| `get_fragment($desig)` | Extract fragment suffix (uppercase) |
| `get_parent($desig)` | Get parent designation without fragment |
| `designations_equal($d1, $d2)` | Compare designations across formats (returns 1/0) |

## Helper Function Examples

### Format Conversion (Minimal to 12-Character Report Format)

```perl
use MPC::Designation qw(to_report_format from_report_format);

# Minimal to 12-char report format
to_report_format('0073Pa');   # '0073P      a'
to_report_format('00001');    # '       00001'

# 12-char report format to minimal
from_report_format('0073P      a');  # '0073Pa'
from_report_format('       00001');  # '00001'
```

### Fragment Extraction

```perl
use MPC::Designation qw(has_fragment get_fragment get_parent);

# Check if designation has a fragment
has_fragment('73P-A');   # 1 (true)
has_fragment('73P');     # 0 (false)
has_fragment('0073Pa');  # 1 (true, packed)

# Extract fragment (uppercase)
get_fragment('73P-A');    # 'A'
get_fragment('73P-AA');   # 'AA'
get_fragment('0073Paa');  # 'AA'

# Get parent comet
get_parent('73P-A');   # '73P'
get_parent('0073Pa');  # '0073P'
```

### Designation Comparison

```perl
use MPC::Designation qw(designations_equal);

# Same object, different formats
designations_equal('1995 XA', 'J95X00A');  # 1 (true)
designations_equal('73P-A', '0073Pa');     # 1 (true)

# Different objects
designations_equal('73P-A', '73P-B');      # 0 (false)
```

## Testing

```bash
# Run CSV conversion tests
perl test/test_csv.pl ../test-data/prov_unpack_to_pack.csv

# Run error handling tests
perl test/test_errors.pl ../test-data/error_test_cases.csv

# Run helper function tests
perl test/test_helpers.pl
```

## License

CC0 1.0 Universal - Public Domain Dedication
