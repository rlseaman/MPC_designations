# MPC Designation Converter - IRAF CL Task Interface

This directory contains the IRAF CL (Command Language) task interface for the
MPC designation converter. The task provides a standard IRAF parameter interface
for converting between packed and unpacked MPC designations.

**Note:** This task is designed to be run from within the IRAF CL environment,
where parameters are handled through the standard IRAF parameter system. For
standalone command-line usage outside IRAF, use the interactive example task
(`example_usage.x`) or the test executables.

## Files

```
cl/
├── t_mpcdes.x    # SPP task implementation
├── mpcdes.par    # Parameter file
├── mpcpkg.cl     # Package definition script
└── README.md     # This file
```

## Building

From the `cl/` directory:

```bash
# Source IRAF environment
source ~/.iraf/setup.sh

# Copy source and build (xc requires local files)
cp ../src/mpc_designation.x .
xc t_mpcdes.x mpc_designation.x -o mpcdes.e
```

Or from the `build/` directory:

```bash
cd spp/build
cp ../cl/t_mpcdes.x ../cl/mpcdes.par .
cp ../src/mpc_designation.x .
xc t_mpcdes.x mpc_designation.x -o mpcdes.e
```

**Note:** The xc compiler requires source files in the current directory.
Relative paths like `../src/mpc_designation.x` may cause linkage issues.

## Installation

### Method 1: Manual Task Definition

From the CL prompt:

```
cl> task mpcdes = "/path/to/mpcdes.e"
```

### Method 2: Package Script

Add to your `login.cl` or `loginuser.cl`:

```
set mpcpkg = "/path/to/spp/cl/"
cl < mpcpkg$mpcpkg.cl
```

### Method 3: Define in login.cl

Add these lines to your `login.cl`:

```
set mpcpkg = "/path/to/spp/cl/"
task mpcdes = "mpcpkg$mpcdes.e"
```

## Usage

### Basic Conversion

```
cl> mpcdes "1995 XA"
J95X00A

cl> mpcdes "J95X00A"
1995 XA

cl> mpcdes 1
00001
```

### Verbose Mode

```
cl> mpcdes "1995 XA" verbose+
  Input:    1995 XA
  Detected: unpacked format, provisional asteroid
  Action:   packing to MPC compact form
  Output:   J95X00A
```

### Batch Processing

Create a file `designations.txt`:
```
1995 XA
2024 KS167
C/1995 O1
S/2019 S 22
```

Convert all:
```
cl> mpcdes @designations.txt
J95X00A
K24KG7S
CJ95O010
SK19S220

cl> mpcdes @designations.txt output=results.txt
```

### Using Parameters

```
cl> epar mpcdes              # Edit parameters interactively
cl> lpar mpcdes              # List parameters
cl> mpcdes.result            # Access last result
```

## Parameters

| Parameter | Type   | Mode | Default | Description |
|-----------|--------|------|---------|-------------|
| input     | string | a    | -       | Designation to convert (or @file) |
| output    | string | h    | STDOUT  | Output file |
| verbose   | bool   | h    | no      | Print detailed output |
| result    | string | h    | ""      | Last conversion result (output) |

### Parameter Modes

- `a` = auto (prompted if not provided)
- `h` = hidden (uses default unless explicitly set)
- `q` = query (always prompted)
- `l` = learned (remembered between calls)

## Examples

```
# Convert asteroid designations
cl> mpcdes "1995 XA"                    # -> J95X00A
cl> mpcdes "100000"                     # -> A0000
cl> mpcdes "620000"                     # -> ~0000

# Convert comet designations
cl> mpcdes "1P"                         # -> 0001P
cl> mpcdes "C/1995 O1"                  # -> CJ95O010
cl> mpcdes "D/1993 F2-B"                # -> DJ93F02b

# Convert satellite designations
cl> mpcdes "S/2019 S 22"                # -> SK19S220

# Reverse conversions
cl> mpcdes "J95X00A"                    # -> 1995 XA
cl> mpcdes "CJ95O010"                   # -> C/1995 O1

# Access result parameter
cl> mpcdes "1995 XA"
cl> = mpcdes.result
J95X00A
```

## Integration with IRAF Scripts

Use in CL scripts:

```cl
procedure myproc (designation)

string designation {prompt="Enter designation"}

begin
    string des, result

    des = designation

    # Convert and capture result
    mpcdes (des)
    result = mpcdes.result

    print ("Converted: ", result)
end
```

## See Also

- [IRAF CL Programmer's Manual](https://iraf.readthedocs.io/en/latest/clman.html)
- [IRAF Task Definition](https://iraf.readthedocs.io/en/latest/tasks/language/task.html)
- [IRAF Parameters](https://iraf.readthedocs.io/en/latest/tasks/language/parameters.html)
- [MPC Packed Designation Format](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
