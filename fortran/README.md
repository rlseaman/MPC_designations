# MPC Designation Converter - Fortran Implementation

Convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

## Requirements

- gfortran (GCC Fortran compiler) or other Fortran 2008 compatible compiler
- GNU Make

## Building

```bash
make build
```

This creates:
- `build/mpc_designation.o` - Compiled module
- `build/mpc_designation.mod` - Module interface file
- `build/mpc_designation_cli` - Command-line tool

## Usage

### Command Line

```bash
make run ARGS='1995 XA'
# Output: J95X00A

make run ARGS='-v J95X00A'
# Verbose output with format info

# Or directly:
./build/mpc_designation_cli '1995 XA'
```

### Library Usage

```fortran
program example
    use mpc_designation
    implicit none

    character(len=80) :: result
    type(conversion_result) :: res
    type(format_info) :: info

    ! Simple conversion
    result = convert_simple('1995 XA')
    print *, 'Packed: ', trim(result)  ! J95X00A

    result = convert_simple('J95X00A')
    print *, 'Unpacked: ', trim(result)  ! 1995 XA

    ! Full conversion with info
    res = convert('1995 XA')
    if (.not. MPCDesignationError) then
        print *, 'Input:  ', trim(res%input)
        print *, 'Output: ', trim(res%output)
        print *, 'Format: ', trim(res%info%format)
        print *, 'Type:   ', trim(res%info%dtype)
    end if

    ! Ensure specific format
    result = pack_designation('1995 XA')      ! J95X00A
    result = unpack_designation('J95X00A')    ! 1995 XA

    ! Format detection
    info = detect_format('1995 XA')
    print *, 'Format: ', trim(info%format)    ! unpacked
    print *, 'Type:   ', trim(info%dtype)     ! provisional

    ! Validation
    if (is_valid_designation('1995 XA')) then
        print *, 'Valid designation'
    end if

    ! Error handling
    result = convert_simple('invalid')
    if (MPCDesignationError) then
        print *, 'Error: ', trim(error_message)
    end if

end program example
```

### Compiling Your Program

```bash
gfortran -I/path/to/build your_program.f90 /path/to/build/mpc_designation.o -o your_program
```

## API Reference

### Module: mpc_designation

#### Types

```fortran
type :: format_info
    character(len=10) :: format    ! 'packed' or 'unpacked'
    character(len=25) :: dtype     ! designation type
    character(len=60) :: subtype   ! detailed description
end type

type :: conversion_result
    character(len=80) :: input
    character(len=80) :: output
    type(format_info) :: info
end type
```

#### Functions

| Function | Description |
|----------|-------------|
| `convert(designation)` | Full conversion with result info |
| `convert_simple(designation)` | Convert and return just the output |
| `pack_designation(designation)` | Ensure packed format |
| `unpack_designation(designation)` | Ensure unpacked format |
| `detect_format(designation)` | Detect format without converting |
| `is_valid_designation(designation)` | Check if valid |

#### Helper Functions

| Function | Description |
|----------|-------------|
| `to_report_format(packed)` | Convert packed designation to 12-character MPC report format |
| `from_report_format(report)` | Convert 12-character MPC report format to minimal packed format |
| `has_fragment(designation)` | Check if designation has a comet fragment suffix |
| `get_fragment(designation)` | Extract fragment suffix (returns uppercase, e.g., "A", "AA") |
| `get_parent(designation)` | Get parent comet without fragment suffix |
| `designations_equal(d1, d2)` | Check if two designations refer to the same object |

```fortran
! Helper function examples
character(len=12) :: report
character(len=80) :: minimal, parent
character(len=10) :: frag
logical :: has_frag, same_obj

! Convert to/from 12-character MPC report format
report = to_report_format('0073Pa')      ! '0073P      a'
minimal = from_report_format('0073P      a')  ! '0073Pa'

! Fragment handling
has_frag = has_fragment('73P-A')         ! .true.
frag = get_fragment('73P-A')             ! 'A'
parent = get_parent('73P-A')             ! '73P'

! Compare designations across formats
same_obj = designations_equal('73P-A', '0073Pa')    ! .true. (same object)
same_obj = designations_equal('73P-A', '73P-B')     ! .false. (different)
```

#### Error Handling

```fortran
! After any function call, check:
if (MPCDesignationError) then
    print *, 'Error: ', trim(error_message)
end if

! Clear error state:
call clear_error()
```

#### Constants

```fortran
character(len=*), parameter :: VERSION = '1.0.0'
integer, parameter :: MAX_ASTEROID_NUMBER = 15396335
```

## Testing

```bash
# Run all tests
make test-all

# Run specific test suites
make test-errors    # Error handling tests (94 cases)
make test-helpers   # Helper function tests (77 cases)
make test-csv       # Conversion tests (2M+ cases)
make test-roundtrip # Roundtrip verification
```

## Examples

```fortran
! Asteroids - permanent (numbered)
convert_simple('1')           ! '00001'
convert_simple('100001')      ! 'A0001'
convert_simple('620000')      ! '~0000'

! Asteroids - provisional
convert_simple('1995 XA')     ! 'J95X00A'
convert_simple('2024 AB631')  ! '_OA004S'

! Asteroids - survey
convert_simple('2040 P-L')    ! 'PLS2040'

! Comets
convert_simple('1P')          ! '0001P'
convert_simple('C/1995 O1')   ! 'CJ95O010'

! Natural satellites
convert_simple('S/2019 S 22') ! 'SK19S220'
```

## License

CC0 1.0 Universal - Public Domain Dedication
