# MPC Designation Converter - C Implementation

C99 library and CLI tool for converting between packed and unpacked MPC designations.

## Quick Start

```bash
# Build
make

# Convert a designation
./mpc_designation '1995 XA'    # Output: J95X00A
./mpc_designation J95X00A      # Output: 1995 XA
```

## Installation

### Using Make (Unix/macOS/Linux)

```bash
cd c
make
# Optionally install to /usr/local/bin:
sudo cp mpc_designation /usr/local/bin/
```

### Using CMake (Cross-platform, including Windows)

```bash
cd c
mkdir build && cd build
cmake ..
cmake --build .

# For Release build:
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .

# Run tests:
ctest

# Install (Unix):
sudo cmake --install .
```

On Windows with Visual Studio:
```cmd
cd c
mkdir build && cd build
cmake ..
cmake --build . --config Release
```

## CLI Usage

```bash
# Single conversion
./mpc_designation '1995 XA'
# Output: J95X00A

# Multiple designations
./mpc_designation 1 '1995 XA' 'C/1995 O1'
# Output:
# 1 -> 00001
# 1995 XA -> J95X00A
# C/1995 O1 -> CJ95O010

# Verbose mode
./mpc_designation -v '1995 XA'
# Output:
#   Input:    1995 XA
#   Detected: unpacked format, provisional
#   Action:   packing to MPC compact form
#   Output:   J95X00A
```

## Library Usage

### Basic Integration

```c
#include "mpc_designation.h"

int main() {
    char output[MPC_MAX_UNPACKED];

    // Simple conversion (returns error code)
    int err = mpc_convert_simple("1995 XA", output, sizeof(output));
    if (err == MPC_OK) {
        printf("Result: %s\n", output);  // J95X00A
    } else {
        printf("Error: %s\n", mpc_strerror(err));
    }

    return 0;
}
```

### With Format Information

```c
#include "mpc_designation.h"

int main() {
    char output[MPC_MAX_UNPACKED];
    mpc_info_t info;

    int err = mpc_convert("1995 XA", output, sizeof(output), &info);
    if (err == MPC_OK) {
        printf("Input:  %s\n", info.input);
        printf("Output: %s\n", output);
        printf("Format: %s\n", info.format == MPC_FORMAT_PACKED ? "packed" : "unpacked");
        printf("Type:   %s\n", info.subtype);
    }

    return 0;
}
```

### Compiling Your Code

**Direct compilation:**
```bash
cc -O2 -I/path/to/mpc_designation your_code.c /path/to/mpc_designation.c -o your_program
```

**Using CMake** (add to your CMakeLists.txt):
```cmake
add_subdirectory(path/to/mpc_designation/c)
target_link_libraries(your_target PRIVATE mpc_designation_lib)
```

Or simply copy `mpc_designation.c` and `mpc_designation.h` into your project.

## API Reference

### Constants

```c
#define MPC_MAX_PACKED    16   // Max packed designation length
#define MPC_MAX_UNPACKED  32   // Max unpacked designation length

// Error codes
#define MPC_OK            0    // Success
#define MPC_ERR_NULL     -1    // Null pointer argument
#define MPC_ERR_FORMAT   -2    // Invalid format
#define MPC_ERR_RANGE    -3    // Number out of range
#define MPC_ERR_BUFFER   -4    // Buffer too small

// Format types
#define MPC_FORMAT_PACKED   1
#define MPC_FORMAT_UNPACKED 2
```

### Functions

```c
// Simple conversion
int mpc_convert_simple(const char *input, char *output, size_t output_size);

// Conversion with format information
int mpc_convert(const char *input, char *output, size_t output_size, mpc_info_t *info);

// Error message
const char *mpc_strerror(int err);
```

### Info Structure

```c
typedef struct {
    int format;          // MPC_FORMAT_PACKED or MPC_FORMAT_UNPACKED
    const char *subtype; // Human-readable description
    char input[32];      // Copy of input
} mpc_info_t;
```

## Building

```bash
# Build all (CLI + tests)
make

# Build just the CLI
make mpc_designation

# Run conversion tests
make test

# Run error handling tests
make test-errors

# Run all tests
make test-all

# Clean build artifacts
make clean
```

## Testing

```bash
# Conversion tests (2M+ test cases)
make test

# Error handling tests
make test-errors

# Both
make test-all
```

## Error Handling

The library validates all inputs and returns appropriate error codes:

- `MPC_ERR_FORMAT`: Invalid designation format
- `MPC_ERR_RANGE`: Number out of valid range
- `MPC_ERR_NULL`: Null pointer passed
- `MPC_ERR_BUFFER`: Output buffer too small

Use `mpc_strerror()` to get human-readable error messages.

## Examples

See `examples/example_usage.c` for more detailed usage examples.
