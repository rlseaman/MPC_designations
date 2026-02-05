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
#define MPC_MAX_PACKED         16   // Max packed designation length
#define MPC_MAX_UNPACKED       32   // Max unpacked designation length
#define MPC_REPORT_FORMAT_SIZE 13   // 12-char MPC report format + null

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

// Format conversion (minimal <-> 12-char MPC report format)
int mpc_to_report_format(const char *minimal, char *report, size_t outlen);
int mpc_from_report_format(const char *report, char *minimal, size_t outlen);

// Fragment helpers
int mpc_has_fragment(const char *desig);
int mpc_get_fragment(const char *desig, char *fragment, size_t outlen);
int mpc_get_parent(const char *desig, char *parent, size_t outlen);

// Comparison
int mpc_designations_equal(const char *desig1, const char *desig2);

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

# Fragment handling tests
make test-fragments

# Helper function tests
make test-helpers

# All tests
make test-all
```

## Special Format Handling

### Comet Fragments

Numbered comets with fragments (e.g., 73P-A, 73P-AA) pack fragment letters as lowercase:

```bash
./mpc_designation '73P-A'     # Output: 0073Pa
./mpc_designation '73P-AA'    # Output: 0073Paa
./mpc_designation 0073Pa      # Output: 73P-A
```

Provisional comets with fragments use a 9-character format:

```bash
./mpc_designation 'D/1993 F2-A'   # Output: DJ93F02a
./mpc_designation 'P/1930 J1-AA'  # Output: PJ30J01aa
```

Fragment letters include I (A-Z, AA-ZZ per MPC data).

### Pre-1925 Asteroid Designations

Asteroids before 1925 use the A-prefix format (MPC-created designations):

```bash
./mpc_designation J08C00J    # Output: A908 CJ (not 1908 CJ)
./mpc_designation 'A908 CJ'  # Output: J08C00J
```

The A-prefix indicates an MPC-assigned designation for objects discovered before 1925.

### Century Code Validation

The library validates century codes:
- Asteroids: 1800-2199 (century codes I, J, K, L)
- Comets: 1000-2199 (century codes A through L)

## Helper Functions

### Format Conversion (Minimal ↔ 12-Character Report Format)

Convert between our minimal packed format and the 12-character MPC observation report format:

```c
char report[13], minimal[16];

// Minimal to 12-char report format
mpc_to_report_format("0073Pa", report, sizeof(report));
// report = "0073P      a"

// 12-char report format to minimal
mpc_from_report_format("0073P      a", minimal, sizeof(minimal));
// minimal = "0073Pa"
```

The 12-character format is used in MPC observation records (columns 1-12), where numbered comet fragments go in columns 11-12.

### Fragment Extraction

```c
char fragment[4], parent[32];

// Check if designation has a fragment
if (mpc_has_fragment("73P-A")) {  // Returns 1
    // Extract fragment (uppercase)
    mpc_get_fragment("73P-A", fragment, sizeof(fragment));  // "A"
    mpc_get_fragment("73P-AA", fragment, sizeof(fragment)); // "AA"

    // Get parent comet (without fragment)
    mpc_get_parent("73P-A", parent, sizeof(parent));   // "73P"
    mpc_get_parent("0073Pa", parent, sizeof(parent));  // "0073P"
}
```

### Designation Comparison

Compare designations across different formats:

```c
// Same object, different formats
mpc_designations_equal("1995 XA", "J95X00A");  // Returns 1
mpc_designations_equal("73P-A", "0073Pa");     // Returns 1

// Different objects
mpc_designations_equal("73P-A", "73P-B");      // Returns 0
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
