# Contributing to MPC Designations

Thank you for your interest in contributing to MPC Designations!

## Adding a New Language Implementation

We welcome implementations in new programming languages. Here's how to add one:

### 1. Create the Directory Structure

```
your-language/
├── README.md           # Language-specific documentation
├── src/                # Source code
│   └── mpc_designation.*
├── test/               # Test files
│   ├── test_csv.*      # Conversion test runner
│   └── test_errors.*   # Error handling test runner
└── examples/           # Usage examples
    └── example_usage.*
```

### 2. Implement Core Functions

Your implementation must provide these functions:

#### `convert_simple(designation) -> string`
- Takes a designation string (packed or unpacked)
- Returns the converted designation (unpacked or packed)
- Throws/raises an error for invalid input

#### `convert(designation) -> info_dict`
- Takes a designation string
- Returns a dictionary/struct with:
  - `input`: Original input
  - `output`: Converted designation
  - `format`: "packed" or "unpacked"
  - `subtype`: Description (e.g., "provisional asteroid")

### 3. Pass All Tests

Your implementation must pass:

1. **Conversion tests** (`test_csv`): All 2,021,090 test cases in `test-data/prov_unpack_to_pack.csv.gz`
2. **Error tests** (`test_errors`): All error cases in `test-data/error_test_cases.csv`

### 4. Document Your Implementation

Create a `README.md` in your language directory with:

1. **Quick Start** - Minimal steps to use
2. **Installation** - How to set up
3. **CLI Usage** - Command-line examples
4. **Library/API Usage** - Integration examples
5. **Building** - Compilation/setup steps
6. **Testing** - How to run tests

### 5. Add CI/CD

Update `.github/workflows/test.yml` to include your language:

```yaml
test-your-language:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - name: Set up your-language
      # Add setup steps
    - name: Test your-language
      run: |
        cd your-language
        # Run your tests
```

## Reporting Issues

When reporting bugs, please include:

1. The input designation that caused the problem
2. Expected output
3. Actual output or error message
4. Which language implementation

## Code Style

- Follow the conventions of each language
- Keep code readable and well-commented
- Match the structure of existing implementations
- Avoid dependencies where possible

## Testing Locally

Before submitting, run all tests:

```bash
# C
cd c && make test-all

# Python
cd python && python test/test_errors.py ../test-data/error_test_cases.csv

# TCL
cd tcl && tclsh test/test_errors.tcl ../test-data/error_test_cases.csv
```

## Questions?

Open an issue for questions about contributing.
