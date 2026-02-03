# Makefile for Octave/MATLAB MPC Designation converter

.PHONY: all test test-quick test-csv test-errors clean help

all: test-quick

# Run quick tests
test-quick:
	@echo "Running quick tests..."
	@octave --no-gui --quiet test/test_quick.m

# Run CSV test suite
test-csv:
	@echo "Running CSV test suite..."
	@octave --no-gui --quiet test/test_csv.m

# Run error handling tests
test-errors:
	@echo "Running error handling tests..."
	@octave --no-gui --quiet test/test_errors.m

# Run all tests
test: test-quick test-errors test-csv

# Clean generated files
clean:
	@rm -f *.oct

# Show help
help:
	@echo "Available targets:"
	@echo "  make test-quick  - Run quick validation tests"
	@echo "  make test-csv    - Run full CSV test suite (2M+ tests)"
	@echo "  make test-errors - Run error handling tests"
	@echo "  make test        - Run all tests"
	@echo "  make clean       - Remove generated files"
	@echo ""
	@echo "CLI usage:"
	@echo "  octave --no-gui src/mpc_designation_cli.m '1995 XA'"
	@echo "  octave --no-gui src/mpc_designation_cli.m J95X00A"
