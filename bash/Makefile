# Makefile for MPC Designation Converter (Bash)

.PHONY: all clean test test-errors test-helpers test-csv test-roundtrip test-all run help

all:
	@echo "Bash scripts need no compilation."
	@echo "Run with: ./src/mpc_designation.sh <designation>"

# Run error handling tests
test-errors:
	@echo "Running error tests..."
	./test/test_errors.sh

# Run helper function tests
test-helpers:
	@echo "Running helper function tests..."
	./test/test_helpers.sh

# Run CSV test suite
test-csv:
	@echo "Running CSV test suite..."
	./test/test_csv.sh

# Run round-trip tests
test-roundtrip:
	@echo "Running round-trip tests..."
	./test/test_roundtrip.sh

# Run all tests
test-all: test-errors test-helpers test-csv
	@echo "All tests passed."

# Legacy test target
test: test-errors test-helpers
	@echo "Tests passed."

run:
	./src/mpc_designation.sh $(ARGS)

help:
	@echo "Available targets:"
	@echo "  make test-errors   - Run error handling tests"
	@echo "  make test-helpers  - Run helper function tests (77 cases)"
	@echo "  make test-csv      - Run full CSV test suite (2M+ tests)"
	@echo "  make test-roundtrip - Run round-trip tests"
	@echo "  make test-all      - Run all tests"
	@echo "  make test          - Run error and helper tests"
	@echo "  make run ARGS='...'- Run with arguments"
	@echo ""
	@echo "CLI usage:"
	@echo "  ./src/mpc_designation.sh '1995 XA'"
	@echo "  ./src/mpc_designation.sh J95X00A"

clean:
	@echo "Nothing to clean for Bash."
