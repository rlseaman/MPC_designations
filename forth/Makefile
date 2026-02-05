# Makefile for MPC Designation Converter (Forth)

GFORTH = gforth
CSV_FILE = ../test-data/prov_unpack_to_pack.csv

.PHONY: all clean test test-quick test-errors test-helpers test-csv test-roundtrip test-all run help

all:
	@echo "Forth is interpreted - no compilation needed."
	@echo "Run with: $(GFORTH) src/mpc_designation_cli.fs <designation>"

# Run error handling tests
test-errors:
	@echo "Running error tests..."
	$(GFORTH) test/test_errors.fs

# Run helper function tests
test-helpers:
	@echo "Running helper function tests..."
	$(GFORTH) test/test_helpers.fs

# Run CSV test suite
test-csv: $(CSV_FILE)
	@echo "Running CSV test suite..."
	$(GFORTH) test/test_csv.fs $(CSV_FILE)

# Run round-trip tests (alias for test-csv)
test-roundtrip: test-csv

# Run all tests
test-all: test-errors test-helpers test-csv
	@echo "All tests passed."

# Legacy test target
test: test-errors test-helpers
	@echo "Tests passed."

test-quick: test-errors

$(CSV_FILE): $(CSV_FILE).gz
	gunzip -k $<

run:
	$(GFORTH) src/mpc_designation_cli.fs $(ARGS)

help:
	@echo "Available targets:"
	@echo "  make test-errors   - Run error handling tests (86 cases)"
	@echo "  make test-helpers  - Run helper function tests (77 cases)"
	@echo "  make test-csv      - Run full CSV test suite (2M+ tests)"
	@echo "  make test-roundtrip - Run round-trip tests"
	@echo "  make test-all      - Run all tests"
	@echo "  make test          - Run error and helper tests"
	@echo "  make run ARGS='...'- Run with arguments"
	@echo ""
	@echo "CLI usage:"
	@echo "  $(GFORTH) src/mpc_designation_cli.fs '1995 XA'"
	@echo "  $(GFORTH) src/mpc_designation_cli.fs J95X00A"

clean:
	@echo "Nothing to clean for Forth."
