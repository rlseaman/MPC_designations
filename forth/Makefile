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
# Note: gforth 0.7.3 has an intermittent "Address alignment exception" bug that
# occurs during code loading on Apple Silicon. The bug doesn't affect test
# correctness (all conversions work correctly) but can crash before completion.
# We retry up to 5 times to work around this. See src/GFORTH_NOTES.txt for details.
test-csv: $(CSV_FILE)
	@echo "Running CSV test suite..."
	@for attempt in 1 2 3 4 5; do \
		output=$$($(GFORTH) test/test_csv.fs $(CSV_FILE) 2>&1); \
		echo "$$output"; \
		if echo "$$output" | grep -q "All tests passed!"; then \
			exit 0; \
		fi; \
		if [ $$attempt -lt 5 ]; then \
			echo "Retrying due to gforth intermittent issue (attempt $$attempt/5)..."; \
		fi; \
	done; \
	echo "Test failed after 5 attempts"; \
	exit 1

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
