# Makefile for MPC Designation Converter (AWK)

TEST_DATA = ../test-data

.PHONY: all clean test test-errors test-helpers test-csv test-roundtrip test-all run help

all:
	@echo "AWK scripts need no compilation."
	@echo "Run with: ./src/mpc_designation_cli.sh <designation>"

test: test-errors
	@echo "Tests passed."

test-all: test-errors test-helpers test-csv test-roundtrip
	@echo ""
	@echo "=== All AWK Tests Complete ==="

test-errors:
	@echo "Running error tests..."
	./test/test_errors.sh

test-helpers:
	@echo "Running helper tests..."
	awk -f src/mpc_designation.awk -f test/test_helpers.awk

test-csv:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	@echo "Running CSV tests..."
	awk -f src/mpc_designation.awk -f test/test_csv.awk $(TEST_DATA)/prov_unpack_to_pack.csv

test-roundtrip:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	@echo "Running roundtrip tests..."
	awk -f src/mpc_designation.awk -f test/test_roundtrip.awk $(TEST_DATA)/prov_unpack_to_pack.csv

run:
	./src/mpc_designation_cli.sh $(ARGS)

help:
	./src/mpc_designation_cli.sh --help

clean:
	@echo "Nothing to clean for AWK."
