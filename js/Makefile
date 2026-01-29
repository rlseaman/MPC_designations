# Makefile for MPC Designation Converter (JavaScript implementation)

NODE = node
TEST_DATA = ../test-data

.PHONY: all clean test test-csv test-errors test-roundtrip test-all

# Default: nothing to build (interpreted language)
all:
	@echo "JavaScript implementation ready (no build step required)"

# Run conversion tests
test: test-csv

test-csv:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(NODE) test/test_csv.js $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors:
	@echo "Error tests not yet implemented for JavaScript"

# Run bidirectional and round-trip tests
test-roundtrip:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(NODE) test/test_roundtrip.js $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test test-roundtrip

# Clean (nothing to clean for JS)
clean:
	@echo "Nothing to clean for JavaScript implementation"
