# Makefile for MPC Designation Converter (Ruby implementation)

RUBY = ruby

TEST_DATA = ../test-data

.PHONY: all test test-csv test-errors test-roundtrip test-all

# Default: run tests
all: test

# Run conversion tests
test: test-csv

test-csv:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(RUBY) test/test_csv.rb $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors:
	$(RUBY) test/test_errors.rb $(TEST_DATA)/error_test_cases.csv

# Run bidirectional and round-trip tests
test-roundtrip:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(RUBY) test/test_roundtrip.rb $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test-errors test-csv test-roundtrip

# Clean (nothing to clean for Ruby)
clean:
	@echo "Nothing to clean for Ruby implementation"
