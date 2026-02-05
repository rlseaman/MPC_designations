# PHP MPC Designation Converter Makefile

.PHONY: test test-csv test-errors test-helpers test-roundtrip test-all clean help

# Test data paths
TEST_DATA_DIR := ../test-data
CSV_FILE := $(TEST_DATA_DIR)/prov_unpack_to_pack.csv
ERROR_FILE := $(TEST_DATA_DIR)/error_test_cases.csv

# Ensure test data is decompressed
$(CSV_FILE): $(CSV_FILE).gz
	gunzip -k $<

# Run all tests
test: test-errors test-csv
	@echo ""
	@echo "=== All PHP Tests Complete ==="

test-all: test-errors test-helpers test-csv test-roundtrip
	@echo ""
	@echo "=== All PHP Tests Complete ==="

# Run CSV conversion tests
test-csv: $(CSV_FILE)
	@echo "--- PHP CSV Tests ---"
	php test/test_csv.php $(CSV_FILE)

# Run error handling tests
test-errors:
	@echo "--- PHP Error Tests ---"
	php test/test_errors.php $(ERROR_FILE)

# Run helper function tests
test-helpers:
	@echo "--- PHP Helper Tests ---"
	php test/test_helpers.php

# Run roundtrip tests
test-roundtrip: $(CSV_FILE)
	@echo "--- PHP Roundtrip Tests ---"
	php test/test_roundtrip.php $(CSV_FILE)

# Clean (nothing to clean for PHP)
clean:
	@echo "Nothing to clean for PHP"

# Help
help:
	@echo "PHP MPC Designation Converter"
	@echo ""
	@echo "Targets:"
	@echo "  test          Run error and CSV tests"
	@echo "  test-all      Run all tests including roundtrip"
	@echo "  test-csv      Run CSV conversion tests (2M+ entries)"
	@echo "  test-errors   Run error handling tests"
	@echo "  test-helpers  Run helper function tests"
	@echo "  test-roundtrip Run roundtrip tests"
	@echo "  help          Show this help"
