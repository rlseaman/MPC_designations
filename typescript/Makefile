# Makefile for MPC Designation Converter (TypeScript implementation)

NPM = npm
NODE = node

TEST_DATA = ../test-data

.PHONY: all clean test test-csv test-errors test-roundtrip test-helpers test-all build install

# Default: build
all: build

# Install dependencies
install:
	$(NPM) install

# Build TypeScript
build: install
	$(NPM) run build

# Run the CLI (for manual testing)
run: build
	$(NODE) dist/src/mpc_designation_cli.js $(ARGS)

# Run conversion tests
test: test-csv

test-csv: build
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(NODE) dist/test/test_csv.js $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors: build
	$(NODE) dist/test/test_errors.js $(TEST_DATA)/error_test_cases.csv

# Run round-trip tests
test-roundtrip: build
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(NODE) dist/test/test_roundtrip.js $(TEST_DATA)/prov_unpack_to_pack.csv

# Run helper function tests
test-helpers: build
	$(NODE) dist/test/test_helpers.js

# Run all tests
test-all: test-errors test-helpers test-csv test-roundtrip

# Clean build artifacts
clean:
	rm -rf dist node_modules

# Help
help:
	@echo "MPC Designation Converter (TypeScript) - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all            Build the library (default)"
	@echo "  install        Install npm dependencies"
	@echo "  build          Compile TypeScript sources"
	@echo "  clean          Remove all build artifacts"
	@echo "  test           Run conversion tests"
	@echo "  test-errors    Run error handling tests"
	@echo "  test-helpers   Run helper function tests"
	@echo "  test-roundtrip Run round-trip tests"
	@echo "  test-all       Run all tests"
	@echo "  run ARGS=...   Run CLI with arguments"
	@echo ""
	@echo "Examples:"
	@echo "  make run ARGS='1995 XA'"
	@echo "  make run ARGS='-v J95X00A'"
