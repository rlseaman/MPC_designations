# Makefile for MPC Designation Converter (Rust implementation)

CARGO = cargo
CARGO_FLAGS = --release

TEST_DATA = ../test-data

.PHONY: all clean build test test-csv test-roundtrip test-all

# Default: build all
all: build

# Build release binaries
build:
	$(CARGO) build $(CARGO_FLAGS)

# Build debug binaries
debug:
	$(CARGO) build

# Run conversion tests
test: test-csv

test-csv: build
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	./target/release/test_csv $(TEST_DATA)/prov_unpack_to_pack.csv

# Run bidirectional and round-trip tests
test-roundtrip: build
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	./target/release/test_roundtrip $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test test-roundtrip

# Run cargo tests (unit tests)
test-unit:
	$(CARGO) test

# Clean build artifacts
clean:
	$(CARGO) clean

# Format code
fmt:
	$(CARGO) fmt

# Run clippy linter
clippy:
	$(CARGO) clippy
