# Swift Makefile for MPC Designation Converter

SWIFTC = swiftc
SWIFT = swift
SWIFTFLAGS = -O -whole-module-optimization

SRCDIR = src
TESTDIR = test
BINDIR = .

# Read version from root VERSION file
VERSION := $(shell cat ../VERSION 2>/dev/null || echo "1.0.0")

.PHONY: all clean test test-csv test-errors

# Default: build the CLI tool
all: mpc_designation

# Build the CLI executable
mpc_designation: $(SRCDIR)/MPCDesignation.swift $(SRCDIR)/main.swift
	$(SWIFTC) $(SWIFTFLAGS) -o $@ $^

# Build test executables
# Swift requires the main entry file to be named main.swift, so we copy it
test_csv: $(SRCDIR)/MPCDesignation.swift $(TESTDIR)/test_csv.swift $(TESTDIR)/test_csv_main.swift
	@mkdir -p .build/test_csv
	@cp $(SRCDIR)/MPCDesignation.swift .build/test_csv/
	@cp $(TESTDIR)/test_csv.swift .build/test_csv/
	@cp $(TESTDIR)/test_csv_main.swift .build/test_csv/main.swift
	$(SWIFTC) $(SWIFTFLAGS) -o $@ .build/test_csv/*.swift

test_errors: $(SRCDIR)/MPCDesignation.swift $(TESTDIR)/test_errors.swift $(TESTDIR)/test_errors_main.swift
	@mkdir -p .build/test_errors
	@cp $(SRCDIR)/MPCDesignation.swift .build/test_errors/
	@cp $(TESTDIR)/test_errors.swift .build/test_errors/
	@cp $(TESTDIR)/test_errors_main.swift .build/test_errors/main.swift
	$(SWIFTC) $(SWIFTFLAGS) -o $@ .build/test_errors/*.swift

# Run CSV conversion tests
test-csv: test_csv ../test-data/prov_unpack_to_pack.csv
	./test_csv ../test-data/prov_unpack_to_pack.csv

# Run error handling tests
test-errors: test_errors ../test-data/error_test_cases.csv
	./test_errors ../test-data/error_test_cases.csv

# Run all tests
test-all: test-errors test-csv

# Alias
test: test-all

# Clean build artifacts
clean:
	rm -f mpc_designation test_csv test_errors
	rm -rf *.dSYM .build

# Run without building (interpreted mode)
run:
	$(SWIFT) $(SRCDIR)/MPCDesignation.swift $(ARGS)
