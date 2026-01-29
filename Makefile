# Root Makefile for MPC Designation Converter
#
# Orchestrates builds and tests across all language implementations

.PHONY: all clean test test-c test-python test-tcl test-swift test-perl test-errors test-all validate version

# Default: build all
all: build-c build-swift

# Build targets
build-c:
	$(MAKE) -C c

build-swift:
	$(MAKE) -C swift

# Clean all build artifacts
clean:
	$(MAKE) -C c clean
	$(MAKE) -C swift clean
	rm -rf python/src/mpc_designation/__pycache__
	rm -rf python/test/__pycache__
	rm -f test-data/prov_unpack_to_pack.csv

# Decompress test data if needed
test-data/prov_unpack_to_pack.csv: test-data/prov_unpack_to_pack.csv.gz
	gunzip -k $<

# Run all tests for all languages
test-all: test-c test-python test-tcl test-swift test-perl
	@echo ""
	@echo "=== All Tests Complete ==="

# C tests
test-c: build-c
	@echo "=== C Tests ==="
	$(MAKE) -C c test-all

# Python tests
test-python: test-data/prov_unpack_to_pack.csv
	@echo "=== Python Tests ==="
	@echo "--- Python Error Tests ---"
	cd python && python3 test/test_errors.py ../test-data/error_test_cases.csv
	@echo ""
	@echo "--- Python Conversion Tests ---"
	cd python && python3 test/test_csv.py ../test-data/prov_unpack_to_pack.csv

# TCL tests
test-tcl: test-data/prov_unpack_to_pack.csv
	@echo "=== TCL Tests ==="
	@echo "--- TCL Error Tests ---"
	cd tcl && tclsh test/test_errors.tcl ../test-data/error_test_cases.csv
	@echo ""
	@echo "--- TCL Conversion Tests ---"
	cd tcl && tclsh test/test_csv.tcl ../test-data/prov_unpack_to_pack.csv

# Swift tests
test-swift: build-swift test-data/prov_unpack_to_pack.csv
	@echo "=== Swift Tests ==="
	$(MAKE) -C swift test-all

# Perl tests
test-perl: test-data/prov_unpack_to_pack.csv
	@echo "=== Perl Tests ==="
	@echo "--- Perl Error Tests ---"
	cd perl && perl test/test_errors.pl ../test-data/error_test_cases.csv
	@echo ""
	@echo "--- Perl Conversion Tests ---"
	cd perl && perl test/test_csv.pl ../test-data/prov_unpack_to_pack.csv

# Error tests only (quick validation)
test-errors: build-c build-swift
	@echo "=== Error Tests (All Languages) ==="
	@echo ""
	@echo "--- C ---"
	$(MAKE) -C c test-errors
	@echo ""
	@echo "--- Python ---"
	cd python && python3 test/test_errors.py ../test-data/error_test_cases.csv
	@echo ""
	@echo "--- TCL ---"
	cd tcl && tclsh test/test_errors.tcl ../test-data/error_test_cases.csv
	@echo ""
	@echo "--- Swift ---"
	$(MAKE) -C swift test-errors
	@echo ""
	@echo "--- Perl ---"
	cd perl && perl test/test_errors.pl ../test-data/error_test_cases.csv

# Cross-language validation
validate: build-c
	@echo "=== Cross-Language Validation ==="
	./scripts/validate_consistency.sh

# Show version
version:
	@cat VERSION

# Help
help:
	@echo "MPC Designation Converter - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          Build all implementations (default)"
	@echo "  clean        Remove all build artifacts"
	@echo "  test-all     Run all tests for all languages"
	@echo "  test-c       Run C tests only"
	@echo "  test-python  Run Python tests only"
	@echo "  test-tcl     Run TCL tests only"
	@echo "  test-swift   Run Swift tests only"
	@echo "  test-perl    Run Perl tests only"
	@echo "  test-errors  Run error tests only (quick)"
	@echo "  validate     Cross-language consistency check"
	@echo "  version      Show current version"
	@echo "  help         Show this help"
