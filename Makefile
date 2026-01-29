# Root Makefile for MPC Designation Converter
#
# Orchestrates builds and tests across all language implementations

.PHONY: all clean test test-c test-python test-tcl test-swift test-perl test-go test-java test-julia test-js test-ruby test-rust test-errors test-all validate version

# Default: build all
all: build-c build-swift build-go build-java build-rust

# Build targets
build-c:
	$(MAKE) -C c

build-swift:
	$(MAKE) -C swift

build-go:
	$(MAKE) -C go

build-java:
	@if command -v javac >/dev/null 2>&1; then \
		$(MAKE) -C java; \
	else \
		echo "Java not installed, skipping Java build"; \
	fi

build-rust:
	@if command -v cargo >/dev/null 2>&1; then \
		$(MAKE) -C rust; \
	else \
		echo "Rust not installed, skipping Rust build"; \
	fi

# Clean all build artifacts
clean:
	$(MAKE) -C c clean
	$(MAKE) -C swift clean
	$(MAKE) -C go clean
	@if command -v javac >/dev/null 2>&1; then $(MAKE) -C java clean; fi
	@if command -v cargo >/dev/null 2>&1; then $(MAKE) -C rust clean; fi
	rm -rf python/src/mpc_designation/__pycache__
	rm -rf python/test/__pycache__
	rm -f test-data/prov_unpack_to_pack.csv

# Decompress test data if needed
test-data/prov_unpack_to_pack.csv: test-data/prov_unpack_to_pack.csv.gz
	gunzip -k $<

# Run all tests for all languages
test-all: test-c test-python test-tcl test-swift test-perl test-go test-java test-julia test-js test-ruby test-rust
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

# Go tests
test-go: build-go test-data/prov_unpack_to_pack.csv
	@echo "=== Go Tests ==="
	$(MAKE) -C go test-all

# Java tests
test-java: test-data/prov_unpack_to_pack.csv
	@echo "=== Java Tests ==="
	@if command -v javac >/dev/null 2>&1; then \
		$(MAKE) -C java test-all; \
	else \
		echo "Java not installed, skipping Java tests"; \
	fi

# Julia tests
test-julia: test-data/prov_unpack_to_pack.csv
	@echo "=== Julia Tests ==="
	@if command -v julia >/dev/null 2>&1; then \
		$(MAKE) -C julia test-all; \
	else \
		echo "Julia not installed, skipping Julia tests"; \
	fi

# JavaScript tests
test-js: test-data/prov_unpack_to_pack.csv
	@echo "=== JavaScript Tests ==="
	@if command -v node >/dev/null 2>&1; then \
		cd js && node test/test_csv.js ../test-data/prov_unpack_to_pack.csv; \
	else \
		echo "Node.js not installed, skipping JavaScript tests"; \
	fi

# Ruby tests
test-ruby: test-data/prov_unpack_to_pack.csv
	@echo "=== Ruby Tests ==="
	@if command -v ruby >/dev/null 2>&1; then \
		$(MAKE) -C ruby test; \
	else \
		echo "Ruby not installed, skipping Ruby tests"; \
	fi

# Rust tests
test-rust: test-data/prov_unpack_to_pack.csv
	@echo "=== Rust Tests ==="
	@if command -v cargo >/dev/null 2>&1; then \
		$(MAKE) -C rust test; \
	else \
		echo "Rust not installed, skipping Rust tests"; \
	fi

# Error tests only (quick validation)
test-errors: build-c build-swift build-go
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
	@echo ""
	@echo "--- Go ---"
	$(MAKE) -C go test-errors

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
	@echo "  test-go      Run Go tests only"
	@echo "  test-java    Run Java tests only"
	@echo "  test-julia   Run Julia tests only"
	@echo "  test-js      Run JavaScript tests only"
	@echo "  test-ruby    Run Ruby tests only"
	@echo "  test-rust    Run Rust tests only"
	@echo "  test-errors  Run error tests only (quick)"
	@echo "  validate     Cross-language consistency check"
	@echo "  version      Show current version"
	@echo "  help         Show this help"
