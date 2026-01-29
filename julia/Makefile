# Makefile for MPC Designation Converter (Julia implementation)

JULIA = julia

TEST_DATA = ../test-data

.PHONY: all clean test test-csv test-errors test-roundtrip test-all run

# Default: nothing to build for Julia
all:
	@echo "Julia is interpreted - no build step required"

# Run the CLI (for manual testing)
run:
	$(JULIA) src/mpc_designation_cli.jl $(ARGS)

# Run conversion tests
test: test-csv

test-csv:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(JULIA) test/test_csv.jl $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors:
	$(JULIA) test/test_errors.jl $(TEST_DATA)/error_test_cases.csv

# Run round-trip tests
test-roundtrip:
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(JULIA) test/test_roundtrip.jl $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test-errors test-csv

# Clean (nothing to clean for Julia)
clean:
	@echo "Nothing to clean for Julia"

# Help
help:
	@echo "MPC Designation Converter (Julia) - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          No build step required (Julia is interpreted)"
	@echo "  test         Run conversion tests"
	@echo "  test-errors  Run error handling tests"
	@echo "  test-roundtrip Run round-trip tests"
	@echo "  test-all     Run all tests"
	@echo "  run ARGS=... Run CLI with arguments"
	@echo ""
	@echo "Examples:"
	@echo "  make run ARGS='1995 XA'"
	@echo "  make run ARGS='-v J95X00A'"
