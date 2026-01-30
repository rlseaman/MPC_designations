# C# MPC Designation Converter Makefile

.PHONY: all build clean test test-csv test-errors test-roundtrip test-all help

# Test data paths
TEST_DATA_DIR := ../test-data
CSV_FILE := $(TEST_DATA_DIR)/prov_unpack_to_pack.csv
ERROR_FILE := $(TEST_DATA_DIR)/error_test_cases.csv

# Ensure test data is decompressed
$(CSV_FILE): $(CSV_FILE).gz
	gunzip -k $<

# Default: build CLI
all: build

build:
	dotnet build -c Release

# Run all tests
test: test-errors test-csv
	@echo ""
	@echo "=== All C# Tests Complete ==="

test-all: test-errors test-csv test-roundtrip
	@echo ""
	@echo "=== All C# Tests Complete ==="

# Run CSV conversion tests
test-csv: $(CSV_FILE)
	@echo "--- C# CSV Tests ---"
	dotnet run --project test/TestCsv.csproj -c Release -- $(CSV_FILE)

# Run error handling tests
test-errors:
	@echo "--- C# Error Tests ---"
	dotnet run --project test/TestErrors.csproj -c Release -- $(ERROR_FILE)

# Run roundtrip tests
test-roundtrip: $(CSV_FILE)
	@echo "--- C# Roundtrip Tests ---"
	dotnet run --project test/TestRoundtrip.csproj -c Release -- $(CSV_FILE)

# Clean
clean:
	dotnet clean
	rm -rf bin obj test/bin test/obj

# Help
help:
	@echo "C# MPC Designation Converter"
	@echo ""
	@echo "Targets:"
	@echo "  all           Build CLI tool (default)"
	@echo "  build         Build the project"
	@echo "  test          Run error and CSV tests"
	@echo "  test-all      Run all tests including roundtrip"
	@echo "  test-csv      Run CSV conversion tests (2M+ entries)"
	@echo "  test-errors   Run error handling tests"
	@echo "  test-roundtrip Run roundtrip tests"
	@echo "  clean         Remove built files"
	@echo "  help          Show this help"
