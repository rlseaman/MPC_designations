# Makefile for MPC Designation Converter (Haskell implementation)

GHC = ghc
GHC_FLAGS = -O2 -isrc

BUILD = build
TEST_DATA = ../test-data

.PHONY: all clean test test-csv test-errors test-helpers test-roundtrip test-all build

# Default: build all
all: build

# Build the CLI and test executables
build: $(BUILD)/mpc_designation $(BUILD)/test_csv $(BUILD)/test_errors $(BUILD)/test_helpers $(BUILD)/test_roundtrip

$(BUILD)/.dir:
	mkdir -p $(BUILD)
	touch $(BUILD)/.dir

$(BUILD)/mpc_designation: src/MPCDesignation.hs src/Main.hs $(BUILD)/.dir
	$(GHC) $(GHC_FLAGS) -o $@ src/Main.hs

$(BUILD)/test_csv: src/MPCDesignation.hs test/TestCsv.hs $(BUILD)/.dir
	$(GHC) $(GHC_FLAGS) -itest -o $@ test/TestCsv.hs

$(BUILD)/test_errors: src/MPCDesignation.hs test/TestErrors.hs $(BUILD)/.dir
	$(GHC) $(GHC_FLAGS) -itest -o $@ test/TestErrors.hs

$(BUILD)/test_helpers: src/MPCDesignation.hs test/TestHelpers.hs $(BUILD)/.dir
	$(GHC) $(GHC_FLAGS) -itest -o $@ test/TestHelpers.hs

$(BUILD)/test_roundtrip: src/MPCDesignation.hs test/TestRoundtrip.hs $(BUILD)/.dir
	$(GHC) $(GHC_FLAGS) -itest -o $@ test/TestRoundtrip.hs

# Run conversion tests
test: test-csv

test-csv: $(BUILD)/test_csv
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(BUILD)/test_csv $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors: $(BUILD)/test_errors
	$(BUILD)/test_errors $(TEST_DATA)/error_test_cases.csv

# Run helper function tests
test-helpers: $(BUILD)/test_helpers
	$(BUILD)/test_helpers

# Run round-trip tests
test-roundtrip: $(BUILD)/test_roundtrip
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(BUILD)/test_roundtrip $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test-errors test-helpers test-csv test-roundtrip

# Clean build artifacts
clean:
	rm -rf $(BUILD)
	rm -f src/*.hi src/*.o test/*.hi test/*.o

# Help
help:
	@echo "MPC Designation Converter (Haskell) - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all            Build the CLI (default)"
	@echo "  build          Compile all executables"
	@echo "  clean          Remove all build artifacts"
	@echo "  test           Run conversion tests"
	@echo "  test-errors    Run error handling tests (94 cases)"
	@echo "  test-helpers   Run helper function tests (77 cases)"
	@echo "  test-csv       Run CSV conversion tests (2M+ entries)"
	@echo "  test-roundtrip Run round-trip tests"
	@echo "  test-all       Run all tests"
	@echo ""
	@echo "Examples:"
	@echo "  ./build/mpc_designation '1995 XA'"
	@echo "  ./build/mpc_designation -v J95X00A"
