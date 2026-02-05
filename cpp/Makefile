# C++ MPC Designation Converter Makefile

CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2
DEBUG_FLAGS = -g -O0

# Source files
SRC_DIR = src
TEST_DIR = test
LIB_SRC = $(SRC_DIR)/mpc_designation.cpp
CLI_SRC = $(SRC_DIR)/mpc_designation_cli.cpp

# Targets
CLI = mpc_designation
TEST_CSV = test_csv
TEST_ERRORS = test_errors
TEST_ROUNDTRIP = test_roundtrip
TEST_HELPERS = test_helpers

# Test data paths
TEST_DATA_DIR = ../test-data
CSV_FILE = $(TEST_DATA_DIR)/prov_unpack_to_pack.csv
ERROR_FILE = $(TEST_DATA_DIR)/error_test_cases.csv

.PHONY: all clean test test-csv test-errors test-roundtrip test-helpers test-all help

# Default: build CLI
all: $(CLI)

# Build CLI
$(CLI): $(LIB_SRC) $(CLI_SRC)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Build test executables
$(TEST_CSV): $(LIB_SRC) $(TEST_DIR)/test_csv.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

$(TEST_ERRORS): $(LIB_SRC) $(TEST_DIR)/test_errors.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

$(TEST_ROUNDTRIP): $(LIB_SRC) $(TEST_DIR)/test_roundtrip.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

$(TEST_HELPERS): $(LIB_SRC) $(TEST_DIR)/test_helpers.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

# Ensure test data is decompressed
$(CSV_FILE): $(CSV_FILE).gz
	gunzip -k $<

# Run all tests
test: test-errors test-csv
	@echo ""
	@echo "=== All C++ Tests Complete ==="

test-all: test-errors test-helpers test-csv test-roundtrip
	@echo ""
	@echo "=== All C++ Tests Complete ==="

# Run CSV conversion tests
test-csv: $(TEST_CSV) $(CSV_FILE)
	@echo "--- C++ CSV Tests ---"
	./$(TEST_CSV) $(CSV_FILE)

# Run error handling tests
test-errors: $(TEST_ERRORS)
	@echo "--- C++ Error Tests ---"
	./$(TEST_ERRORS) $(ERROR_FILE)

# Run roundtrip tests
test-roundtrip: $(TEST_ROUNDTRIP) $(CSV_FILE)
	@echo "--- C++ Roundtrip Tests ---"
	./$(TEST_ROUNDTRIP) $(CSV_FILE)

# Run helper function tests
test-helpers: $(TEST_HELPERS)
	@echo "--- C++ Helper Tests ---"
	./$(TEST_HELPERS)

# Clean
clean:
	rm -f $(CLI) $(TEST_CSV) $(TEST_ERRORS) $(TEST_ROUNDTRIP) $(TEST_HELPERS)

# Help
help:
	@echo "C++ MPC Designation Converter"
	@echo ""
	@echo "Targets:"
	@echo "  all           Build CLI tool (default)"
	@echo "  test          Run error and CSV tests"
	@echo "  test-all      Run all tests including roundtrip and helpers"
	@echo "  test-csv      Run CSV conversion tests (2M+ entries)"
	@echo "  test-errors   Run error handling tests"
	@echo "  test-helpers  Run helper function tests (77 cases)"
	@echo "  test-roundtrip Run roundtrip tests"
	@echo "  clean         Remove built files"
	@echo "  help          Show this help"
