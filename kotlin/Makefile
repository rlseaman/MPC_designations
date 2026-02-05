# Makefile for MPC Designation Converter (Kotlin implementation)

KOTLINC = kotlinc
KOTLIN = kotlin
JAVA = java

# Compiler flags
KFLAGS = -include-runtime

TEST_DATA = ../test-data

# Output directories
BUILD = build
CLASSES = $(BUILD)/classes

.PHONY: all clean test test-csv test-errors test-helpers test-roundtrip test-all build build-tests

# Default: build all
all: build

# Build the library, CLI, and tests
build:
	@mkdir -p $(BUILD)
	$(KOTLINC) -d $(BUILD)/mpc_designation.jar $(KFLAGS) src/mpc/*.kt test/*.kt

# Build tests (compile to classes for faster execution)
build-tests: build
	@mkdir -p $(CLASSES)
	$(KOTLINC) -d $(CLASSES) -cp $(BUILD)/mpc_designation.jar test/*.kt

# Run the CLI (for manual testing)
run: build
	$(KOTLIN) -cp $(BUILD)/mpc_designation.jar mpc.MainKt $(ARGS)

# Alternative: run with java
run-java: build
	$(JAVA) -jar $(BUILD)/mpc_designation.jar $(ARGS)

# Run conversion tests
test: test-csv

test-csv: build build-tests
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(KOTLIN) -cp $(BUILD)/mpc_designation.jar:$(CLASSES) mpc.TestCsvKt $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors: build build-tests
	$(KOTLIN) -cp $(BUILD)/mpc_designation.jar:$(CLASSES) mpc.TestErrorsKt $(TEST_DATA)/error_test_cases.csv

# Run round-trip tests
test-roundtrip: build build-tests
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(KOTLIN) -cp $(BUILD)/mpc_designation.jar:$(CLASSES) mpc.TestRoundtripKt $(TEST_DATA)/prov_unpack_to_pack.csv

# Run helper function tests
test-helpers: build build-tests
	$(KOTLIN) -cp $(BUILD)/mpc_designation.jar:$(CLASSES) mpc.TestHelpersKt

# Run all tests
test-all: test-errors test-helpers test-csv test-roundtrip

# Clean build artifacts
clean:
	rm -rf $(BUILD)

# Help
help:
	@echo "MPC Designation Converter (Kotlin) - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all            Build the library (default)"
	@echo "  build          Compile all Kotlin sources"
	@echo "  clean          Remove all build artifacts"
	@echo "  test           Run conversion tests"
	@echo "  test-errors    Run error handling tests (94 cases)"
	@echo "  test-helpers   Run helper function tests (77 cases)"
	@echo "  test-csv       Run CSV conversion tests (2M+ entries)"
	@echo "  test-roundtrip Run round-trip tests"
	@echo "  test-all       Run all tests"
	@echo "  run ARGS=...   Run CLI with arguments"
	@echo ""
	@echo "Examples:"
	@echo "  make run ARGS='1995 XA'"
	@echo "  make run ARGS='-v J95X00A'"
