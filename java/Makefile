# Makefile for MPC Designation Converter (Java implementation)

JAVAC = javac
JAVA = java
JFLAGS = -d classes -sourcepath src:test

TEST_DATA = ../test-data

# Output directories
CLASSES = classes

.PHONY: all clean test test-csv test-errors test-roundtrip test-helpers test-all build

# Default: build all
all: build

# Create output directory
$(CLASSES):
	mkdir -p $(CLASSES)

# Build the library and CLI
build: $(CLASSES)
	$(JAVAC) $(JFLAGS) src/mpc/*.java

# Build tests
build-tests: build
	$(JAVAC) $(JFLAGS) test/*.java

# Run the CLI (for manual testing)
run: build
	$(JAVA) -cp $(CLASSES) mpc.MPCDesignationCLI $(ARGS)

# Run conversion tests
test: test-csv

test-csv: build-tests
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(JAVA) -cp $(CLASSES) mpc.TestCSV $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors: build-tests
	$(JAVA) -cp $(CLASSES) mpc.TestErrors $(TEST_DATA)/error_test_cases.csv

# Run round-trip tests
test-roundtrip: build-tests
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(JAVA) -cp $(CLASSES) mpc.TestRoundtrip $(TEST_DATA)/prov_unpack_to_pack.csv

# Run helper function tests
test-helpers: build-tests
	$(JAVA) -cp $(CLASSES) TestHelpers

# Run all tests
test-all: test-errors test-helpers test-csv

# Clean build artifacts
clean:
	rm -rf $(CLASSES)

# Create a JAR file
jar: build
	jar cf mpc-designation.jar -C $(CLASSES) mpc

# Help
help:
	@echo "MPC Designation Converter (Java) - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          Build the library (default)"
	@echo "  build        Compile all Java sources"
	@echo "  clean        Remove all build artifacts"
	@echo "  test         Run conversion tests"
	@echo "  test-errors  Run error handling tests"
	@echo "  test-helpers Run helper function tests"
	@echo "  test-roundtrip Run round-trip tests"
	@echo "  test-all     Run all tests"
	@echo "  jar          Create JAR file"
	@echo "  run ARGS=... Run CLI with arguments"
	@echo ""
	@echo "Examples:"
	@echo "  make run ARGS='1995 XA'"
	@echo "  make run ARGS='-v J95X00A'"
