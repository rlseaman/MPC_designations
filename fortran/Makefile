# Makefile for MPC Designation Converter (Fortran implementation)

FC = gfortran
FFLAGS = -O2 -Wall -Wextra -std=f2008 -fcheck=all
FFLAGS_RELEASE = -O3 -std=f2008

TEST_DATA = ../test-data

# Output directory
BUILD = build

.PHONY: all clean test test-csv test-errors test-roundtrip test-all build

# Default: build all
all: $(BUILD)/mpc_designation_cli

# Create output directory
$(BUILD):
	mkdir -p $(BUILD)

# Build the library and CLI
build: $(BUILD)/mpc_designation_cli

$(BUILD)/mpc_designation.o: src/mpc_designation.f90
	@mkdir -p $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(BUILD)

$(BUILD)/mpc_designation_cli: src/mpc_designation_cli.f90 $(BUILD)/mpc_designation.o
	$(FC) $(FFLAGS) -I$(BUILD) $^ -o $@

# Build tests
$(BUILD)/test_csv: test/test_csv.f90 $(BUILD)/mpc_designation.o
	$(FC) $(FFLAGS) -I$(BUILD) $^ -o $@

$(BUILD)/test_errors: test/test_errors.f90 $(BUILD)/mpc_designation.o
	$(FC) $(FFLAGS) -I$(BUILD) $^ -o $@

$(BUILD)/test_roundtrip: test/test_roundtrip.f90 $(BUILD)/mpc_designation.o
	$(FC) $(FFLAGS) -I$(BUILD) $^ -o $@

# Run the CLI (for manual testing)
run: $(BUILD)/mpc_designation_cli
	$(BUILD)/mpc_designation_cli $(ARGS)

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

# Run roundtrip tests
test-roundtrip: $(BUILD)/test_roundtrip
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	$(BUILD)/test_roundtrip $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test-errors test-csv test-roundtrip

# Clean build artifacts
clean:
	rm -rf $(BUILD)

# Help
help:
	@echo "MPC Designation Converter (Fortran) - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          Build the library and CLI (default)"
	@echo "  build        Compile all Fortran sources"
	@echo "  clean        Remove all build artifacts"
	@echo "  test         Run conversion tests"
	@echo "  test-errors  Run error handling tests"
	@echo "  test-all     Run all tests"
	@echo "  run ARGS=... Run CLI with arguments"
	@echo ""
	@echo "Examples:"
	@echo "  make run ARGS='1995 XA'"
	@echo "  make run ARGS='-v J95X00A'"
