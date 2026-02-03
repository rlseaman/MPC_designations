# Makefile for R MPC Designation converter

.PHONY: all test test-quick test-csv test-errors clean help

all: test-quick

# Run quick tests
test-quick:
	@echo "Running quick tests..."
	@Rscript test/test_quick.R

# Run CSV test suite
test-csv:
	@echo "Running CSV test suite..."
	@Rscript test/test_csv.R

# Run error handling tests
test-errors:
	@echo "Running error handling tests..."
	@Rscript test/test_errors.R

# Run all tests
test: test-quick test-errors test-csv

# Clean generated files
clean:
	@rm -f *.Rout
	@rm -f .RData .Rhistory

# Show help
help:
	@echo "Available targets:"
	@echo "  make test-quick  - Run quick validation tests"
	@echo "  make test-csv    - Run full CSV test suite (2M+ tests)"
	@echo "  make test-errors - Run error handling tests"
	@echo "  make test        - Run all tests"
	@echo "  make clean       - Remove generated files"
	@echo ""
	@echo "CLI usage:"
	@echo "  Rscript src/mpc_designation_cli.R '1995 XA'"
	@echo "  Rscript src/mpc_designation_cli.R J95X00A"
