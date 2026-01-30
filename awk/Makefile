# Makefile for MPC Designation Converter (AWK)

.PHONY: all clean test test-errors run help

all:
	@echo "AWK scripts need no compilation."
	@echo "Run with: ./src/mpc_designation_cli.sh <designation>"

test: test-errors
	@echo "Tests passed."

test-errors:
	@echo "Running error tests..."
	./test/test_errors.sh

run:
	./src/mpc_designation_cli.sh $(ARGS)

help:
	./src/mpc_designation_cli.sh --help

clean:
	@echo "Nothing to clean for AWK."
