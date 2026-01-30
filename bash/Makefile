# Makefile for MPC Designation Converter (Bash)

.PHONY: all clean test test-errors run help

all:
	@echo "Bash scripts need no compilation."
	@echo "Run with: ./src/mpc_designation.sh <designation>"

test: test-errors
	@echo "Tests passed."

test-errors:
	@echo "Running error tests..."
	./test/test_errors.sh

run:
	./src/mpc_designation.sh $(ARGS)

help:
	./src/mpc_designation.sh --help

clean:
	@echo "Nothing to clean for Bash."
