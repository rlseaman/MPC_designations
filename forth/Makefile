# Makefile for MPC Designation Converter (Forth)

GFORTH = gforth

.PHONY: all clean test test-errors run help

all:
	@echo "Forth is interpreted - no compilation needed."
	@echo "Run with: $(GFORTH) src/mpc_designation_cli.fs <designation>"

test: test-errors
	@echo "Tests passed."

test-errors:
	@echo "Running error tests..."
	$(GFORTH) test/test_errors.fs

run:
	$(GFORTH) src/mpc_designation_cli.fs $(ARGS)

help:
	$(GFORTH) src/mpc_designation_cli.fs --help

clean:
	@echo "Nothing to clean for Forth."
