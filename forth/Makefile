# Makefile for MPC Designation Converter (Forth)

GFORTH = gforth
CSV_FILE = ../test-data/prov_unpack_to_pack.csv

.PHONY: all clean test test-quick test-errors test-csv test-all run help

all:
	@echo "Forth is interpreted - no compilation needed."
	@echo "Run with: $(GFORTH) src/mpc_designation_cli.fs <designation>"

test: test-errors
	@echo "Tests passed."

test-quick: test-errors

test-errors:
	@echo "Running error tests..."
	$(GFORTH) test/test_errors.fs

test-csv: $(CSV_FILE)
	@echo "Running CSV test suite..."
	$(GFORTH) test/test_csv.fs $(CSV_FILE)

test-all: test-errors test-csv

$(CSV_FILE): $(CSV_FILE).gz
	gunzip -k $<

run:
	$(GFORTH) src/mpc_designation_cli.fs $(ARGS)

help:
	$(GFORTH) src/mpc_designation_cli.fs --help

clean:
	@echo "Nothing to clean for Forth."
