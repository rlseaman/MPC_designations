# Makefile for MPC Designation Converter (Nim)

NIM = nim
NIMFLAGS = -d:release --opt:speed

.PHONY: all clean test test-errors test-helpers test-csv test-roundtrip build run help

all: build

build: mpc_designation test_errors test_helpers test_csv test_roundtrip

mpc_designation: src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_errors: test/test_errors.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_csv: test/test_csv.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_roundtrip: test/test_roundtrip.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_helpers: test/test_helpers.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test: test-errors test-helpers
	@echo "Tests passed."

test-errors: test_errors
	@echo "Running error tests..."
	./test_errors

test-helpers: test_helpers
	@echo "Running helper tests..."
	./test_helpers

test-csv: test_csv
	./test_csv $(CSV)

test-roundtrip: test_roundtrip
	./test_roundtrip $(CSV)

test-all: build
	@echo "Running all tests..."
	./test_errors
	./test_helpers
	@if [ -f "../test-data/prov_unpack_to_pack.csv" ]; then \
		./test_csv ../test-data/prov_unpack_to_pack.csv; \
	fi

run: mpc_designation
	./mpc_designation $(ARGS)

help: mpc_designation
	./mpc_designation --help

clean:
	rm -f mpc_designation test_errors test_helpers test_csv test_roundtrip
	rm -rf nimcache/
