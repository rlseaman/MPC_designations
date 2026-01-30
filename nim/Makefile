# Makefile for MPC Designation Converter (Nim)

NIM = nim
NIMFLAGS = -d:release --opt:speed

.PHONY: all clean test test-errors test-csv test-roundtrip build run help

all: build

build: mpc_designation test_errors test_csv test_roundtrip

mpc_designation: src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_errors: test/test_errors.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_csv: test/test_csv.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test_roundtrip: test/test_roundtrip.nim src/mpc_designation.nim
	$(NIM) c $(NIMFLAGS) -o:$@ $<

test: test-errors
	@echo "Tests passed."

test-errors: test_errors
	@echo "Running error tests..."
	./test_errors

test-csv: test_csv
	./test_csv $(CSV)

test-roundtrip: test_roundtrip
	./test_roundtrip $(CSV)

test-all: build
	@echo "Running all tests..."
	./test_errors
	@if [ -f "../test-data/prov_unpack_to_pack.csv" ]; then \
		./test_csv ../test-data/prov_unpack_to_pack.csv; \
	fi

run: mpc_designation
	./mpc_designation $(ARGS)

help: mpc_designation
	./mpc_designation --help

clean:
	rm -f mpc_designation test_errors test_csv test_roundtrip
	rm -rf nimcache/
