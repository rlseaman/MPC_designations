# Makefile for MPC designation converter (C implementation)

CC = cc
CFLAGS = -O2 -Wall -Wextra

all: mpc_designation_c test_csv_c test_errors_c

mpc_designation_c: mpc_designation_cli.c mpc_designation.c mpc_designation.h
	$(CC) $(CFLAGS) -o $@ mpc_designation_cli.c mpc_designation.c

test_csv_c: test_csv.c mpc_designation.c mpc_designation.h
	$(CC) $(CFLAGS) -o $@ test_csv.c mpc_designation.c

test_errors_c: test_errors.c mpc_designation.c mpc_designation.h
	$(CC) $(CFLAGS) -o $@ test_errors.c mpc_designation.c

clean:
	rm -f mpc_designation_c test_csv_c test_errors_c

# Run conversion tests (valid input/output pairs)
test: test_csv_c
	@if [ ! -f prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k prov_unpack_to_pack.csv.gz; \
	fi
	./test_csv_c prov_unpack_to_pack.csv

# Run error handling tests (invalid inputs)
test-errors: test_errors_c
	./test_errors_c error_test_cases.csv

# Run all tests
test-all: test test-errors
	@echo ""
	@echo "=== Python Conversion Tests ==="
	python3 test_csv.py prov_unpack_to_pack.csv
	@echo ""
	@echo "=== Python Error Tests ==="
	python3 test_errors.py error_test_cases.csv
	@echo ""
	@echo "=== TCL Error Tests ==="
	tclsh test_errors.tcl error_test_cases.csv

.PHONY: all clean test test-errors test-all
