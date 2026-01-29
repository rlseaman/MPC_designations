# Makefile for MPC designation converter (C implementation)

CC = cc
CFLAGS = -O2 -Wall -Wextra
SRCDIR = src
TESTDIR = test
TEST_DATA = ../test-data

# Include path for header
INCLUDES = -I$(SRCDIR)

all: mpc_designation test_csv test_errors

mpc_designation: $(SRCDIR)/mpc_designation_cli.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ $(SRCDIR)/mpc_designation_cli.c $(SRCDIR)/mpc_designation.c

test_csv: $(TESTDIR)/test_csv.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ $(TESTDIR)/test_csv.c $(SRCDIR)/mpc_designation.c

test_errors: $(TESTDIR)/test_errors.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ $(TESTDIR)/test_errors.c $(SRCDIR)/mpc_designation.c

clean:
	rm -f mpc_designation test_csv test_errors

# Run conversion tests (valid input/output pairs)
test: test_csv
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	./test_csv $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests (invalid inputs)
test-errors: test_errors
	./test_errors $(TEST_DATA)/error_test_cases.csv

# Run all tests
test-all: test test-errors

.PHONY: all clean test test-errors test-all
