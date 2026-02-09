# Makefile for MPC designation converter (C implementation)

CC = cc
CFLAGS = -O2 -Wall -Wextra
SRCDIR = src
TESTDIR = test
TEST_DATA = ../test-data

# Read version from root VERSION file
VERSION := $(shell cat ../VERSION 2>/dev/null || echo "1.0.0")

# Include path for header and version definition
INCLUDES = -I$(SRCDIR)
DEFINES = -DMPC_VERSION=\"$(VERSION)\"

all: mpc_designation test_csv test_errors test_roundtrip test_fragments test_helpers

mpc_designation: $(SRCDIR)/mpc_designation_cli.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $(SRCDIR)/mpc_designation_cli.c $(SRCDIR)/mpc_designation.c

test_csv: $(TESTDIR)/test_csv.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $(TESTDIR)/test_csv.c $(SRCDIR)/mpc_designation.c

test_errors: $(TESTDIR)/test_errors.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $(TESTDIR)/test_errors.c $(SRCDIR)/mpc_designation.c

test_roundtrip: $(TESTDIR)/test_roundtrip.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $(TESTDIR)/test_roundtrip.c $(SRCDIR)/mpc_designation.c

test_fragments: $(TESTDIR)/test_fragments.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $(TESTDIR)/test_fragments.c $(SRCDIR)/mpc_designation.c

test_helpers: $(TESTDIR)/test_helpers.c $(SRCDIR)/mpc_designation.c $(SRCDIR)/mpc_designation.h
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $(TESTDIR)/test_helpers.c $(SRCDIR)/mpc_designation.c

clean:
	rm -f mpc_designation test_csv test_errors test_roundtrip test_fragments test_helpers

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

# Run bidirectional and round-trip tests
test-roundtrip: test_roundtrip
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	./test_roundtrip $(TEST_DATA)/prov_unpack_to_pack.csv

# Run fragment handling tests
test-fragments: test_fragments
	./test_fragments

# Run helper function tests
test-helpers: test_helpers
	./test_helpers

# Run all tests
test-all: test test-errors test-fragments test-helpers

.PHONY: all clean test test-errors test-roundtrip test-fragments test-helpers test-all
