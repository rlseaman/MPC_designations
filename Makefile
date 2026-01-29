# Makefile for MPC designation converter (C implementation)

CC = cc
CFLAGS = -O2 -Wall -Wextra

all: mpc_designation_c test_csv_c

mpc_designation_c: mpc_designation_cli.c mpc_designation.c mpc_designation.h
	$(CC) $(CFLAGS) -o $@ mpc_designation_cli.c mpc_designation.c

test_csv_c: test_csv.c mpc_designation.c mpc_designation.h
	$(CC) $(CFLAGS) -o $@ test_csv.c mpc_designation.c

clean:
	rm -f mpc_designation_c test_csv_c

test: test_csv_c
	@if [ ! -f prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k prov_unpack_to_pack.csv.gz; \
	fi
	./test_csv_c prov_unpack_to_pack.csv

.PHONY: all clean test
