# Makefile for MPC Designation Converter (Go implementation)

GO = go
GOFLAGS = -ldflags="-s -w"

TEST_DATA = ../test-data

.PHONY: all clean test test-csv test-errors test-roundtrip test-all build

# Default: build all
all: mpc_designation test_csv test_errors test_roundtrip

# Build the CLI
mpc_designation:
	$(GO) build $(GOFLAGS) -o $@ ./cmd/mpc_designation

# Build test executables
test_csv:
	$(GO) build $(GOFLAGS) -o $@ ./test/test_csv

test_errors:
	$(GO) build $(GOFLAGS) -o $@ ./test/test_errors

test_roundtrip:
	$(GO) build $(GOFLAGS) -o $@ ./test/test_roundtrip

# Run conversion tests
test: test_csv
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	./test_csv $(TEST_DATA)/prov_unpack_to_pack.csv

# Run error handling tests
test-errors: test_errors
	./test_errors $(TEST_DATA)/error_test_cases.csv

# Run bidirectional and round-trip tests
test-roundtrip: test_roundtrip
	@if [ ! -f $(TEST_DATA)/prov_unpack_to_pack.csv ]; then \
		echo "Decompressing test data..."; \
		gunzip -k $(TEST_DATA)/prov_unpack_to_pack.csv.gz; \
	fi
	./test_roundtrip $(TEST_DATA)/prov_unpack_to_pack.csv

# Run all tests
test-all: test-errors test

# Clean build artifacts
clean:
	rm -f mpc_designation test_csv test_errors test_roundtrip

# Cross-compile for multiple platforms
build-all: build-linux build-windows build-darwin

build-linux:
	GOOS=linux GOARCH=amd64 $(GO) build $(GOFLAGS) -o mpc_designation-linux-amd64 ./cmd/mpc_designation
	GOOS=linux GOARCH=arm64 $(GO) build $(GOFLAGS) -o mpc_designation-linux-arm64 ./cmd/mpc_designation

build-windows:
	GOOS=windows GOARCH=amd64 $(GO) build $(GOFLAGS) -o mpc_designation-windows-amd64.exe ./cmd/mpc_designation

build-darwin:
	GOOS=darwin GOARCH=amd64 $(GO) build $(GOFLAGS) -o mpc_designation-darwin-amd64 ./cmd/mpc_designation
	GOOS=darwin GOARCH=arm64 $(GO) build $(GOFLAGS) -o mpc_designation-darwin-arm64 ./cmd/mpc_designation
