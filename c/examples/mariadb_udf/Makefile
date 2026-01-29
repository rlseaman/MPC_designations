# Makefile for MPC Designation MariaDB/MySQL UDF Plugin
#
# Prerequisites:
#   - MariaDB or MySQL development headers
#   - mysql_config in PATH
#
# Usage:
#   make              - Build the plugin
#   make install      - Install to MySQL plugin directory (requires sudo)
#   make uninstall    - Remove from MySQL plugin directory
#   make clean        - Remove build artifacts

CC = gcc
CFLAGS = -Wall -Wextra -O2 -fPIC
MYSQL_CFLAGS = $(shell mysql_config --cflags 2>/dev/null || mariadb_config --cflags)
MYSQL_PLUGINDIR = $(shell mysql_config --plugindir 2>/dev/null || mariadb_config --plugindir)

# Source files
MPC_SRC = ../../src/mpc_designation.c
UDF_SRC = mpc_designation_udf.c

# Target
TARGET = mpc_designation_udf.so

# Detect OS for shared library flags
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    SHARED_FLAGS = -bundle -undefined dynamic_lookup
else
    SHARED_FLAGS = -shared
endif

all: $(TARGET)

$(TARGET): $(UDF_SRC) $(MPC_SRC)
	@if [ -z "$(MYSQL_CFLAGS)" ]; then \
		echo "Error: mysql_config or mariadb_config not found in PATH"; \
		echo "Install MariaDB/MySQL development headers first"; \
		exit 1; \
	fi
	$(CC) $(SHARED_FLAGS) $(CFLAGS) -o $@ $^ $(MYSQL_CFLAGS) -I../../src

install: $(TARGET)
	@if [ -z "$(MYSQL_PLUGINDIR)" ]; then \
		echo "Error: Could not determine MySQL plugin directory"; \
		exit 1; \
	fi
	@echo "Installing to $(MYSQL_PLUGINDIR)/"
	sudo cp $(TARGET) $(MYSQL_PLUGINDIR)/

uninstall:
	@if [ -z "$(MYSQL_PLUGINDIR)" ]; then \
		echo "Error: Could not determine MySQL plugin directory"; \
		exit 1; \
	fi
	@echo "Removing from $(MYSQL_PLUGINDIR)/"
	sudo rm -f $(MYSQL_PLUGINDIR)/$(TARGET)

clean:
	rm -f $(TARGET)

# Help target
help:
	@echo "MPC Designation MariaDB/MySQL UDF Plugin"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the plugin (default)"
	@echo "  install   - Install to MySQL plugin directory"
	@echo "  uninstall - Remove from MySQL plugin directory"
	@echo "  clean     - Remove build artifacts"
	@echo ""
	@echo "After building and installing, load the functions in MySQL:"
	@echo ""
	@echo "  CREATE FUNCTION mpc_convert RETURNS STRING SONAME 'mpc_designation_udf.so';"
	@echo "  CREATE FUNCTION mpc_detect RETURNS STRING SONAME 'mpc_designation_udf.so';"
	@echo "  CREATE FUNCTION mpc_type RETURNS STRING SONAME 'mpc_designation_udf.so';"

.PHONY: all install uninstall clean help
