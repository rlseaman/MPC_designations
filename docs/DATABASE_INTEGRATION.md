# Database Integration Guide

This guide explains how to use the MPC designation converter with various database systems.

## Overview

The MPC designation converter can be integrated with databases in two ways:

1. **Application-level integration**: Use the converter in your application code before/after database operations
2. **Database UDF integration**: Register custom functions directly in the database

## Use Cases

### 1. Data Import/Normalization
When importing asteroid/comet data from various sources, designations may be in different formats:
- JPL Horizons uses unpacked format: "1995 XA", "C/1995 O1"
- MPC observation files use packed format: "J95X00A", "CJ95O010"

Normalize to a consistent format during import to ensure uniform querying.

### 2. Cross-System Queries
When joining data from multiple sources, convert designations to a common format:
```sql
-- Without UDF: normalize in application layer before query
SELECT * FROM observations WHERE packed_id = ?
-- Application converts user input "1995 XA" to "J95X00A" before query
```

### 3. User-Friendly Display
Store designations in packed format (space-efficient) but display in unpacked format (human-readable).

### 4. Duplicate Detection
Convert all designations to a canonical form to detect duplicates that may appear in different formats.

---

## Python + SQLite Integration

SQLite supports user-defined functions (UDFs) through Python's `sqlite3` module.

### Setup

```python
import sqlite3
import sys
sys.path.insert(0, '/path/to/mpc_designation/python/src')
from mpc_designation import convert_simple, MPCDesignationError

def mpc_pack(designation):
    """Convert to packed format (returns None on error)."""
    try:
        # If already packed, return as-is
        result = convert_simple(designation)
        # If input was unpacked, result is packed; if input was packed, result is unpacked
        # We need to determine which way to go
        if len(designation.strip()) > len(result.strip()):
            return result  # Was unpacked, now packed
        return designation.strip()  # Was already packed
    except MPCDesignationError:
        return None

def mpc_unpack(designation):
    """Convert to unpacked format (returns None on error)."""
    try:
        result = convert_simple(designation)
        if len(result.strip()) > len(designation.strip()):
            return result  # Was packed, now unpacked
        return designation.strip()  # Was already unpacked
    except MPCDesignationError:
        return None

def mpc_convert(designation):
    """Convert between formats (auto-detect)."""
    try:
        return convert_simple(designation)
    except MPCDesignationError:
        return None

# Register functions with SQLite
conn = sqlite3.connect(':memory:')
conn.create_function('mpc_pack', 1, mpc_pack)
conn.create_function('mpc_unpack', 1, mpc_unpack)
conn.create_function('mpc_convert', 1, mpc_convert)
```

### Usage Examples

```python
# Create test table
conn.execute('''
    CREATE TABLE observations (
        id INTEGER PRIMARY KEY,
        designation TEXT,
        magnitude REAL
    )
''')

# Insert data in various formats
conn.execute("INSERT INTO observations (designation, magnitude) VALUES ('1995 XA', 15.2)")
conn.execute("INSERT INTO observations (designation, magnitude) VALUES ('J95X00B', 14.8)")
conn.execute("INSERT INTO observations (designation, magnitude) VALUES ('C/1995 O1', 12.5)")

# Query with conversion
cursor = conn.execute('''
    SELECT designation, mpc_convert(designation) as converted, magnitude
    FROM observations
''')
for row in cursor:
    print(row)
# Output:
# ('1995 XA', 'J95X00A', 15.2)
# ('J95X00B', '1995 XB', 14.8)
# ('C/1995 O1', 'CJ95O010', 12.5)

# Search by either format
def find_by_designation(conn, user_input):
    """Find observations matching a designation in any format."""
    packed = mpc_pack(user_input) or user_input
    unpacked = mpc_unpack(user_input) or user_input

    cursor = conn.execute('''
        SELECT * FROM observations
        WHERE designation = ? OR designation = ?
    ''', (packed, unpacked))
    return cursor.fetchall()

# Works with either format
find_by_designation(conn, '1995 XA')   # Finds "1995 XA"
find_by_designation(conn, 'J95X00A')   # Also finds "1995 XA"
```

### Normalization Pattern

For large databases, consider normalizing to packed format for storage efficiency:

```python
def normalize_designation_column(conn, table, column):
    """Normalize all designations in a column to packed format."""
    conn.execute(f'''
        UPDATE {table}
        SET {column} = mpc_pack({column})
        WHERE mpc_pack({column}) IS NOT NULL
          AND mpc_pack({column}) != {column}
    ''')
    conn.commit()
```

---

## C UDF Plugin for MariaDB/MySQL

MariaDB and MySQL support loadable user-defined functions written in C.

### Prerequisites

- MariaDB/MySQL development headers
- C compiler (gcc/clang)
- The mpc_designation C library

### Building the Plugin

Create `mpc_designation_udf.c`:

```c
#include <mysql.h>
#include <string.h>
#include "mpc_designation.h"

/*
 * mpc_convert(designation) -> converted designation
 * Auto-detects format and converts to the other
 */

my_bool mpc_convert_init(UDF_INIT *initid, UDF_ARGS *args, char *message) {
    if (args->arg_count != 1) {
        strcpy(message, "mpc_convert() requires exactly one argument");
        return 1;
    }
    if (args->arg_type[0] != STRING_RESULT) {
        strcpy(message, "mpc_convert() requires a string argument");
        return 1;
    }
    initid->max_length = MPC_MAX_UNPACKED;
    initid->maybe_null = 1;
    initid->ptr = malloc(MPC_MAX_UNPACKED);
    return 0;
}

void mpc_convert_deinit(UDF_INIT *initid) {
    if (initid->ptr) free(initid->ptr);
}

char *mpc_convert(UDF_INIT *initid, UDF_ARGS *args, char *result,
                   unsigned long *length, char *is_null, char *error) {
    if (args->args[0] == NULL) {
        *is_null = 1;
        return NULL;
    }

    char *output = initid->ptr;
    int rc = mpc_convert_simple(args->args[0], output, MPC_MAX_UNPACKED);

    if (rc != MPC_OK) {
        *is_null = 1;
        return NULL;
    }

    *length = strlen(output);
    *is_null = 0;
    return output;
}

/*
 * mpc_detect(designation) -> 'packed' or 'unpacked' or NULL
 */

my_bool mpc_detect_init(UDF_INIT *initid, UDF_ARGS *args, char *message) {
    if (args->arg_count != 1) {
        strcpy(message, "mpc_detect() requires exactly one argument");
        return 1;
    }
    initid->max_length = 16;
    initid->maybe_null = 1;
    return 0;
}

void mpc_detect_deinit(UDF_INIT *initid) {}

char *mpc_detect(UDF_INIT *initid, UDF_ARGS *args, char *result,
                  unsigned long *length, char *is_null, char *error) {
    if (args->args[0] == NULL) {
        *is_null = 1;
        return NULL;
    }

    mpc_info_t info;
    int rc = mpc_detect_format(args->args[0], &info);

    if (rc != MPC_OK) {
        *is_null = 1;
        return NULL;
    }

    if (info.format == MPC_FORMAT_PACKED) {
        strcpy(result, "packed");
        *length = 6;
    } else {
        strcpy(result, "unpacked");
        *length = 8;
    }
    *is_null = 0;
    return result;
}
```

### Compilation

```bash
# Linux
gcc -shared -fPIC -o mpc_designation_udf.so mpc_designation_udf.c \
    mpc_designation.c \
    $(mysql_config --cflags) \
    -I/path/to/mpc_designation/c/src

# macOS
gcc -bundle -o mpc_designation_udf.so mpc_designation_udf.c \
    mpc_designation.c \
    $(mysql_config --cflags) \
    -I/path/to/mpc_designation/c/src
```

### Installation

```bash
# Copy to MySQL plugin directory
sudo cp mpc_designation_udf.so $(mysql_config --plugindir)/

# In MySQL/MariaDB
CREATE FUNCTION mpc_convert RETURNS STRING SONAME 'mpc_designation_udf.so';
CREATE FUNCTION mpc_detect RETURNS STRING SONAME 'mpc_designation_udf.so';
```

### Usage

```sql
-- Convert designations
SELECT mpc_convert('1995 XA');
-- Returns: 'J95X00A'

SELECT mpc_convert('CJ95O010');
-- Returns: 'C/1995 O1'

-- Detect format
SELECT mpc_detect('J95X00A');
-- Returns: 'packed'

-- Use in queries
SELECT designation, mpc_convert(designation) as converted
FROM observations
WHERE mpc_detect(designation) = 'unpacked';

-- Normalize table
UPDATE observations
SET designation = mpc_convert(designation)
WHERE mpc_detect(designation) = 'unpacked';
```

---

## PostgreSQL Integration

PostgreSQL supports UDFs in various languages including PL/Python.

### Using PL/Python

```sql
-- Enable PL/Python
CREATE EXTENSION IF NOT EXISTS plpython3u;

-- Create wrapper function
CREATE OR REPLACE FUNCTION mpc_convert(designation text)
RETURNS text AS $$
import sys
sys.path.insert(0, '/path/to/mpc_designation/python/src')
from mpc_designation import convert_simple, MPCDesignationError
try:
    return convert_simple(designation)
except MPCDesignationError:
    return None
$$ LANGUAGE plpython3u;

-- Usage
SELECT mpc_convert('1995 XA');  -- Returns 'J95X00A'
```

---

## Performance Considerations

1. **Indexing**: Store designations in a consistent format (packed or unpacked) and create an index. Don't rely on UDF conversions for indexed lookups.

2. **Batch Processing**: For large imports, convert designations in application code rather than using UDFs for each row.

3. **Caching**: If converting user input repeatedly, cache the conversion results.

4. **Format Choice**:
   - Packed format: More compact (7 chars vs ~10+ chars), better for storage
   - Unpacked format: Human-readable, better for display and debugging

---

## Best Practices

1. **Choose a canonical format** for storage and stick with it consistently

2. **Convert at boundaries**: Convert when data enters/leaves the system, not during queries

3. **Handle errors gracefully**: Invalid designations should return NULL, not throw errors

4. **Test with edge cases**: Include comets, satellites, and high-numbered asteroids in tests

5. **Document the format** used in each column/table

---

## See Also

- [examples/sqlite_integration.py](../python/examples/sqlite_integration.py) - Complete SQLite example
- [examples/mariadb_udf/](../c/examples/mariadb_udf/) - MariaDB UDF example
