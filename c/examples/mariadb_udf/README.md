# MPC Designation MariaDB/MySQL UDF Plugin

This directory contains a User Defined Function (UDF) plugin for MariaDB and MySQL
that provides MPC designation conversion functions directly in SQL.

## Prerequisites

- MariaDB 10.x or MySQL 5.7+ with UDF support
- Development headers (`mariadb-devel` or `mysql-devel` package)
- C compiler (gcc or clang)

## Installation

### 1. Install Development Headers

**Ubuntu/Debian:**
```bash
sudo apt-get install libmariadb-dev
# or for MySQL
sudo apt-get install libmysqlclient-dev
```

**CentOS/RHEL:**
```bash
sudo yum install mariadb-devel
# or for MySQL
sudo yum install mysql-devel
```

**macOS (with Homebrew):**
```bash
brew install mariadb
# or
brew install mysql
```

### 2. Build the Plugin

```bash
make
```

### 3. Install the Plugin

```bash
make install
```

This copies `mpc_designation_udf.so` to the MySQL plugin directory.

### 4. Load Functions in MariaDB/MySQL

Connect to your database and run:

```sql
CREATE FUNCTION mpc_convert RETURNS STRING SONAME 'mpc_designation_udf.so';
CREATE FUNCTION mpc_detect RETURNS STRING SONAME 'mpc_designation_udf.so';
CREATE FUNCTION mpc_type RETURNS STRING SONAME 'mpc_designation_udf.so';
```

## Functions

### mpc_convert(designation)

Converts between packed and unpacked MPC designation formats.
Auto-detects the input format and converts to the other.

Returns NULL if the designation is invalid.

```sql
SELECT mpc_convert('1995 XA');
-- Returns: 'J95X00A'

SELECT mpc_convert('CJ95O010');
-- Returns: 'C/1995 O1'
```

### mpc_detect(designation)

Detects the format of a designation.
Returns 'packed', 'unpacked', or NULL if invalid.

```sql
SELECT mpc_detect('J95X00A');
-- Returns: 'packed'

SELECT mpc_detect('1995 XA');
-- Returns: 'unpacked'
```

### mpc_type(designation)

Detects the type of a designation.
Returns one of: permanent, provisional, provisional_extended, survey,
comet_numbered, comet_provisional, comet_full, comet_ancient, comet_bce, satellite.
Returns NULL if invalid.

```sql
SELECT mpc_type('1995 XA');
-- Returns: 'provisional'

SELECT mpc_type('C/1995 O1');
-- Returns: 'comet_full'
```

## Usage Examples

### Convert All Designations in a Table

```sql
-- Display converted designations
SELECT designation, mpc_convert(designation) as converted
FROM observations;

-- Normalize to packed format
UPDATE observations
SET designation = mpc_convert(designation)
WHERE mpc_detect(designation) = 'unpacked';
```

### Filter by Designation Type

```sql
-- Find all comets
SELECT * FROM observations
WHERE mpc_type(designation) LIKE 'comet%';

-- Find provisional asteroids
SELECT * FROM observations
WHERE mpc_type(designation) = 'provisional';
```

### Search by Either Format

```sql
-- Create a view that includes both formats
CREATE VIEW observations_with_both AS
SELECT
    id,
    designation AS original,
    CASE mpc_detect(designation)
        WHEN 'packed' THEN designation
        ELSE mpc_convert(designation)
    END AS packed,
    CASE mpc_detect(designation)
        WHEN 'unpacked' THEN designation
        ELSE mpc_convert(designation)
    END AS unpacked,
    magnitude
FROM observations;

-- Now search works with either format
SELECT * FROM observations_with_both
WHERE packed = 'J95X00A' OR unpacked = '1995 XA';
```

## Uninstallation

Remove the functions:

```sql
DROP FUNCTION IF EXISTS mpc_convert;
DROP FUNCTION IF EXISTS mpc_detect;
DROP FUNCTION IF EXISTS mpc_type;
```

Remove the plugin file:

```bash
make uninstall
```

## Troubleshooting

### "Cannot find mysql.h"

Install the MariaDB/MySQL development package:
- Ubuntu: `sudo apt-get install libmariadb-dev`
- CentOS: `sudo yum install mariadb-devel`

### "Can't open shared library"

1. Check the plugin directory: `SELECT @@plugin_dir;`
2. Verify the file exists: `ls -la /path/to/plugin/dir/mpc_designation_udf.so`
3. Check permissions: The MySQL user must be able to read the file

### "No paths allowed for shared library"

Use only the filename, not the full path:
```sql
-- Wrong:
CREATE FUNCTION mpc_convert RETURNS STRING SONAME '/path/to/mpc_designation_udf.so';

-- Correct:
CREATE FUNCTION mpc_convert RETURNS STRING SONAME 'mpc_designation_udf.so';
```

### Function returns NULL unexpectedly

The functions return NULL for invalid designations. Check:
- Correct format (e.g., "1995 XA" not "1995  XA")
- No leading/trailing whitespace
- Valid half-month letters (A-Y, excluding I)

## Performance Notes

- UDFs add overhead compared to simple column lookups
- For indexed columns, convert values in application code
- Use UDFs for display/transformation, not WHERE clauses on large tables
- Consider materializing conversions if frequently needed
