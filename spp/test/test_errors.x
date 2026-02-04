# test_errors.x - Error test suite for MPC Designation Converter
#
# This tests that invalid inputs are handled correctly (return empty string)
#
# The test CSV file path is hardcoded for direct execution.

task	test_errors = t_tsterr

include	<ctype.h>

define	MAX_DES		80
define	MAX_LINE	256
define	CSV_FILE	"../../test-data/error_test_cases.csv"

procedure t_tsterr ()

char	line[MAX_LINE]
char	category[MAX_DES], subcat[MAX_DES], raw_input[MAX_DES]
char	expected[MAX_DES], descrip[MAX_DES]
char	input[MAX_DES], result[MAX_DES]
int	fd, total, passed, failed
int	len, strlen(), open(), getline()
bool	got_error

begin
	fd = open (CSV_FILE, READ_ONLY, TEXT_FILE)
	if (fd == ERR) {
	    call eprintf ("Cannot open file: %s\n")
		call pargstr (CSV_FILE)
	    return
	}

	total = 0
	passed = 0
	failed = 0

	call printf ("=== MPC Designation Error Tests (SPP) ===\n\n")

	# Read lines
	while (getline (fd, line) != EOF) {
	    len = strlen (line)
	    if (len == 0)
		next
	    if (line[1] == '#')
		next
	    if (line[1] == 'c' && line[2] == 'a' && line[3] == 't')
		next

	    # Parse CSV: category,subcategory,input,expected,description
	    call tes_parseline (line, category, subcat, raw_input,
				expected, descrip)

	    # Unescape input
	    call tes_unescape (raw_input, input, MAX_DES)

	    total = total + 1

	    # Convert
	    call mpc_convert (input, result, MAX_DES)

	    # Check result
	    got_error = (result[1] == EOS)

	    if (expected[1] == 'v' && expected[2] == 'a' &&
		expected[3] == 'l' && expected[4] == 'i' &&
		expected[5] == 'd') {
		# Expected valid conversion
		if (got_error) {
		    failed = failed + 1
		    call printf ("FAIL [%s/%s]: '%s'\n")
			call pargstr (category)
			call pargstr (subcat)
			call pargstr (descrip)
		    call printf ("      Expected: valid conversion\n")
		    call printf ("      Got: empty (error)\n")
		} else {
		    passed = passed + 1
		}
	    } else {
		# Expected error
		if (!got_error) {
		    failed = failed + 1
		    call printf ("FAIL [%s/%s]: '%s'\n")
			call pargstr (category)
			call pargstr (subcat)
			call pargstr (descrip)
		    call printf ("      Expected: error (%s)\n")
			call pargstr (expected)
		    call printf ("      Got: '%s' (success)\n")
			call pargstr (result)
		} else {
		    passed = passed + 1
		}
	    }
	}

	call close (fd)

	call printf ("\n=== Error Test Results ===\n")
	call printf ("Total:  %d\n")
	    call pargi (total)
	call printf ("Passed: %d\n")
	    call pargi (passed)
	call printf ("Failed: %d\n")
	    call pargi (failed)

	if (failed == 0)
	    call printf ("All error tests passed!\n")
end


# TES_PARSELINE -- Parse a CSV line into 5 fields

procedure tes_parseline (line, f1, f2, f3, f4, f5)

char	line[ARB], f1[ARB], f2[ARB], f3[ARB], f4[ARB], f5[ARB]

int	i, j, field, len, strlen()
char	comma

begin
	comma = ','
	len = strlen (line)
	field = 1
	j = 1

	f1[1] = EOS
	f2[1] = EOS
	f3[1] = EOS
	f4[1] = EOS
	f5[1] = EOS

	do i = 1, len {
	    if (line[i] == comma) {
		switch (field) {
		case 1:
		    f1[j] = EOS
		case 2:
		    f2[j] = EOS
		case 3:
		    f3[j] = EOS
		case 4:
		    f4[j] = EOS
		case 5:
		    f5[j] = EOS
		}
		field = field + 1
		j = 1
	    } else {
		switch (field) {
		case 1:
		    f1[j] = line[i]
		    j = j + 1
		case 2:
		    f2[j] = line[i]
		    j = j + 1
		case 3:
		    f3[j] = line[i]
		    j = j + 1
		case 4:
		    f4[j] = line[i]
		    j = j + 1
		case 5:
		    f5[j] = line[i]
		    j = j + 1
		}
	    }
	}

	# Terminate last field
	switch (field) {
	case 1:
	    f1[j] = EOS
	case 2:
	    f2[j] = EOS
	case 3:
	    f3[j] = EOS
	case 4:
	    f4[j] = EOS
	case 5:
	    f5[j] = EOS
	}
end


# TES_UNESCAPE -- Unescape escape sequences in test input

procedure tes_unescape (input, output, maxch)

char	input[ARB], output[ARB]
int	maxch

int	i, j, len, hexval, strlen(), tes_hexval()
char	c

begin
	len = strlen (input)
	i = 1
	j = 1

	while (i <= len && j <= maxch) {
	    if (input[i] == '\\' && i < len) {
		c = input[i + 1]
		if (c == 't') {
		    output[j] = 9    # tab
		    i = i + 2
		    j = j + 1
		} else if (c == 'n') {
		    output[j] = 10   # newline
		    i = i + 2
		    j = j + 1
		} else if (c == 'r') {
		    output[j] = 13   # carriage return
		    i = i + 2
		    j = j + 1
		} else if (c == 'f') {
		    output[j] = 12   # form feed
		    i = i + 2
		    j = j + 1
		} else if (c == 'v') {
		    output[j] = 11   # vertical tab
		    i = i + 2
		    j = j + 1
		} else if (c == 'x' && i + 3 <= len) {
		    # Parse hex escape \xNN
		    hexval = tes_hexval (input[i+2], input[i+3])
		    if (hexval >= 0) {
			output[j] = hexval
			i = i + 4
			j = j + 1
		    } else {
			output[j] = input[i]
			i = i + 1
			j = j + 1
		    }
		} else {
		    output[j] = input[i]
		    i = i + 1
		    j = j + 1
		}
	    } else {
		output[j] = input[i]
		i = i + 1
		j = j + 1
	    }
	}

	output[j] = EOS
end


# TES_HEXVAL -- Convert two hex characters to integer

int procedure tes_hexval (c1, c2)

char	c1, c2

int	v1, v2

begin
	if (c1 >= '0' && c1 <= '9')
	    v1 = c1 - '0'
	else if (c1 >= 'a' && c1 <= 'f')
	    v1 = c1 - 'a' + 10
	else if (c1 >= 'A' && c1 <= 'F')
	    v1 = c1 - 'A' + 10
	else
	    return (-1)

	if (c2 >= '0' && c2 <= '9')
	    v2 = c2 - '0'
	else if (c2 >= 'a' && c2 <= 'f')
	    v2 = c2 - 'a' + 10
	else if (c2 >= 'A' && c2 <= 'F')
	    v2 = c2 - 'A' + 10
	else
	    return (-1)

	return (v1 * 16 + v2)
end
