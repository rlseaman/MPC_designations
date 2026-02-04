# test_csv.x - CSV benchmark test for MPC Designation Converter
#
# Tests conversions against prov_unpack_to_pack.csv and measures timing
#
# The test CSV file path is hardcoded for direct execution.

task	test_csv = t_tstcsv

include	<ctype.h>

define	MAX_DES		80
define	MAX_LINE	256
define	CSV_FILE	"../../test-data/prov_unpack_to_pack.csv"

procedure t_tstcsv ()

char	line[MAX_LINE]
char	unpacked[MAX_DES], expected[MAX_DES], actual[MAX_DES]
int	fd, total, passed, failed, show_fail
int	comma, len, i, j, strlen(), open(), getline()
long	tstart, tend, clktime()
real	elapsed
bool	tcs_streq()

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
	show_fail = 10

	call printf ("=== MPC Designation Conversion Tests (SPP) ===\n\n")

	tstart = clktime (0)

	# Skip header line
	if (getline (fd, line) == EOF) {
	    call close (fd)
	    return
	}

	# Process data lines
	while (getline (fd, line) != EOF) {
	    len = strlen (line)
	    if (len == 0)
		next

	    # Parse CSV: unpacked,expected_packed
	    comma = 0
	    do i = 1, len {
		if (line[i] == ',') {
		    comma = i
		    break
		}
	    }

	    if (comma == 0)
		next

	    # Extract unpacked designation
	    do j = 1, comma - 1 {
		unpacked[j] = line[j]
	    }
	    unpacked[comma] = EOS

	    # Extract expected packed
	    j = 1
	    do i = comma + 1, len {
		expected[j] = line[i]
		j = j + 1
	    }
	    expected[j] = EOS

	    # Trim trailing whitespace from expected
	    j = strlen (expected)
	    while (j > 0 && (expected[j] == ' ' || expected[j] == '\n' ||
			    expected[j] == '\r')) {
		expected[j] = EOS
		j = j - 1
	    }

	    total = total + 1

	    # Convert
	    call mpc_convert (unpacked, actual, MAX_DES)

	    # Compare
	    if (tcs_streq (actual, expected)) {
		passed = passed + 1
	    } else {
		failed = failed + 1
		if (failed <= show_fail) {
		    call printf ("FAIL: pack('%s')\n")
			call pargstr (unpacked)
		    call printf ("      Expected: '%s'\n")
			call pargstr (expected)
		    call printf ("      Got:      '%s'\n")
			call pargstr (actual)
		}
	    }

	    # Progress indicator
	    if (mod (total, 100000) == 0) {
		call printf ("Processed %d entries...\n")
		    call pargi (total)
		call flush (STDOUT)
	    }
	}

	call close (fd)

	tend = clktime (0)
	elapsed = real (tend - tstart)

	call printf ("\n=== Conversion Test Results ===\n")
	call printf ("Total:  %d\n")
	    call pargi (total)
	call printf ("Passed: %d\n")
	    call pargi (passed)
	call printf ("Failed: %d\n")
	    call pargi (failed)
	call printf ("Time:   %0.2f seconds\n")
	    call pargr (elapsed)

	if (failed > show_fail)
	    call printf ("(Showing first %d failures only)\n")
		call pargi (show_fail)

	if (failed == 0)
	    call printf ("All conversion tests passed!\n")
end


# TCS_STREQ -- Compare two strings for equality

bool procedure tcs_streq (s1, s2)

char	s1[ARB], s2[ARB]

int	i

begin
	do i = 1, ARB {
	    if (s1[i] != s2[i])
		return (false)
	    if (s1[i] == EOS)
		return (true)
	}
	return (true)
end
