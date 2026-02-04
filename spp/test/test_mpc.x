# test_mpc.x - Test suite for MPC Designation Converter
#
# Run this from the IRAF cl environment:
#   cl> task test_mpc = path$test/test_mpc.x
#   cl> test_mpc

task	test_mpc = t_testmpc

include	<ctype.h>

define	MAX_DES		80

procedure t_testmpc ()

int	pass, fail, total
char	input[MAX_DES], expected[MAX_DES], result[MAX_DES]

begin
	pass = 0
	fail = 0
	total = 0

	call printf ("MPC Designation Converter - SPP Test Suite\n")
	call printf ("==========================================\n\n")

	# ----- Permanent asteroids -----
	call printf ("Testing permanent asteroids...\n")

	call testit ("00001", "1", pass, fail, total)
	call testit ("00012", "12", pass, fail, total)
	call testit ("12345", "12345", pass, fail, total)
	call testit ("A0000", "100000", pass, fail, total)
	call testit ("Z9999", "359999", pass, fail, total)
	call testit ("a0000", "360000", pass, fail, total)
	call testit ("~0000", "620000", pass, fail, total)

	call testit ("1", "00001", pass, fail, total)
	call testit ("12345", "12345", pass, fail, total)
	call testit ("100000", "A0000", pass, fail, total)
	call testit ("620000", "~0000", pass, fail, total)

	# ----- Provisional designations -----
	call printf ("Testing provisional designations...\n")

	call testit ("J95X00A", "1995 XA", pass, fail, total)
	call testit ("J95X01A", "1995 XA1", pass, fail, total)
	call testit ("K00A00A", "2000 AA", pass, fail, total)
	call testit ("K24KG7S", "2024 KS167", pass, fail, total)

	call testit ("1995 XA", "J95X00A", pass, fail, total)
	call testit ("2000 AA", "K00A00A", pass, fail, total)
	call testit ("2024 GT", "K24G00T", pass, fail, total)

	# ----- Survey designations -----
	call printf ("Testing survey designations...\n")

	call testit ("PLS2040", "2040 P-L", pass, fail, total)
	call testit ("2040 P-L", "PLS2040", pass, fail, total)
	call testit ("T1S2040", "2040 T-1", pass, fail, total)
	call testit ("T2S3138", "3138 T-2", pass, fail, total)

	# ----- Numbered comets -----
	call printf ("Testing numbered comets...\n")

	call testit ("0001P", "1P", pass, fail, total)
	call testit ("1P", "0001P", pass, fail, total)
	call testit ("0354P", "354P", pass, fail, total)
	call testit ("0003D", "3D", pass, fail, total)

	# ----- Comet provisional -----
	call printf ("Testing comet provisional...\n")

	call testit ("CJ95O010", "C/1995 O1", pass, fail, total)
	call testit ("C/1995 O1", "CJ95O010", pass, fail, total)
	call testit ("PK24F010", "P/2024 F1", pass, fail, total)

	# ----- Natural satellites -----
	call printf ("Testing natural satellites...\n")

	call testit ("SK19S220", "S/2019 S 22", pass, fail, total)
	call testit ("S/2019 S 22", "SK19S220", pass, fail, total)
	call testit ("SK00J010", "S/2000 J 1", pass, fail, total)

	# ----- Results -----
	call printf ("\n==========================================\n")
	call printf ("Results: %d passed, %d failed, %d total\n")
	    call pargi (pass)
	    call pargi (fail)
	    call pargi (total)

	if (fail == 0)
	    call printf ("All tests passed!\n")
	else
	    call printf ("Some tests failed.\n")
end


# TESTIT -- Test a single conversion

procedure testit (input, expected, pass, fail, total)

char	input[ARB], expected[ARB]
int	pass, fail, total

char	result[MAX_DES]
bool	streq()

begin
	total = total + 1

	call mpc_convert (input, result, MAX_DES)

	if (streq (result, expected)) {
	    pass = pass + 1
	} else {
	    fail = fail + 1
	    call printf ("FAIL: '%s' -> '%s' (expected '%s')\n")
		call pargstr (input)
		call pargstr (result)
		call pargstr (expected)
	}
end
