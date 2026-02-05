# MPC Designation Helper Function Tests - SPP/IRAF Implementation
# Tests the 6 helper functions with 77 test cases

task	test_helpers = t_hlptest

include	<ctype.h>

define	SZ_LINE		80

procedure t_hlptest ()

int	passed, failed
char	result[SZ_LINE], frag[10], parent[SZ_LINE]
bool	bresult, mpc_hasfrag(), mpc_deseq()

begin
	passed = 0
	failed = 0

	call printf ("=== MPC Designation Helper Function Tests (SPP) ===\n")
	call printf ("\n")

	# Test mpc_torep (to_report_format)
	call printf ("--- mpc_torep ---\n")

	# Numbered asteroids
	call mpc_torep ("00001", result, SZ_LINE)
	call test_str (result, "       00001", "Numbered asteroid 1", passed, failed)
	call mpc_torep ("00433", result, SZ_LINE)
	call test_str (result, "       00433", "Numbered asteroid 433", passed, failed)
	call mpc_torep ("99999", result, SZ_LINE)
	call test_str (result, "       99999", "Numbered asteroid 99999", passed, failed)
	call mpc_torep ("A0000", result, SZ_LINE)
	call test_str (result, "       A0000", "Numbered asteroid 100000", passed, failed)
	call mpc_torep ("~0000", result, SZ_LINE)
	call test_str (result, "       ~0000", "Numbered asteroid 620000", passed, failed)

	# Provisional asteroids
	call mpc_torep ("J95X00A", result, SZ_LINE)
	call test_str (result, "     J95X00A", "Provisional 1995 XA", passed, failed)
	call mpc_torep ("K24A12B", result, SZ_LINE)
	call test_str (result, "     K24A12B", "Provisional 2024 AB12", passed, failed)

	# Survey designations
	call mpc_torep ("PLS2040", result, SZ_LINE)
	call test_str (result, "     PLS2040", "Survey P-L", passed, failed)
	call mpc_torep ("T3S3141", result, SZ_LINE)
	call test_str (result, "     T3S3141", "Survey T-3", passed, failed)

	# Numbered comets
	call mpc_torep ("0001P", result, SZ_LINE)
	call test_str (result, "0001P       ", "Comet 1P", passed, failed)
	call mpc_torep ("0073P", result, SZ_LINE)
	call test_str (result, "0073P       ", "Comet 73P", passed, failed)

	# Numbered comets with fragments
	call mpc_torep ("0073Pa", result, SZ_LINE)
	call test_str (result, "0073P      a", "Comet 73P-A", passed, failed)
	call mpc_torep ("0073Pb", result, SZ_LINE)
	call test_str (result, "0073P      b", "Comet 73P-B", passed, failed)
	call mpc_torep ("0073Paa", result, SZ_LINE)
	call test_str (result, "0073P     aa", "Comet 73P-AA", passed, failed)
	call mpc_torep ("0073Paz", result, SZ_LINE)
	call test_str (result, "0073P     az", "Comet 73P-AZ", passed, failed)
	call mpc_torep ("0073Pzz", result, SZ_LINE)
	call test_str (result, "0073P     zz", "Comet 73P-ZZ", passed, failed)

	# Provisional comets
	call mpc_torep ("CJ95O010", result, SZ_LINE)
	call test_str (result, "    CJ95O010", "Comet C/1995 O1", passed, failed)
	call mpc_torep ("DJ93F020", result, SZ_LINE)
	call test_str (result, "    DJ93F020", "Comet D/1993 F2", passed, failed)
	call mpc_torep ("DJ93F02a", result, SZ_LINE)
	call test_str (result, "    DJ93F02a", "Comet D/1993 F2-A", passed, failed)

	# Test mpc_fromrep (from_report_format)
	call printf ("\n--- mpc_fromrep ---\n")

	# Numbered asteroids
	call mpc_fromrep ("       00001", result, SZ_LINE)
	call test_str (result, "00001", "Numbered asteroid 1", passed, failed)
	call mpc_fromrep ("       00433", result, SZ_LINE)
	call test_str (result, "00433", "Numbered asteroid 433", passed, failed)
	call mpc_fromrep ("       A0000", result, SZ_LINE)
	call test_str (result, "A0000", "Numbered asteroid 100000", passed, failed)

	# Provisional asteroids
	call mpc_fromrep ("     J95X00A", result, SZ_LINE)
	call test_str (result, "J95X00A", "Provisional 1995 XA", passed, failed)

	# Numbered comets
	call mpc_fromrep ("0073P       ", result, SZ_LINE)
	call test_str (result, "0073P", "Comet 73P", passed, failed)

	# Numbered comets with fragments
	call mpc_fromrep ("0073P      a", result, SZ_LINE)
	call test_str (result, "0073Pa", "Comet 73P-A", passed, failed)
	call mpc_fromrep ("0073P     aa", result, SZ_LINE)
	call test_str (result, "0073Paa", "Comet 73P-AA", passed, failed)
	call mpc_fromrep ("0073P     az", result, SZ_LINE)
	call test_str (result, "0073Paz", "Comet 73P-AZ", passed, failed)

	# Provisional comets
	call mpc_fromrep ("    CJ95O010", result, SZ_LINE)
	call test_str (result, "CJ95O010", "Comet C/1995 O1", passed, failed)

	# Test mpc_hasfrag (has_fragment)
	call printf ("\n--- mpc_hasfrag ---\n")

	# Unpacked with fragments
	bresult = mpc_hasfrag ("73P-A")
	call test_bool (bresult, true, "Unpacked numbered comet with fragment", passed, failed)
	bresult = mpc_hasfrag ("73P-AA")
	call test_bool (bresult, true, "Unpacked numbered comet with 2-letter fragment", passed, failed)
	bresult = mpc_hasfrag ("D/1993 F2-A")
	call test_bool (bresult, true, "Unpacked provisional comet with fragment", passed, failed)
	bresult = mpc_hasfrag ("P/1930 J1-AA")
	call test_bool (bresult, true, "Unpacked provisional comet with 2-letter fragment", passed, failed)

	# Unpacked without fragments
	bresult = mpc_hasfrag ("73P")
	call test_bool (bresult, false, "Unpacked numbered comet no fragment", passed, failed)
	bresult = mpc_hasfrag ("C/1995 O1")
	call test_bool (bresult, false, "Unpacked provisional comet no fragment", passed, failed)

	# Packed with fragments
	bresult = mpc_hasfrag ("0073Pa")
	call test_bool (bresult, true, "Packed numbered comet with fragment", passed, failed)
	bresult = mpc_hasfrag ("0073Paa")
	call test_bool (bresult, true, "Packed numbered comet with 2-letter fragment", passed, failed)
	bresult = mpc_hasfrag ("DJ93F02a")
	call test_bool (bresult, true, "Packed provisional comet with fragment", passed, failed)

	# Packed without fragments
	bresult = mpc_hasfrag ("0073P")
	call test_bool (bresult, false, "Packed numbered comet no fragment", passed, failed)
	bresult = mpc_hasfrag ("CJ95O010")
	call test_bool (bresult, false, "Packed provisional comet no fragment", passed, failed)

	# Non-comets
	bresult = mpc_hasfrag ("1995 XA")
	call test_bool (bresult, false, "Asteroid no fragment", passed, failed)
	bresult = mpc_hasfrag ("00001")
	call test_bool (bresult, false, "Numbered asteroid", passed, failed)

	# Test mpc_getfrag (get_fragment)
	call printf ("\n--- mpc_getfrag ---\n")

	# Unpacked with fragments
	call mpc_getfrag ("73P-A", frag, 10)
	call test_str (frag, "A", "Unpacked single fragment", passed, failed)
	call mpc_getfrag ("73P-AA", frag, 10)
	call test_str (frag, "AA", "Unpacked 2-letter fragment", passed, failed)
	call mpc_getfrag ("73P-I", frag, 10)
	call test_str (frag, "I", "Unpacked fragment I", passed, failed)
	call mpc_getfrag ("D/1993 F2-B", frag, 10)
	call test_str (frag, "B", "Unpacked provisional fragment", passed, failed)
	call mpc_getfrag ("P/1930 J1-AZ", frag, 10)
	call test_str (frag, "AZ", "Unpacked provisional 2-letter", passed, failed)

	# Unpacked without fragments
	call mpc_getfrag ("73P", frag, 10)
	call test_str (frag, "", "Unpacked no fragment", passed, failed)
	call mpc_getfrag ("C/1995 O1", frag, 10)
	call test_str (frag, "", "Unpacked provisional no fragment", passed, failed)

	# Packed with fragments
	call mpc_getfrag ("0073Pa", frag, 10)
	call test_str (frag, "A", "Packed single fragment", passed, failed)
	call mpc_getfrag ("0073Paa", frag, 10)
	call test_str (frag, "AA", "Packed 2-letter fragment", passed, failed)
	call mpc_getfrag ("0073Pi", frag, 10)
	call test_str (frag, "I", "Packed fragment I", passed, failed)
	call mpc_getfrag ("DJ93F02b", frag, 10)
	call test_str (frag, "B", "Packed provisional fragment", passed, failed)

	# Packed without fragments
	call mpc_getfrag ("0073P", frag, 10)
	call test_str (frag, "", "Packed no fragment", passed, failed)
	call mpc_getfrag ("CJ95O010", frag, 10)
	call test_str (frag, "", "Packed provisional no fragment", passed, failed)

	# Test mpc_getpar (get_parent)
	call printf ("\n--- mpc_getpar ---\n")

	# Unpacked with fragments
	call mpc_getpar ("73P-A", parent, SZ_LINE)
	call test_str (parent, "73P", "Unpacked single fragment", passed, failed)
	call mpc_getpar ("73P-AA", parent, SZ_LINE)
	call test_str (parent, "73P", "Unpacked 2-letter fragment", passed, failed)
	call mpc_getpar ("D/1993 F2-B", parent, SZ_LINE)
	call test_str (parent, "D/1993 F2", "Unpacked provisional fragment", passed, failed)
	call mpc_getpar ("P/1930 J1-AA", parent, SZ_LINE)
	call test_str (parent, "P/1930 J1", "Unpacked provisional 2-letter", passed, failed)

	# Unpacked without fragments
	call mpc_getpar ("73P", parent, SZ_LINE)
	call test_str (parent, "73P", "Unpacked no fragment", passed, failed)
	call mpc_getpar ("C/1995 O1", parent, SZ_LINE)
	call test_str (parent, "C/1995 O1", "Unpacked provisional no fragment", passed, failed)

	# Packed with fragments
	call mpc_getpar ("0073Pa", parent, SZ_LINE)
	call test_str (parent, "0073P", "Packed single fragment", passed, failed)
	call mpc_getpar ("0073Paa", parent, SZ_LINE)
	call test_str (parent, "0073P", "Packed 2-letter fragment", passed, failed)

	# Packed without fragments
	call mpc_getpar ("0073P", parent, SZ_LINE)
	call test_str (parent, "0073P", "Packed no fragment", passed, failed)

	# Non-comets (should return as-is)
	call mpc_getpar ("1995 XA", parent, SZ_LINE)
	call test_str (parent, "1995 XA", "Asteroid", passed, failed)
	call mpc_getpar ("00001", parent, SZ_LINE)
	call test_str (parent, "00001", "Numbered asteroid", passed, failed)

	# Test mpc_deseq (designations_equal)
	call printf ("\n--- mpc_deseq ---\n")

	# Same designation, different formats
	bresult = mpc_deseq ("1995 XA", "J95X00A")
	call test_bool (bresult, true, "Provisional packed/unpacked", passed, failed)
	bresult = mpc_deseq ("73P", "0073P")
	call test_bool (bresult, true, "Numbered comet packed/unpacked", passed, failed)
	bresult = mpc_deseq ("73P-A", "0073Pa")
	call test_bool (bresult, true, "Comet with fragment packed/unpacked", passed, failed)
	bresult = mpc_deseq ("73P-AA", "0073Paa")
	call test_bool (bresult, true, "Comet with 2-letter fragment", passed, failed)
	bresult = mpc_deseq ("1", "00001")
	call test_bool (bresult, true, "Numbered asteroid", passed, failed)
	bresult = mpc_deseq ("C/1995 O1", "CJ95O010")
	call test_bool (bresult, true, "Provisional comet", passed, failed)

	# Different designations
	bresult = mpc_deseq ("1995 XA", "1995 XB")
	call test_bool (bresult, false, "Different provisional", passed, failed)
	bresult = mpc_deseq ("73P-A", "73P-B")
	call test_bool (bresult, false, "Different fragments", passed, failed)
	bresult = mpc_deseq ("73P", "74P")
	call test_bool (bresult, false, "Different comet numbers", passed, failed)
	bresult = mpc_deseq ("1", "2")
	call test_bool (bresult, false, "Different asteroid numbers", passed, failed)

	# Same designation (both packed or both unpacked)
	bresult = mpc_deseq ("1995 XA", "1995 XA")
	call test_bool (bresult, true, "Same unpacked", passed, failed)
	bresult = mpc_deseq ("J95X00A", "J95X00A")
	call test_bool (bresult, true, "Same packed", passed, failed)

	# Summary
	call printf ("\n==================================================\n")
	call printf ("Total: %d, Passed: %d, Failed: %d\n")
	    call pargi (passed + failed)
	    call pargi (passed)
	    call pargi (failed)

	if (failed > 0)
	    call error (1, "Some tests failed")
end


# Helper procedure to test string equality
procedure test_str (result, expected, desc, passed, failed)

char	result[ARB], expected[ARB], desc[ARB]
int	passed, failed

int	i, len1, len2, strlen()
bool	match

begin
	len1 = strlen (result)
	len2 = strlen (expected)
	match = true

	if (len1 != len2) {
	    match = false
	} else {
	    do i = 1, len1 {
		if (result[i] != expected[i])
		    match = false
	    }
	}

	if (match) {
	    call printf ("  PASS: \"%s\"\n")
		call pargstr (result)
	    passed = passed + 1
	} else {
	    call printf ("  FAIL: expected \"%s\", got \"%s\"\n")
		call pargstr (expected)
		call pargstr (result)
	    failed = failed + 1
	}
end


# Helper procedure to test boolean equality
procedure test_bool (result, expected, desc, passed, failed)

bool	result, expected
char	desc[ARB]
int	passed, failed

begin
	if (result == expected) {
	    call printf ("  PASS: %s -> %b\n")
		call pargstr (desc)
		call pargb (result)
	    passed = passed + 1
	} else {
	    call printf ("  FAIL: %s -> expected %b, got %b\n")
		call pargstr (desc)
		call pargb (expected)
		call pargb (result)
	    failed = failed + 1
	}
end
