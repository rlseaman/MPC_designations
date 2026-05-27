# error_runner.awk - helper for test_errors.sh (AWK error harness)
#
# Loaded after mpc_designation.awk via a second -f.  Reads the exact input
# designation from the DESIG environment variable (so embedded whitespace and
# control characters survive intact, unlike a newline-delimited stdin record),
# runs the library converter, and prints a single status line:
#
#   REJECT          - the converter rejected the input
#   OK              - the converter produced a valid result
#
# The AWK library signals failure by returning an empty string or a string
# beginning with "ERROR".  Anything else is treated as a successful conversion.

BEGIN {
    result = convert_simple(ENVIRON["DESIG"])
    if (result == "" || substr(result, 1, 5) == "ERROR") {
        print "REJECT"
    } else {
        print "OK"
    }
}
