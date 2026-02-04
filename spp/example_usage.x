# example_usage.x - Example usage of MPC Designation Converter for IRAF
#
# This file demonstrates how to use the MPC designation conversion
# routines in an IRAF/SPP task.

task	mpcdes = t_mpcdes

include "src/mpc_designation.x"

define	MAX_DES		80

# T_MPCDES -- Interactive MPC designation converter
#
# Usage: mpcdes
# Then enter designations at the prompt, 'q' to quit.

procedure t_mpcdes ()

char	input[MAX_DES], output[MAX_DES]
int	format, dtype

begin
	call printf ("MPC Designation Converter\n")
	call printf ("Enter a designation to convert, 'q' to quit.\n\n")

	repeat {
	    call printf ("des> ")
	    call flush (STDOUT)

	    call getline (STDIN, input)
	    call mpc_strtrim (input, input, MAX_DES)

	    if (input[1] == 'q' && input[2] == EOS)
		break

	    if (input[1] == EOS)
		next

	    # Detect format first
	    call mpc_detect (input, format, dtype)

	    if (format == 0) {
		call printf ("Unknown format: %s\n")
		    call pargstr (input)
		next
	    }

	    # Convert
	    call mpc_convert (input, output, MAX_DES)

	    # Display result
	    if (format == 1) {
		call printf ("Packed '%s' -> Unpacked '%s'\n")
		    call pargstr (input)
		    call pargstr (output)
	    } else {
		call printf ("Unpacked '%s' -> Packed '%s'\n")
		    call pargstr (input)
		    call pargstr (output)
	    }
	}

	call printf ("Goodbye.\n")
end

# Additional example: batch conversion from a file
#
# task	mpcbatch = t_mpcbatch
#
# procedure t_mpcbatch ()
#
# char	input[MAX_DES], output[MAX_DES]
# char	infile[SZ_FNAME], outfile[SZ_FNAME]
# int	fd_in, fd_out
#
# begin
#     call clgstr ("input", infile, SZ_FNAME)
#     call clgstr ("output", outfile, SZ_FNAME)
#
#     fd_in = open (infile, READ_ONLY, TEXT_FILE)
#     fd_out = open (outfile, NEW_FILE, TEXT_FILE)
#
#     while (getline (fd_in, input) != EOF) {
#         call mpc_strtrim (input, input, MAX_DES)
#         if (input[1] != EOS) {
#             call mpc_convert (input, output, MAX_DES)
#             call fprintf (fd_out, "%s\n")
#                 call pargstr (output)
#         }
#     }
#
#     call close (fd_in)
#     call close (fd_out)
# end
