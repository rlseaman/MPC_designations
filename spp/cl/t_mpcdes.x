# T_MPCDES.X -- IRAF Task for MPC Designation Conversion
#
# This SPP task provides the CL-callable interface for MPC designation
# conversion. It reads parameters from the CL environment and outputs
# results to STDOUT or a file.
#
# Build: xc t_mpcdes.x ../src/mpc_designation.x -o mpcdes.e
#
# Usage from CL:
#   cl> task mpcdes = "path/mpcdes.e"
#   cl> mpcdes "1995 XA"
#   cl> mpcdes "1995 XA" verbose+
#   cl> mpcdes @input.txt output=results.txt

task	mpcdes = t_mpcdes

include	<ctype.h>
include	<fset.h>

define	MAX_DES		80
define	MAX_LINE	256
define	SZ_FNAME	256

procedure t_mpcdes ()

char	input[SZ_FNAME], output[SZ_FNAME]
char	line[MAX_LINE], converted[MAX_DES]
int	fd_in, fd_out, ip
int	strlen(), open(), getline()
bool	verbose, islist, clgetb()

begin
	# Get CL parameters
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)
	verbose = clgetb ("verbose")

	# Check if input is a list file (@filename)
	islist = (input[1] == '@')

	# Determine output destination
	if (output[1] == 'S' && output[2] == 'T' && output[3] == 'D' &&
	    output[4] == 'O' && output[5] == 'U' && output[6] == 'T') {
	    fd_out = STDOUT
	} else {
	    fd_out = open (output, NEW_FILE, TEXT_FILE)
	    if (fd_out == ERR) {
		call eprintf ("Cannot open output file: %s\n")
		    call pargstr (output)
		return
	    }
	}

	if (islist) {
	    # Process list from file
	    fd_in = open (input[2], READ_ONLY, TEXT_FILE)
	    if (fd_in == ERR) {
		call eprintf ("Cannot open input file: %s\n")
		    call pargstr (input[2])
		if (fd_out != STDOUT)
		    call close (fd_out)
		return
	    }

	    while (getline (fd_in, line) != EOF) {
		# Trim trailing whitespace
		ip = strlen (line)
		while (ip > 0 && (line[ip] == '\n' || line[ip] == ' ')) {
		    line[ip] = EOS
		    ip = ip - 1
		}

		if (strlen(line) > 0) {
		    call mpc_convert (line, converted, MAX_DES)

		    if (verbose) {
			call tmd_verbose (fd_out, line, converted)
		    } else {
			if (converted[1] != EOS) {
			    call fprintf (fd_out, "%s\n")
				call pargstr (converted)
			} else {
			    call fprintf (fd_out, "# ERROR: %s\n")
				call pargstr (line)
			}
		    }
		}
	    }

	    call close (fd_in)

	} else {
	    # Single designation
	    call mpc_convert (input, converted, MAX_DES)

	    if (verbose) {
		call tmd_verbose (fd_out, input, converted)
	    } else {
		if (converted[1] != EOS) {
		    call fprintf (fd_out, "%s\n")
			call pargstr (converted)
		} else {
		    call fprintf (fd_out, "# ERROR: %s\n")
			call pargstr (input)
		}
	    }

	    # Store result in output parameter
	    call clpstr ("result", converted)
	}

	if (fd_out != STDOUT)
	    call close (fd_out)
end


# TMD_VERBOSE -- Print verbose output for a conversion

procedure tmd_verbose (fd, input, output)

int	fd
char	input[ARB], output[ARB]

int	format, dtype
char	fmtstr[20], typestr[30]

begin
	call mpc_detect (input, format, dtype)

	# Format string
	if (format == 1) {
	    call strcpy ("packed", fmtstr, 20)
	} else if (format == 2) {
	    call strcpy ("unpacked", fmtstr, 20)
	} else {
	    call strcpy ("unknown", fmtstr, 20)
	}

	# Type string
	switch (dtype) {
	case 1:
	    call strcpy ("permanent asteroid", typestr, 30)
	case 2:
	    call strcpy ("provisional asteroid", typestr, 30)
	case 3:
	    call strcpy ("survey designation", typestr, 30)
	case 4:
	    call strcpy ("numbered comet", typestr, 30)
	case 5:
	    call strcpy ("comet provisional", typestr, 30)
	case 6:
	    call strcpy ("full comet", typestr, 30)
	case 7:
	    call strcpy ("natural satellite", typestr, 30)
	case 8:
	    call strcpy ("extended provisional", typestr, 30)
	case 9:
	    call strcpy ("old-style provisional", typestr, 30)
	default:
	    call strcpy ("unknown", typestr, 30)
	}

	call fprintf (fd, "  Input:    %s\n")
	    call pargstr (input)

	if (output[1] != EOS) {
	    call fprintf (fd, "  Detected: %s format, %s\n")
		call pargstr (fmtstr)
		call pargstr (typestr)

	    if (format == 1) {
		call fprintf (fd, "  Action:   unpacking to readable form\n")
	    } else {
		call fprintf (fd, "  Action:   packing to MPC compact form\n")
	    }

	    call fprintf (fd, "  Output:   %s\n")
		call pargstr (output)
	} else {
	    call fprintf (fd, "  ERROR:    Invalid or unrecognized designation\n")
	}
end
