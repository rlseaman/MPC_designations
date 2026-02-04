# MPCPKG.CL -- MPC Designation Converter Package Definition
#
# This script defines the mpc package for IRAF. Source this file from
# your login.cl or loginuser.cl to make the mpcdes task available.
#
# Installation:
#   1. Build the executable: xc t_mpcdes.x ../src/mpc_designation.x -o mpcdes.e
#   2. Add to login.cl:  cl> cl < path/mpcpkg.cl
#   Or manually:         cl> task mpcdes = "path/mpcdes.e"
#
# Usage:
#   cl> mpcdes "1995 XA"              # Output: J95X00A
#   cl> mpcdes "J95X00A"              # Output: 1995 XA
#   cl> mpcdes "1995 XA" verbose+     # Detailed output
#   cl> mpcdes @designations.txt      # Batch conversion
#   cl> mpcdes "1995 XA" out=out.txt  # Output to file

# Define the task - adjust path as needed
task	mpcdes = "mpcpkg$mpcdes.e"

# Print package info
print ("")
print ("MPC Designation Converter package loaded")
print ("  mpcdes - Convert between packed and unpacked MPC designations")
print ("")
