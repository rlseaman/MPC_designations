# mpc_designation.h - Header file for MPC Designation Converter
#
# Include this file to use the MPC designation conversion routines.

define	MAX_DES		80		# Maximum designation length
define	MAX_ASTEROID	15396335	# Maximum asteroid number
define	SZ_BASE62	62		# Base-62 character set size

# Format codes returned by mpc_detect
define	FMT_UNKNOWN	0		# Unknown format
define	FMT_PACKED	1		# Packed format
define	FMT_UNPACKED	2		# Unpacked format

# Designation type codes returned by mpc_detect
define	TYPE_PERMANENT	1		# Numbered asteroid
define	TYPE_PROVISIONAL	2		# Provisional asteroid
define	TYPE_SURVEY	3		# Survey designation
define	TYPE_COMET_NUM	4		# Numbered comet
define	TYPE_COMET_PROV	5		# Comet provisional
define	TYPE_COMET_FULL	6		# Full comet designation
define	TYPE_SATELLITE	7		# Natural satellite
define	TYPE_PROV_EXT	8		# Extended provisional (>= 620)
