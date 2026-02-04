# MPC Designation Converter for IRAF/SPP
#
# Convert between packed and unpacked Minor Planet Center (MPC) designations
# for asteroids, comets, and natural satellites.
#
# Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html

include	<ctype.h>

# Constants
define	MAX_DES		80
define	MAX_ASTEROID	15396335

# Macro for case conversion
define	TO_UPPER	($1-32)
define	TO_LOWER	($1+32)


#---------------------------------------------------------------------------
# MPC_CONVERT -- Convert a designation between packed and unpacked formats

procedure mpc_convert (input, output, maxch)

char	input[ARB], output[ARB]
int	maxch

char	trimmed[MAX_DES]
int	len, format, dtype, strlen()
bool	valid, mpc_valid()

begin
	call mpc_strtrim (input, trimmed, MAX_DES)
	len = strlen (trimmed)

	if (len == 0) {
	    output[1] = EOS
	    return
	}

	# Validate input for invalid characters and whitespace issues
	valid = mpc_valid (input, trimmed)
	if (!valid) {
	    output[1] = EOS
	    return
	}

	call mpc_detect (trimmed, format, dtype)

	if (format == 0) {
	    output[1] = EOS
	    return
	}

	if (format == 1)
	    call mpc_unpack (trimmed, dtype, output, maxch)
	else
	    call mpc_pack (trimmed, dtype, output, maxch)
end


#---------------------------------------------------------------------------
# MPC_VALID -- Validate input for invalid characters and whitespace issues
#
# Returns false if input contains:
# - Tab characters (leading/trailing)
# - Non-printable ASCII (< 32 or > 126, except null terminator)
# - Consecutive spaces
#
# Note: Embedded null bytes cannot be reliably detected in null-terminated
# strings since the null terminates the string before we can scan past it.

bool procedure mpc_valid (orig, trimmed)

char	orig[ARB], trimmed[ARB]

int	i, origlen, trimlen, strlen()
char	c

begin
	origlen = strlen (orig)

	# Check for tabs and control characters in original (before trim)
	do i = 1, origlen {
	    c = orig[i]
	    if (c == 9)		# tab
		return (false)
	    if (c < 32)  # control characters
		return (false)
	    if (c > 126)	# non-ASCII
		return (false)
	}

	# Check trimmed string for double spaces
	trimlen = strlen (trimmed)
	do i = 1, trimlen - 1 {
	    if (trimmed[i] == ' ' && trimmed[i+1] == ' ')  # double space
		return (false)
	}

	return (true)
end


#---------------------------------------------------------------------------
# MPC_DETECT -- Detect format and type of a designation
#
# format: 0=unknown, 1=packed, 2=unpacked
# dtype: 1=permanent, 2=provisional, 3=survey, 4=comet_num,
#        5=comet_prov, 6=comet_full, 7=satellite, 8=prov_ext

procedure mpc_detect (des, format, dtype)

char	des[ARB]
int	format, dtype

int	len, i, strlen(), mpc_strchr(), mpc_strstr()
char	first, space
bool	alldig, mpc_iscent()

begin
	format = 0
	dtype = 0
	space = ' '
	len = strlen (des)

	if (len == 0)
	    return

	first = des[1]

	# Packed permanent (5 chars)
	if (len == 5) {
	    if (first == '~') {
		format = 1
		dtype = 1
		return
	    }

	    alldig = true
	    do i = 1, 5 {
		if (!IS_DIGIT(des[i]))
		    alldig = false
	    }
	    if (alldig) {
		format = 1
		dtype = 1
		return
	    }

	    if ((IS_UPPER(first) || IS_LOWER(first)) && first != '~') {
		alldig = true
		do i = 2, 5 {
		    if (!IS_DIGIT(des[i]))
			alldig = false
		}
		if (alldig) {
		    format = 1
		    dtype = 1
		    return
		}
	    }

	    alldig = true
	    do i = 1, 4 {
		if (!IS_DIGIT(des[i]))
		    alldig = false
	    }
	    if (alldig && (des[5] == 'P' || des[5] == 'D')) {
		format = 1
		dtype = 4
		return
	    }
	}

	# Packed provisional (7 chars)
	if (len == 7) {
	    if (first == '_') {
		format = 1
		dtype = 8
		return
	    }

	    if ((des[1]=='P' && des[2]=='L' && des[3]=='S') ||
		(des[1]=='T' && des[3]=='S' &&
		 (des[2]=='1' || des[2]=='2' || des[2]=='3'))) {
		format = 1
		dtype = 3
		return
	    }

	    # Exclude old-style pattern (space at position 5)
	    if (mpc_iscent(first) && IS_UPPER(des[7]) && des[5] != ' ') {
		format = 1
		dtype = 2
		return
	    }

	    if (mpc_iscent(first) && (IS_DIGIT(des[7]) || IS_LOWER(des[7])) && des[5] != ' ') {
		format = 1
		dtype = 5
		return
	    }
	}

	# Packed satellite (8 chars starting with S)
	if (len == 8 && first == 'S' && mpc_strchr(des, space) == 0) {
	    format = 1
	    dtype = 7
	    return
	}

	# Packed comet full (8-9 chars)
	if ((len == 8 || len == 9) && mpc_strchr(des, space) == 0) {
	    if (first=='P' || first=='C' || first=='D' ||
		first=='X' || first=='A' || first=='I') {
		format = 1
		dtype = 6
		return
	    }
	}

	# Unpacked permanent (all digits)
	alldig = true
	do i = 1, len {
	    if (!IS_DIGIT(des[i]))
		alldig = false
	}
	if (alldig) {
	    format = 2
	    dtype = 1
	    return
	}

	# Unpacked satellite
	if (len >= 10 && des[1] == 'S' && des[2] == '/') {
	    format = 2
	    dtype = 7
	    return
	}

	# Unpacked numbered comet
	if (len >= 2 && len <= 5) {
	    alldig = true
	    do i = 1, len - 1 {
		if (!IS_DIGIT(des[i]))
		    alldig = false
	    }
	    if (alldig && (des[len] == 'P' || des[len] == 'D')) {
		format = 2
		dtype = 4
		return
	    }
	}

	# Unpacked comet with prefix
	if (len >= 7 && des[2] == '/') {
	    if (first=='P' || first=='C' || first=='D' ||
		first=='X' || first=='A' || first=='I') {
		format = 2
		dtype = 6
		return
	    }
	}

	# Unpacked survey
	if (mpc_strstr(des, " P-L") > 0 || mpc_strstr(des, " T-") > 0) {
	    format = 2
	    dtype = 3
	    return
	}

	# Unpacked old-style provisional (A908 CJ format)
	# Pattern: [AB]DDD SS where D=digit, S=uppercase letter
	if (len == 7) {
	    if ((des[1] == 'A' || des[1] == 'B') &&
		IS_DIGIT(des[2]) && IS_DIGIT(des[3]) && IS_DIGIT(des[4]) &&
		des[5] == ' ' &&
		IS_UPPER(des[6]) && IS_UPPER(des[7])) {
		format = 2
		dtype = 9    # old-style provisional
		return
	    }
	}

	# Unpacked provisional
	if (len >= 7) {
	    alldig = true
	    do i = 1, 4 {
		if (!IS_DIGIT(des[i]))
		    alldig = false
	    }
	    if (alldig && des[5] == ' ' && IS_UPPER(des[6])) {
		format = 2
		dtype = 2
		return
	    }
	}
end


#---------------------------------------------------------------------------
# MPC_UNPACK -- Unpack a packed designation

procedure mpc_unpack (packed, dtype, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	dtype, maxch

begin
	switch (dtype) {
	case 1:
	    call mpc_unperm (packed, unpacked, maxch)
	case 2:
	    call mpc_unprov (packed, unpacked, maxch)
	case 3:
	    call mpc_unsurv (packed, unpacked, maxch)
	case 4:
	    call mpc_uncnum (packed, unpacked, maxch)
	case 5:
	    call mpc_uncprv (packed, unpacked, maxch)
	case 6:
	    call mpc_uncful (packed, unpacked, maxch)
	case 7:
	    call mpc_unsat (packed, unpacked, maxch)
	case 8:
	    call mpc_unext (packed, unpacked, maxch)
	default:
	    unpacked[1] = EOS
	}
end


#---------------------------------------------------------------------------
# MPC_PACK -- Pack an unpacked designation

procedure mpc_pack (unpacked, dtype, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	dtype, maxch

begin
	switch (dtype) {
	case 1:
	    call mpc_pkperm (unpacked, packed, maxch)
	case 2:
	    call mpc_pkprov (unpacked, packed, maxch)
	case 3:
	    call mpc_pksurv (unpacked, packed, maxch)
	case 4:
	    call mpc_pkcnum (unpacked, packed, maxch)
	case 5:
	    packed[1] = EOS
	case 6:
	    call mpc_pkcful (unpacked, packed, maxch)
	case 7:
	    call mpc_pksat (unpacked, packed, maxch)
	case 9:
	    call mpc_pkold (unpacked, packed, maxch)
	default:
	    packed[1] = EOS
	}
end


#---------------------------------------------------------------------------
# MPC_UNPERM -- Unpack a numbered asteroid

procedure mpc_unperm (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	number, val, rest, mpc_b62d4()
char	first
bool	mpc_isb62()

begin
	first = packed[1]

	if (first == '~') {
	    # Validate base-62 characters
	    if (!mpc_isb62(packed[2]) || !mpc_isb62(packed[3]) ||
		!mpc_isb62(packed[4]) || !mpc_isb62(packed[5])) {
		unpacked[1] = EOS
		return
	    }
	    val = mpc_b62d4 (packed, 2)
	    number = 620000 + val
	    # Validate number doesn't exceed maximum
	    if (number > MAX_ASTEROID) {
		unpacked[1] = EOS
		return
	    }
	    call sprintf (unpacked, maxch, "%d")
		call pargi (number)
	    return
	}

	if (IS_DIGIT(first)) {
	    number = (packed[1]-'0')*10000 + (packed[2]-'0')*1000 +
		     (packed[3]-'0')*100 + (packed[4]-'0')*10 + (packed[5]-'0')
	    call sprintf (unpacked, maxch, "%d")
		call pargi (number)
	    return
	}

	if (IS_UPPER(first)) {
	    val = first - 'A' + 10
	    rest = (packed[2]-'0')*1000 + (packed[3]-'0')*100 +
		   (packed[4]-'0')*10 + (packed[5]-'0')
	    number = val * 10000 + rest
	    call sprintf (unpacked, maxch, "%d")
		call pargi (number)
	    return
	}

	if (IS_LOWER(first)) {
	    val = first - 'a' + 36
	    rest = (packed[2]-'0')*1000 + (packed[3]-'0')*100 +
		   (packed[4]-'0')*10 + (packed[5]-'0')
	    number = val * 10000 + rest
	    call sprintf (unpacked, maxch, "%d")
		call pargi (number)
	    return
	}

	unpacked[1] = EOS
end


#---------------------------------------------------------------------------
# MPC_PKPERM -- Pack a numbered asteroid

procedure mpc_pkperm (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

int	number, div, imod, i
char	letter

begin
	number = 0
	i = 1
	while (IS_DIGIT(unpacked[i])) {
	    number = number * 10 + (unpacked[i] - '0')
	    i = i + 1
	}

	if (number < 1 || number > MAX_ASTEROID) {
	    packed[1] = EOS
	    return
	}

	if (number < 100000) {
	    call sprintf (packed, maxch, "%05d")
		call pargi (number)
	} else if (number < 620000) {
	    div = number / 10000
	    imod = mod (number, 10000)
	    if (div < 36)
		letter = 'A' + div - 10
	    else
		letter = 'a' + div - 36
	    call sprintf (packed, maxch, "%c%04d")
		call pargc (letter)
		call pargi (imod)
	} else {
	    packed[1] = '~'
	    call mpc_b62e4 (number - 620000, packed, 2)
	    packed[6] = EOS
	}
end


#---------------------------------------------------------------------------
# MPC_UNPROV -- Unpack a provisional asteroid

procedure mpc_unprov (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	year, order, mpc_centc(), mpc_deccyc()
bool	mpc_iscent(), mpc_ishalfmo(), mpc_isseclet(), mpc_isb62()

begin
	# Validate packed format characters
	# Position 1: century code (A-L)
	if (!mpc_iscent (packed[1])) {
	    unpacked[1] = EOS
	    return
	}
	# Position 2-3: digits
	if (!IS_DIGIT(packed[2]) || !IS_DIGIT(packed[3])) {
	    unpacked[1] = EOS
	    return
	}
	# Position 4: half-month letter (A-Y excluding I)
	if (!mpc_ishalfmo (packed[4])) {
	    unpacked[1] = EOS
	    return
	}
	# Position 5: base-62 char (first char of cycle)
	if (!mpc_isb62 (packed[5])) {
	    unpacked[1] = EOS
	    return
	}
	# Position 6: digit (second char of cycle)
	if (!IS_DIGIT(packed[6])) {
	    unpacked[1] = EOS
	    return
	}
	# Position 7: second letter (A-Z excluding I)
	if (!mpc_isseclet (packed[7])) {
	    unpacked[1] = EOS
	    return
	}

	year = mpc_centc (packed[1]) + (packed[2] - '0') * 10 + (packed[3] - '0')
	order = mpc_deccyc (packed, 5)

	if (order == 0) {
	    call sprintf (unpacked, maxch, "%d %c%c")
		call pargi (year)
		call pargc (packed[4])
		call pargc (packed[7])
	} else {
	    call sprintf (unpacked, maxch, "%d %c%c%d")
		call pargi (year)
		call pargc (packed[4])
		call pargc (packed[7])
		call pargi (order)
	}
end


#---------------------------------------------------------------------------
# MPC_PKPROV -- Pack a provisional asteroid

procedure mpc_pkprov (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

int	year, century, order, i, strlen()
char	centcode, halfmo, seclet, mpc_centl()
bool	mpc_ishalfmo(), mpc_isseclet()

begin
	# Validate half-month letter
	halfmo = unpacked[6]
	if (!mpc_ishalfmo (halfmo)) {
	    packed[1] = EOS
	    return
	}

	# Validate second letter
	seclet = unpacked[7]
	if (!mpc_isseclet (seclet)) {
	    packed[1] = EOS
	    return
	}

	# Check for mixed case (second letter must be uppercase, handled above)
	# Check for three consecutive letters (invalid)
	i = 8
	if (IS_UPPER(unpacked[i]) && !IS_DIGIT(unpacked[i])) {
	    packed[1] = EOS
	    return
	}

	year = (unpacked[1]-'0')*1000 + (unpacked[2]-'0')*100 +
	       (unpacked[3]-'0')*10 + (unpacked[4]-'0')
	century = year / 100
	centcode = mpc_centl (century)

	# Validate century code
	if (centcode == '?') {
	    packed[1] = EOS
	    return
	}

	order = 0
	i = 8
	while (IS_DIGIT(unpacked[i])) {
	    # Check for integer overflow (max extended cycle is about 600000)
	    if (order > 600000) {
		packed[1] = EOS
		return
	    }
	    order = order * 10 + (unpacked[i] - '0')
	    i = i + 1
	}

	# Check for cycle overflow (max 619 for standard format)
	if (order >= 620) {
	    # Extended format: max cycle is 620 + (62^4 - 1) / 25 ~ 591535
	    # But practical max is around 14 million total entries
	    # For safety, reject cycle > 591535
	    if (order > 591535) {
		packed[1] = EOS
		return
	    }
	    call mpc_pkext (year, unpacked[6], unpacked[7], order, packed, maxch)
	    return
	}

	packed[1] = centcode
	packed[2] = unpacked[3]
	packed[3] = unpacked[4]
	packed[4] = unpacked[6]
	call mpc_enccyc (order, packed, 5)
	packed[7] = unpacked[7]
	packed[8] = EOS
end


#---------------------------------------------------------------------------
# MPC_UNEXT -- Unpack extended provisional

procedure mpc_unext (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	year, baseseq, cycle, letpos, mpc_b62c(), mpc_b62d4()
char	seclet, mpc_p2l()

begin
	year = 2000 + mpc_b62c (packed[2])
	baseseq = mpc_b62d4 (packed, 4)
	cycle = 620 + baseseq / 25
	letpos = mod (baseseq, 25) + 1
	seclet = mpc_p2l (letpos)

	call sprintf (unpacked, maxch, "%d %c%c%d")
	    call pargi (year)
	    call pargc (packed[3])
	    call pargc (seclet)
	    call pargi (cycle)
end


#---------------------------------------------------------------------------
# MPC_PKEXT -- Pack extended provisional

procedure mpc_pkext (year, halfmo, seclet, cycle, packed, maxch)

int	year, cycle, maxch
char	halfmo, seclet, packed[ARB]

int	yrshort, letpos, baseseq, mpc_l2p()
char	mpc_n2b62()

begin
	yrshort = mod (year, 100)
	letpos = mpc_l2p (seclet)
	baseseq = (cycle - 620) * 25 + letpos - 1

	packed[1] = '_'
	packed[2] = mpc_n2b62 (yrshort)
	packed[3] = halfmo
	call mpc_b62e4 (baseseq, packed, 4)
	packed[8] = EOS
end


#---------------------------------------------------------------------------
# MPC_UNSURV -- Unpack survey designation

procedure mpc_unsurv (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	num

begin
	num = (packed[4]-'0')*1000 + (packed[5]-'0')*100 +
	      (packed[6]-'0')*10 + (packed[7]-'0')

	if (packed[1] == 'P') {
	    call sprintf (unpacked, maxch, "%d P-L")
		call pargi (num)
	} else if (packed[2] == '1') {
	    call sprintf (unpacked, maxch, "%d T-1")
		call pargi (num)
	} else if (packed[2] == '2') {
	    call sprintf (unpacked, maxch, "%d T-2")
		call pargi (num)
	} else {
	    call sprintf (unpacked, maxch, "%d T-3")
		call pargi (num)
	}
end


#---------------------------------------------------------------------------
# MPC_PKSURV -- Pack survey designation

procedure mpc_pksurv (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

int	num, i, mpc_strstr()

begin
	num = 0
	i = 1
	while (IS_DIGIT(unpacked[i])) {
	    num = num * 10 + (unpacked[i] - '0')
	    i = i + 1
	}

	# Validate survey number > 0
	if (num <= 0) {
	    packed[1] = EOS
	    return
	}

	if (mpc_strstr(unpacked, "P-L") > 0) {
	    call sprintf (packed, maxch, "PLS%04d")
		call pargi (num)
	} else if (mpc_strstr(unpacked, "T-1") > 0) {
	    call sprintf (packed, maxch, "T1S%04d")
		call pargi (num)
	} else if (mpc_strstr(unpacked, "T-2") > 0) {
	    call sprintf (packed, maxch, "T2S%04d")
		call pargi (num)
	} else if (mpc_strstr(unpacked, "T-3") > 0) {
	    call sprintf (packed, maxch, "T3S%04d")
		call pargi (num)
	} else {
	    packed[1] = EOS
	}
end


#---------------------------------------------------------------------------
# MPC_PKOLD -- Pack old-style provisional designation (A908 CJ format)
#
# Old-style format: [AB]YYY HH where:
#   A/B = prefix (both map to same century based on first digit)
#   Y = century digit (8=1800s, 9=1900s, 0=2000s)
#   YY = year within century
#   H = half-month letter
#   H = secondary letter
#
# Packed format: CYYHM00S where:
#   C = century code (I=18, J=19, K=20)
#   YY = year within century
#   H = half-month
#   M = cycle code (always 00 for old-style)
#   S = secondary letter

procedure mpc_pkold (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

char	prefix, centdig, centcode, halfmo, seclet
int	len, strlen()

begin
	len = strlen (unpacked)

	# Validate length: exactly 7 chars (like "A908 CJ")
	if (len != 7) {
	    packed[1] = EOS
	    return
	}

	# Validate format: [AB]DDD SS
	prefix = unpacked[1]
	if (prefix != 'A' && prefix != 'B') {
	    packed[1] = EOS
	    return
	}

	# Validate digits in positions 2-4
	if (!IS_DIGIT(unpacked[2]) || !IS_DIGIT(unpacked[3]) ||
	    !IS_DIGIT(unpacked[4])) {
	    packed[1] = EOS
	    return
	}

	# Validate space at position 5
	if (unpacked[5] != ' ') {
	    packed[1] = EOS
	    return
	}

	# Validate letters at positions 6-7
	if (!IS_UPPER(unpacked[6]) || !IS_UPPER(unpacked[7])) {
	    packed[1] = EOS
	    return
	}

	# Get century from first digit
	centdig = unpacked[2]
	if (centdig == '8') {
	    centcode = 'I'      # 1800s
	} else if (centdig == '9') {
	    centcode = 'J'      # 1900s
	} else if (centdig == '0') {
	    centcode = 'K'      # 2000s
	} else {
	    packed[1] = EOS
	    return
	}

	halfmo = unpacked[6]
	seclet = unpacked[7]

	# Build packed format: CYYHM00S
	packed[1] = centcode
	packed[2] = unpacked[3]     # year tens digit
	packed[3] = unpacked[4]     # year units digit
	packed[4] = halfmo
	packed[5] = '0'
	packed[6] = '0'
	packed[7] = seclet
	packed[8] = EOS
end


#---------------------------------------------------------------------------
# MPC_UNCNUM -- Unpack numbered comet

procedure mpc_uncnum (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	num

begin
	num = (packed[1]-'0')*1000 + (packed[2]-'0')*100 +
	      (packed[3]-'0')*10 + (packed[4]-'0')

	call sprintf (unpacked, maxch, "%d%c")
	    call pargi (num)
	    call pargc (packed[5])
end


#---------------------------------------------------------------------------
# MPC_PKCNUM -- Pack numbered comet

procedure mpc_pkcnum (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

int	num, i, len, strlen()

begin
	len = strlen (unpacked)
	num = 0
	i = 1
	while (IS_DIGIT(unpacked[i])) {
	    num = num * 10 + (unpacked[i] - '0')
	    i = i + 1
	}

	# Validate comet number > 0
	if (num <= 0) {
	    packed[1] = EOS
	    return
	}

	call sprintf (packed, maxch, "%04d%c")
	    call pargi (num)
	    call pargc (unpacked[len])
end


#---------------------------------------------------------------------------
# MPC_UNCPRV -- Unpack comet provisional

procedure mpc_uncprv (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	year, order, mpc_centc(), mpc_deccyc()
char	frag, ufrag

begin
	year = mpc_centc (packed[1]) + (packed[2] - '0') * 10 + (packed[3] - '0')
	order = mpc_deccyc (packed, 5)
	frag = packed[7]

	if (frag == '0' || frag == EOS) {
	    call sprintf (unpacked, maxch, "%d %c%d")
		call pargi (year)
		call pargc (packed[4])
		call pargi (order)
	} else {
	    ufrag = frag - 32
	    call sprintf (unpacked, maxch, "%d %c%d-%c")
		call pargi (year)
		call pargc (packed[4])
		call pargi (order)
		call pargc (ufrag)
	}
end


#---------------------------------------------------------------------------
# MPC_UNCFUL -- Unpack full comet designation

procedure mpc_uncful (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	len, year, order, bcecode, mpc_deccyc(), strlen()
char	ctype, bceprefix, halfmo, frag, frag2, ufrag, ufrag2, provpk[10], provup[30]

begin
	len = strlen (packed)
	ctype = packed[1]

	# Check for BCE comet (prefix is /, ., or -)
	bceprefix = packed[2]
	if (bceprefix == '/' || bceprefix == '.' || bceprefix == '-') {
	    bcecode = (packed[3] - '0') * 10 + (packed[4] - '0')
	    year = 99 - bcecode
	    if (bceprefix == '.')
		year = year + 100
	    else if (bceprefix == '-')
		year = year + 200
	    year = -year
	    halfmo = packed[5]
	    order = mpc_deccyc (packed, 6)
	    frag = packed[8]
	    frag2 = packed[9]
	    if (frag != '0' && frag != EOS) {
		ufrag = frag - 32
		if (frag2 != EOS && IS_LOWER(frag2)) {
		    # Two-letter fragment
		    ufrag2 = frag2 - 32
		    call sprintf (unpacked, maxch, "%c/%d %c%d-%c%c")
			call pargc (ctype)
			call pargi (year)
			call pargc (halfmo)
			call pargi (order)
			call pargc (ufrag)
			call pargc (ufrag2)
		} else {
		    call sprintf (unpacked, maxch, "%c/%d %c%d-%c")
			call pargc (ctype)
			call pargi (year)
			call pargc (halfmo)
			call pargi (order)
			call pargc (ufrag)
		}
	    } else {
		call sprintf (unpacked, maxch, "%c/%d %c%d")
		    call pargc (ctype)
		    call pargi (year)
		    call pargc (halfmo)
		    call pargi (order)
	    }
	    return
	}

	# Check for ancient comet (year < 1000, digit in position 2)
	if (IS_DIGIT(packed[2])) {
	    year = (packed[2] - '0') * 100 + (packed[3] - '0') * 10 + (packed[4] - '0')
	    halfmo = packed[5]
	    order = mpc_deccyc (packed, 6)
	    frag = packed[8]
	    frag2 = packed[9]
	    if (frag != '0' && frag != EOS) {
		ufrag = frag - 32
		if (frag2 != EOS && IS_LOWER(frag2)) {
		    # Two-letter fragment
		    ufrag2 = frag2 - 32
		    call sprintf (unpacked, maxch, "%c/%d %c%d-%c%c")
			call pargc (ctype)
			call pargi (year)
			call pargc (halfmo)
			call pargi (order)
			call pargc (ufrag)
			call pargc (ufrag2)
		} else {
		    call sprintf (unpacked, maxch, "%c/%d %c%d-%c")
			call pargc (ctype)
			call pargi (year)
			call pargc (halfmo)
			call pargi (order)
			call pargc (ufrag)
		}
	    } else {
		call sprintf (unpacked, maxch, "%c/%d %c%d")
		    call pargc (ctype)
		    call pargi (year)
		    call pargc (halfmo)
		    call pargi (order)
	    }
	    return
	}

	# Standard comet: delegate to existing functions
	call strcpy (packed, provpk, 10)
	call mpc_shiftl (provpk, 1)

	if (IS_UPPER(packed[len]))
	    call mpc_unprov (provpk, provup, 30)
	else
	    call mpc_uncprv (provpk, provup, 30)

	call sprintf (unpacked, maxch, "%c/%s")
	    call pargc (ctype)
	    call pargstr (provup)
end


#---------------------------------------------------------------------------
# MPC_PKCFUL -- Pack full comet designation

procedure mpc_pkcful (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

int	i, j, year, order, century, spcpos, dashpos, slashpos, absyear, bcecode
int	mpc_strchr(), mpc_strfrom(), strlen()
char	ctype, halfmo, frag, frag2, fragchar, centcode, yrstr[5], provpart[40], mpc_centl()
char	slash, space, dash, bceprefix
bool	isneg, twoletter

begin
	slash = '/'
	space = ' '
	dash = '-'

	slashpos = mpc_strchr (unpacked, slash)
	if (slashpos == 0) {
	    packed[1] = EOS
	    return
	}

	# Check for double slash
	if (unpacked[slashpos + 1] == '/') {
	    packed[1] = EOS
	    return
	}

	ctype = unpacked[slashpos - 1]
	spcpos = mpc_strfrom (unpacked, space, slashpos + 1)
	if (spcpos == 0) {
	    packed[1] = EOS
	    return
	}

	# Parse year (may be negative for BCE)
	year = 0
	isneg = false
	i = slashpos + 1
	if (unpacked[i] == '-') {
	    isneg = true
	    i = i + 1
	}
	while (i < spcpos) {
	    year = year * 10 + (unpacked[i] - '0')
	    i = i + 1
	}
	if (isneg)
	    year = -year

	century = year / 100
	centcode = mpc_centl (century)
	call sprintf (yrstr, 5, "%02d")
	    call pargi (mod (year, 100))

	halfmo = unpacked[spcpos + 1]

	dashpos = mpc_strfrom (unpacked, dash, spcpos)
	frag = '0'
	frag2 = EOS
	twoletter = false
	if (dashpos > 0) {
	    fragchar = unpacked[dashpos + 1]
	    # Validate fragment is a letter (not numeric)
	    if (IS_DIGIT(fragchar)) {
		packed[1] = EOS
		return
	    }
	    # Check for two-letter fragment (like AA, AB, etc.)
	    if (unpacked[dashpos + 2] != EOS && IS_UPPER(unpacked[dashpos + 2])) {
		# Two letter fragment
		if (unpacked[dashpos + 3] != EOS && unpacked[dashpos + 3] != ' ' &&
		    unpacked[dashpos + 3] != '\n') {
		    # More than two letters - invalid
		    packed[1] = EOS
		    return
		}
		twoletter = true
		frag = fragchar + 32       # first letter lowercase
		frag2 = unpacked[dashpos + 2] + 32  # second letter lowercase
	    } else {
		# Single letter fragment
		frag = fragchar + 32
	    }
	    order = 0
	    i = spcpos + 2
	    while (i < dashpos && IS_DIGIT(unpacked[i])) {
		order = order * 10 + (unpacked[i] - '0')
		i = i + 1
	    }
	} else {
	    order = 0
	    i = spcpos + 2
	    while (IS_DIGIT(unpacked[i])) {
		# Check for integer overflow
		if (order > 100000) {
		    packed[1] = EOS
		    return
		}
		order = order * 10 + (unpacked[i] - '0')
		i = i + 1
	    }
	}

	# Validate comet order > 0 (order 0 is invalid)
	if (order <= 0) {
	    # Check if there's a second letter (provisional format like "C/1995 O1")
	    # In that case order defaults to 1
	    if (!IS_UPPER(unpacked[spcpos + 2])) {
		packed[1] = EOS
		return
	    }
	    order = 1
	}

	# Validate comet order range (max 619 for standard encoding)
	if (order > 619) {
	    packed[1] = EOS
	    return
	}

	# Handle BCE years (year < 0)
	if (year < 0) {
	    absyear = -year
	    if (absyear > 299) {
		# BCE year out of supported range
		packed[1] = EOS
		return
	    }
	    bcecode = 99 - mod(absyear, 100)
	    if (absyear < 100) {
		bceprefix = '/'
	    } else if (absyear < 200) {
		bceprefix = '.'
	    } else {
		bceprefix = '-'
	    }
	    packed[1] = ctype
	    packed[2] = bceprefix
	    call sprintf (yrstr, 5, "%02d")
		call pargi (bcecode)
	    packed[3] = yrstr[1]
	    packed[4] = yrstr[2]
	    packed[5] = halfmo
	    call mpc_enccyc (order, packed, 6)
	    packed[8] = frag
	    if (twoletter) {
		packed[9] = frag2
		packed[10] = EOS
	    } else {
		packed[9] = EOS
	    }
	} else if (year < 1000) {
	    # Ancient comet (year 0-999): 3-digit year directly
	    packed[1] = ctype
	    call sprintf (yrstr, 5, "%03d")
		call pargi (year)
	    packed[2] = yrstr[1]
	    packed[3] = yrstr[2]
	    packed[4] = yrstr[3]
	    packed[5] = halfmo
	    call mpc_enccyc (order, packed, 6)
	    packed[8] = frag
	    if (twoletter) {
		packed[9] = frag2
		packed[10] = EOS
	    } else {
		packed[9] = EOS
	    }
	} else if (strlen(unpacked) > spcpos + 1 && IS_UPPER(unpacked[spcpos + 2])) {
	    # Extract provisional part (after slash) for packing
	    j = 1
	    do i = slashpos + 1, strlen(unpacked) {
		provpart[j] = unpacked[i]
		j = j + 1
	    }
	    provpart[j] = EOS

	    call mpc_pkprov (provpart, packed, maxch)
	    call mpc_shiftr (packed, 1)
	    packed[1] = ctype
	} else {
	    packed[1] = ctype
	    packed[2] = centcode
	    packed[3] = yrstr[1]
	    packed[4] = yrstr[2]
	    packed[5] = halfmo
	    call mpc_enccyc (order, packed, 6)
	    packed[8] = frag
	    if (twoletter) {
		packed[9] = frag2
		packed[10] = EOS
	    } else {
		packed[9] = EOS
	    }
	}
end


#---------------------------------------------------------------------------
# MPC_UNSAT -- Unpack satellite designation

procedure mpc_unsat (packed, unpacked, maxch)

char	packed[ARB], unpacked[ARB]
int	maxch

int	year, num, mpc_centc(), mpc_deccyc()

begin
	year = mpc_centc (packed[2]) + (packed[3] - '0') * 10 + (packed[4] - '0')
	num = mpc_deccyc (packed, 6)

	call sprintf (unpacked, maxch, "S/%d %c %d")
	    call pargi (year)
	    call pargc (packed[5])
	    call pargi (num)
end


#---------------------------------------------------------------------------
# MPC_PKSAT -- Pack satellite designation

procedure mpc_pksat (unpacked, packed, maxch)

char	unpacked[ARB], packed[ARB]
int	maxch

int	year, num, century, i
char	planet, centcode, mpc_centl()
bool	mpc_isplanet()

begin
	# Validate planet code (J, S, U, N)
	planet = unpacked[8]
	if (!mpc_isplanet (planet)) {
	    packed[1] = EOS
	    return
	}

	year = (unpacked[3]-'0')*1000 + (unpacked[4]-'0')*100 +
	       (unpacked[5]-'0')*10 + (unpacked[6]-'0')
	century = year / 100
	centcode = mpc_centl (century)

	num = 0
	i = 10
	while (IS_DIGIT(unpacked[i])) {
	    num = num * 10 + (unpacked[i] - '0')
	    i = i + 1
	}

	# Validate satellite number > 0
	if (num <= 0) {
	    packed[1] = EOS
	    return
	}

	packed[1] = 'S'
	packed[2] = centcode
	packed[3] = unpacked[5]
	packed[4] = unpacked[6]
	packed[5] = unpacked[8]
	call mpc_enccyc (num, packed, 6)
	packed[8] = '0'
	packed[9] = EOS
end


#---------------------------------------------------------------------------
# Helper procedures
#---------------------------------------------------------------------------

# MPC_B62C -- Base-62 char to number

int procedure mpc_b62c (c)

char	c

begin
	if (IS_DIGIT(c))
	    return (c - '0')
	else if (IS_UPPER(c))
	    return (c - 'A' + 10)
	else if (IS_LOWER(c))
	    return (c - 'a' + 36)
	else
	    return (0)
end


# MPC_N2B62 -- Number to base-62 char

char procedure mpc_n2b62 (n)

int	n

begin
	if (n < 10)
	    return ('0' + n)
	else if (n < 36)
	    return ('A' + n - 10)
	else
	    return ('a' + n - 36)
end


# MPC_B62D4 -- Decode 4-char base-62 string

int procedure mpc_b62d4 (s, start)

char	s[ARB]
int	start

int	val, mpc_b62c()

begin
	val = mpc_b62c (s[start])
	val = val * 62 + mpc_b62c (s[start + 1])
	val = val * 62 + mpc_b62c (s[start + 2])
	val = val * 62 + mpc_b62c (s[start + 3])
	return (val)
end


# MPC_B62E4 -- Encode number as 4-char base-62

procedure mpc_b62e4 (n, s, start)

int	n, start
char	s[ARB]

int	val
char	mpc_n2b62()

begin
	val = n
	s[start + 3] = mpc_n2b62 (mod (val, 62))
	val = val / 62
	s[start + 2] = mpc_n2b62 (mod (val, 62))
	val = val / 62
	s[start + 1] = mpc_n2b62 (mod (val, 62))
	val = val / 62
	s[start] = mpc_n2b62 (mod (val, 62))
end


# MPC_CENTC -- Century letter to value

int procedure mpc_centc (c)

char	c

begin
	if (c >= 'A' && c <= 'L')
	    return ((c - 'A' + 10) * 100)
	else
	    return (0)
end


# MPC_CENTL -- Century value to letter

char procedure mpc_centl (century)

int	century

begin
	if (century < 10 || century > 21)
	    return ('?')
	return ('A' + century - 10)
end


# MPC_ISCENT -- Check if char is century code

bool procedure mpc_iscent (c)

char	c

begin
	return (c >= 'A' && c <= 'L')
end


# MPC_ISHALFMO -- Check if char is valid half-month letter (A-Y excluding I)

bool procedure mpc_ishalfmo (c)

char	c

begin
	return (c >= 'A' && c <= 'Y' && c != 'I')
end


# MPC_ISSECLET -- Check if char is valid second letter (A-Z excluding I)

bool procedure mpc_isseclet (c)

char	c

begin
	return (IS_UPPER(c) && c != 'I')
end


# MPC_ISPLANET -- Check if char is valid satellite planet code (J, S, U, N)

bool procedure mpc_isplanet (c)

char	c

begin
	return (c == 'J' || c == 'S' || c == 'U' || c == 'N')
end


# MPC_ISB62 -- Check if char is valid base-62 character (0-9, A-Z, a-z)

bool procedure mpc_isb62 (c)

char	c

begin
	return (IS_DIGIT(c) || IS_UPPER(c) || IS_LOWER(c))
end


# MPC_DECCYC -- Decode 2-char cycle count

int procedure mpc_deccyc (s, start)

char	s[ARB]
int	start

int	tens, ones
char	first, second

begin
	first = s[start]
	second = s[start + 1]

	if (IS_DIGIT(first))
	    tens = first - '0'
	else if (IS_UPPER(first))
	    tens = first - 'A' + 10
	else if (IS_LOWER(first))
	    tens = first - 'a' + 36
	else
	    tens = 0

	if (IS_DIGIT(second))
	    ones = second - '0'
	else
	    ones = 0

	return (tens * 10 + ones)
end


# MPC_ENCCYC -- Encode cycle count as 2 chars

procedure mpc_enccyc (count, s, start)

int	count, start
char	s[ARB]

int	tens, ones

begin
	tens = count / 10
	ones = mod (count, 10)

	if (tens < 10)
	    s[start] = '0' + tens
	else if (tens < 36)
	    s[start] = 'A' + tens - 10
	else
	    s[start] = 'a' + tens - 36

	s[start + 1] = '0' + ones
end


# MPC_L2P -- Letter to position (A=1, skipping I)

int procedure mpc_l2p (letter)

char	letter

int	pos

begin
	pos = letter - 'A' + 1
	if (letter > 'I')
	    pos = pos - 1
	return (pos)
end


# MPC_P2L -- Position to letter (1=A, skipping I)

char procedure mpc_p2l (pos)

int	pos

int	p

begin
	p = pos
	if (p >= 9)
	    p = p + 1
	return ('A' + p - 1)
end


#---------------------------------------------------------------------------
# String utilities
#---------------------------------------------------------------------------

# MPC_STRTRIM -- Trim whitespace

procedure mpc_strtrim (input, output, maxch)

char	input[ARB], output[ARB]
int	maxch

int	i, j, start, iend, len, strlen()

begin
	len = strlen (input)

	start = 1
	while (start <= len && (input[start] == ' ' || input[start] == '\t'))
	    start = start + 1

	iend = len
	while (iend >= start && (input[iend] == ' ' || input[iend] == '\t'))
	    iend = iend - 1

	j = 1
	do i = start, iend {
	    if (j <= maxch) {
		output[j] = input[i]
		j = j + 1
	    }
	}
	output[j] = EOS
end


# MPC_STRCHR -- Find char in string

int procedure mpc_strchr (s, c)

char	s[ARB], c
int	i

begin
	do i = 1, ARB {
	    if (s[i] == EOS)
		return (0)
	    else if (s[i] == c)
		return (i)
	}
end


# MPC_STRFROM -- Find char starting from position

int procedure mpc_strfrom (s, c, start)

char	s[ARB], c
int	start, i

begin
	do i = start, ARB {
	    if (s[i] == EOS)
		return (0)
	    else if (s[i] == c)
		return (i)
	}
end


# MPC_STRSTR -- Find substring

int procedure mpc_strstr (s, sub)

char	s[ARB], sub[ARB]

int	i, j, slen, sublen, strlen()
bool	match

begin
	slen = strlen (s)
	sublen = strlen (sub)

	if (sublen > slen)
	    return (0)

	do i = 1, slen - sublen + 1 {
	    match = true
	    do j = 1, sublen {
		if (s[i + j - 1] != sub[j])
		    match = false
	    }
	    if (match)
		return (i)
	}
	return (0)
end


# MPC_SHIFTL -- Shift string left

procedure mpc_shiftl (s, n)

char	s[ARB]
int	n

int	i, len, strlen()

begin
	len = strlen (s)
	do i = 1, len - n + 1 {
	    s[i] = s[i + n]
	}
end


# MPC_SHIFTR -- Shift string right

procedure mpc_shiftr (s, n)

char	s[ARB]
int	n

int	i, len, strlen()

begin
	len = strlen (s)
	do i = len + n, n + 1, -1 {
	    s[i] = s[i - n]
	}
end
