## MPC Designation Converter - Nim Implementation
##
## Converts between packed and unpacked Minor Planet Center designations.
## Supports: asteroids (permanent, provisional, survey), comets, natural satellites.

import strutils, tables, parseutils

const
  BASE62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  # Century codes: A=10 (1000s), B=11 (1100s), ... I=18 (1800s), J=19 (1900s), K=20 (2000s), L=21 (2100s)
  CENTURY_TO_NUM = {'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15,
                    'G': 16, 'H': 17, 'I': 18, 'J': 19, 'K': 20, 'L': 21}.toTable
  NUM_TO_CENTURY = {10: 'A', 11: 'B', 12: 'C', 13: 'D', 14: 'E', 15: 'F',
                    16: 'G', 17: 'H', 18: 'I', 19: 'J', 20: 'K', 21: 'L'}.toTable

  SURVEY_TO_PACKED = {"P-L": "PLS", "T-1": "T1S", "T-2": "T2S", "T-3": "T3S"}.toTable
  SURVEY_TO_UNPACKED = {"PLS": "P-L", "T1S": "T-1", "T2S": "T-2", "T3S": "T-3"}.toTable

  COMET_TYPES = {'P', 'D', 'C', 'X', 'A', 'I'}

# Build base62 value lookup at compile time
proc buildBase62Lookup(): array[128, int] =
  for i in 0..<128:
    result[i] = -1
  for i, c in BASE62:
    result[ord(c)] = i

const BASE62_VAL = buildBase62Lookup()

#==============================================================================
# Helper functions
#==============================================================================

proc base62Char(val: int): char {.inline.} =
  BASE62[val]

proc base62Value(c: char): int {.inline.} =
  BASE62_VAL[ord(c)]

proc isAllDigits(s: string): bool =
  if s.len == 0: return false
  for c in s:
    if c < '0' or c > '9': return false
  true

# Half-month letter to position (A=1, B=2, ..., H=8, J=9, ... Y=24, skipping I)
proc letterToPosition(c: char): int =
  if c <= 'H': ord(c) - ord('A') + 1
  else: ord(c) - ord('A')  # Skip I

proc positionToLetter(pos: int): char =
  if pos <= 8: char(ord('A') + pos - 1)
  else: char(ord('A') + pos)  # Skip I

# Convert number to 4-character base-62 string
proc numToBase62String(num: int): string =
  result = ""
  var n = num
  for i in 0..<4:
    result = $base62Char(n mod 62) & result
    n = n div 62

# Convert 4-character base-62 string to number
proc base62StringToNum(s: string): int =
  result = 0
  for c in s:
    result = result * 62 + base62Value(c)

#==============================================================================
# Extended provisional format (underscore prefix for cycle >= 620)
#==============================================================================

proc packExtendedProvisional(year: int, halfMonth: char, secondLetter: char, cycle: int): string =
  let yearShort = year mod 100
  # Cycle 620 letter A = sequence 0, cycle 620 letter B = sequence 1, etc.
  let baseSequence = (cycle - 620) * 25 + letterToPosition(secondLetter) - 1
  let seqEncoded = numToBase62String(baseSequence)
  result = "_" & base62Char(yearShort) & halfMonth & seqEncoded

proc unpackExtendedProvisional(packed: string): string =
  let yearDigit = packed[1]
  let halfMonth = packed[2]
  let seqEncoded = packed[3..6]

  let baseSequence = base62StringToNum(seqEncoded)
  let cycle = 620 + baseSequence div 25
  let letterPos = (baseSequence mod 25) + 1
  let secondLetter = positionToLetter(letterPos)

  # Year code is base-62, extended format is for years 2000+
  let year = 2000 + base62Value(yearDigit)

  result = $year & " " & halfMonth & secondLetter & $cycle

#==============================================================================
# Permanent asteroid encoding/decoding
#==============================================================================

proc packPermanent*(num: int): string =
  if num < 100000:
    result = align($num, 5, '0')
  elif num < 620000:
    let prefixVal = num div 10000
    let suffix = num mod 10000
    result = $base62Char(prefixVal) & align($suffix, 4, '0')
  else:
    let val = num - 620000
    let c1 = base62Char((val div (62*62*62)) mod 62)
    let c2 = base62Char((val div (62*62)) mod 62)
    let c3 = base62Char((val div 62) mod 62)
    let c4 = base62Char(val mod 62)
    result = "~" & c1 & c2 & c3 & c4

proc unpackPermanent*(packed: string): string =
  let first = packed[0]
  if first == '~':
    let v1 = base62Value(packed[1])
    let v2 = base62Value(packed[2])
    let v3 = base62Value(packed[3])
    let v4 = base62Value(packed[4])
    result = $(620000 + v1*62*62*62 + v2*62*62 + v3*62 + v4)
  elif first >= 'A' and first <= 'Z':
    let prefixVal = base62Value(first)
    let suffix = parseInt(packed[1..4])
    result = $(prefixVal * 10000 + suffix)
  elif first >= 'a' and first <= 'z':
    let prefixVal = base62Value(first)
    let suffix = parseInt(packed[1..4])
    result = $(prefixVal * 10000 + suffix)
  else:
    result = $parseInt(packed)

#==============================================================================
# Provisional designation encoding/decoding
#==============================================================================

proc encodeCycle(cycle: int): string =
  let c1 = base62Char(cycle div 10)
  let c2 = char(ord('0') + (cycle mod 10))
  result = $c1 & c2

proc decodeCycle(encoded: string): int =
  let v1 = base62Value(encoded[0])
  let v2 = ord(encoded[1]) - ord('0')
  v1 * 10 + v2

proc packProvisional*(desig: string): string =
  # Handle old-style designations: A908 CJ or B842 FA
  if desig[0] in {'A', 'B'} and desig[1] >= '0' and desig[1] <= '9':
    let centuryDigit = desig[1]
    let yearShort = desig[2..3]
    let half = desig[5]
    let second = desig[6]

    # Convert century digit to century code: 8->18(I), 9->19(J), 0->20(K)
    let centuryChar = case centuryDigit
      of '8': 'I'
      of '9': 'J'
      of '0': 'K'
      else: 'J'  # default

    return $centuryChar & yearShort & half & "00" & second

  # Parse standard: "1995 XA" or "1995 XA123"
  let year = parseInt(desig[0..3])
  let half = desig[5]
  let second = desig[6]
  let cycle = if desig.len > 7: parseInt(desig[7..^1]) else: 0

  # Use extended format for cycle >= 620
  if cycle >= 620:
    return packExtendedProvisional(year, half, second, cycle)

  let century = year div 100
  let centuryChar = NUM_TO_CENTURY[century]
  let yy = year mod 100

  result = $centuryChar & align($yy, 2, '0') & half & encodeCycle(cycle) & second

proc unpackProvisional*(packed: string): string =
  let centuryChar = packed[0]
  let yy = parseInt(packed[1..2])
  let half = packed[3]
  let cycleEnc = packed[4..5]
  let second = packed[6]

  let century = CENTURY_TO_NUM[centuryChar]
  let year = century * 100 + yy
  let cycle = decodeCycle(cycleEnc)

  # Pre-1925 designations use A-prefix format (A for 1xxx years, B for 2xxx years)
  var prefix = ""
  var yearStr = $year
  if year < 1925:
    let firstDigit = year div 1000
    if firstDigit == 1:
      prefix = "A"
    elif firstDigit == 2:
      prefix = "B"
    yearStr = $(year mod 1000)  # e.g., 1908 -> 908

  if cycle == 0:
    result = prefix & yearStr & " " & half & second
  else:
    result = prefix & yearStr & " " & half & second & $cycle

#==============================================================================
# Survey designation encoding/decoding
#==============================================================================

proc packSurvey*(desig: string): string =
  let parts = desig.split(' ')
  let num = parseInt(parts[0])
  let survey = parts[1]
  let prefix = SURVEY_TO_PACKED[survey]
  result = prefix & align($num, 4, '0')

proc unpackSurvey*(packed: string): string =
  let prefix = packed[0..2]
  let num = parseInt(packed[3..6])
  let survey = SURVEY_TO_UNPACKED[prefix]
  result = $num & " " & survey

#==============================================================================
# Comet encoding/decoding
#==============================================================================

proc packNumberedComet*(desig: string): string =
  var fragment = ""
  var baseDesig = desig

  # Check for 2-letter fragment: "73P-AA"
  if desig.len >= 4 and desig[^3] == '-':
    let frag1 = desig[^2]
    let frag2 = desig[^1]
    if frag1 in {'A'..'Z'} and frag2 in {'A'..'Z'}:
      fragment = $toLowerAscii(frag1) & toLowerAscii(frag2)
      baseDesig = desig[0..^4]
  # Check for 1-letter fragment: "73P-A"
  elif desig.len >= 2 and desig[^2] == '-':
    let fragChar = desig[^1]
    if fragChar in {'A'..'Z'}:
      fragment = $toLowerAscii(fragChar)
      baseDesig = desig[0..^3]

  let cometType = baseDesig[^1]
  let num = parseInt(baseDesig[0..^2])
  result = align($num, 4, '0') & cometType & fragment

proc unpackNumberedComet*(packed: string): string =
  let num = parseInt(packed[0..3])
  let cometType = packed[4]
  result = $num & cometType

  # Check for fragment (6-7 chars)
  if packed.len >= 6:
    let fragment = packed[5..^1]
    result &= "-" & toUpperAscii(fragment)

proc packProvisionalComet*(desig: string): string =
  var fragment = ""
  var baseDesig = desig

  # Handle fragment: "-B" (1-letter) or "-AA" (2-letter), convert to lowercase
  # Check for 2-letter fragment first: "C/1995 O1-AA"
  if desig.len >= 4 and desig[^3] == '-':
    let frag1 = desig[^2]
    let frag2 = desig[^1]
    if ((frag1 >= 'A' and frag1 <= 'Z') or (frag1 >= 'a' and frag1 <= 'z')) and
       ((frag2 >= 'A' and frag2 <= 'Z') or (frag2 >= 'a' and frag2 <= 'z')):
      fragment = $toLowerAscii(frag1) & toLowerAscii(frag2)
      baseDesig = desig[0..^4]
  # Check for 1-letter fragment: "C/1995 O1-B"
  elif desig.len >= 2 and desig[^2] == '-':
    let fragChar = desig[^1]
    if (fragChar >= 'a' and fragChar <= 'z') or (fragChar >= 'A' and fragChar <= 'Z'):
      fragment = $toLowerAscii(fragChar)
      baseDesig = desig[0..^3]

  let cometType = baseDesig[0]

  # Find space to parse variable-length year (handles ancient comets like C/240 V1)
  let spaceIdx = baseDesig.find(' ')
  let year = parseInt(baseDesig[2..<spaceIdx])
  let designPart = baseDesig[spaceIdx+1..^1]  # "V1" or "O1" or "AH2"
  let half = designPart[0]

  # BCE comets (negative years) use special century codes
  if year < 0:
    let absYear = -year
    var centuryCode: char
    var yy: int
    if absYear < 100:
      centuryCode = '/'
      yy = 100 - absYear - 1
    elif absYear < 200:
      centuryCode = '.'
      yy = 200 - absYear - 1
    else:
      centuryCode = '-'
      yy = 300 - absYear - 1

    let order = if designPart.len > 1: parseInt(designPart[1..^1]) else: 0
    let frag = if fragment.len > 0: fragment else: "0"
    let orderStr = align($order, 2, '0')
    result = $cometType & centuryCode & align($yy, 2, '0') & half & orderStr & frag
    return

  # Ancient comets (year < 1000) use special format: TYYYHNNN
  if year < 1000:
    let order = if designPart.len > 1: parseInt(designPart[1..^1]) else: 0
    let frag = if fragment.len > 0: fragment else: "0"
    let orderStr = if order < 100:
      align($order, 2, '0')
    else:
      $base62Char(order div 10) & $(order mod 10)
    result = $cometType & align($year, 3, '0') & half & orderStr & frag
    return

  let century = year div 100
  let centuryChar = NUM_TO_CENTURY[century]
  let yy = year mod 100

  # Check if extended format: "AH2" (half + second letter + cycle)
  # vs simple format: "O1" (half + order number)
  if designPart.len > 1 and designPart[1] >= 'A' and designPart[1] <= 'Z':
    # Extended format - same as provisional asteroids
    let second = designPart[1]
    let cycle = if designPart.len > 2: parseInt(designPart[2..^1]) else: 0
    let cycleStr = if cycle < 100:
      align($cycle, 2, '0')
    else:
      $base62Char(cycle div 10) & $(cycle mod 10)
    result = $cometType & centuryChar & align($yy, 2, '0') & half & cycleStr & second
  else:
    # Simple format - order number only
    let order = if designPart.len > 1: parseInt(designPart[1..^1]) else: 0
    let frag = if fragment.len > 0: fragment else: "0"
    let orderStr = if order < 100:
      align($order, 2, '0')
    else:
      $base62Char(order div 10) & $(order mod 10)
    result = $cometType & centuryChar & align($yy, 2, '0') & half & orderStr & frag

proc unpackProvisionalComet*(packed: string): string =
  let cometType = packed[0]
  let centuryChar = packed[1]
  let yy = try: parseInt(packed[2..3]) except ValueError: 0
  let half = packed[4]
  let cycleOrOrderStr = packed[5..6]

  # Check for 2-letter fragment (9-char format)
  let has2LetterFragment = packed.len == 9
  let lastChar = packed[7]
  let fragment2 = if has2LetterFragment: packed[8] else: '\0'

  let century = CENTURY_TO_NUM.getOrDefault(centuryChar, 19)
  let year = century * 100 + yy

  # Check if extended format (last char at position 7 is uppercase letter A-Z) or simple format
  if lastChar >= 'A' and lastChar <= 'Z':
    # Extended format: cycle + second letter (like provisional asteroids)
    var cycle: int
    if cycleOrOrderStr[0] >= '0' and cycleOrOrderStr[0] <= '9':
      cycle = try: parseInt(cycleOrOrderStr) except ValueError: 0
    else:
      cycle = base62Value(cycleOrOrderStr[0]) * 10 + (ord(cycleOrOrderStr[1]) - ord('0'))
    let second = lastChar
    result = $cometType & "/" & $year & " " & half & second
    if cycle > 0:
      result &= $cycle
  else:
    # Simple format: order + fragment(s)
    var order: int
    if cycleOrOrderStr[0] >= '0' and cycleOrOrderStr[0] <= '9':
      order = try: parseInt(cycleOrOrderStr) except ValueError: 0
    else:
      order = base62Value(cycleOrOrderStr[0]) * 10 + (ord(cycleOrOrderStr[1]) - ord('0'))
    result = $cometType & "/" & $year & " " & half
    if order > 0:
      result &= $order
    if has2LetterFragment:
      # 2-letter fragment (convert to uppercase for unpacked format)
      result &= "-" & toUpperAscii(lastChar) & toUpperAscii(fragment2)
    elif lastChar != '0':
      # 1-letter fragment (convert to uppercase for unpacked format)
      result &= "-" & toUpperAscii(lastChar)

proc unpackAncientComet*(packed: string): string =
  # Format: TYYYHNNN where T=type, YYY=3-digit year, H=half, NNN=order+fragment
  let cometType = packed[0]
  let year = parseInt(packed[1..3])
  let half = packed[4]
  let orderStr = packed[5..6]
  let fragment = packed[7]

  var order: int
  if orderStr[0] >= '0' and orderStr[0] <= '9':
    order = try: parseInt(orderStr) except ValueError: 0
  else:
    order = base62Value(orderStr[0]) * 10 + (ord(orderStr[1]) - ord('0'))

  result = $cometType & "/" & $year & " " & half
  if order > 0:
    result &= $order
  if fragment != '0':
    result &= "-" & toUpperAscii(fragment)

#==============================================================================
# Natural satellite encoding/decoding
#==============================================================================

proc packSatellite*(desig: string): string =
  # Parse: "S/2019 S 22"
  let year = parseInt(desig[2..5])
  let planet = desig[7]
  let num = parseInt(desig[9..^1])

  let century = year div 100
  let centuryChar = NUM_TO_CENTURY[century]
  let yy = year mod 100

  result = "S" & centuryChar & align($yy, 2, '0') & planet & align($num, 2, '0') & "0"

proc unpackSatellite*(packed: string): string =
  let centuryChar = packed[1]
  let yy = parseInt(packed[2..3])
  let planet = packed[4]
  let num = parseInt(packed[5..6])

  let century = CENTURY_TO_NUM[centuryChar]
  let year = century * 100 + yy

  result = "S/" & $year & " " & planet & " " & $num

#==============================================================================
# Format detection
#==============================================================================

type DesignationFormat* = enum
  fmtUnknown
  fmtPackedPermanent
  fmtUnpackedPermanent
  fmtPackedProvisional
  fmtUnpackedProvisional
  fmtPackedExtendedProvisional  # Underscore format for cycle >= 620
  fmtPackedSurvey
  fmtUnpackedSurvey
  fmtPackedNumberedComet
  fmtUnpackedNumberedComet
  fmtPackedProvisionalComet
  fmtUnpackedProvisionalComet
  fmtPackedAncientComet  # Year < 1000
  fmtPackedSatellite
  fmtUnpackedSatellite

proc detectFormat*(desig: string): DesignationFormat =
  let len = desig.len
  if len == 0: return fmtUnknown

  let first = desig[0]

  # Packed permanent asteroid (5 chars, no slash, space, or hyphen)
  if len == 5 and '/' notin desig and ' ' notin desig and '-' notin desig:
    let last = desig[4]
    # Check for numbered comet
    if last in {'P', 'D', 'C', 'X', 'A'} and desig[0..3].isAllDigits:
      return fmtPackedNumberedComet
    return fmtPackedPermanent

  # Packed numbered comet with fragment (6 or 7 chars: 4 digits + P/D + 1-2 lowercase)
  if (len == 6 or len == 7) and desig[0..3].isAllDigits:
    let cometType = desig[4]
    if cometType in {'P', 'D'}:
      let fragmentPart = desig[5..^1]
      var allLower = true
      for c in fragmentPart:
        if c notin {'a'..'z'}:
          allLower = false
          break
      if allLower:
        return fmtPackedNumberedComet

  # Packed provisional asteroid (7 chars starting with century code)
  if len == 7 and first in {'I', 'J', 'K', 'L'} and '/' notin desig:
    return fmtPackedProvisional

  # Packed survey (7 chars starting with PLS, T1S, T2S, T3S)
  if len == 7:
    let prefix = desig[0..2]
    if prefix in ["PLS", "T1S", "T2S", "T3S"]:
      return fmtPackedSurvey

  # Extended provisional format (7 chars starting with underscore)
  if len == 7 and first == '_':
    return fmtPackedExtendedProvisional

  # Packed provisional comet, ancient comet, or satellite (8 chars)
  if len == 8:
    let second = desig[1]
    if first in COMET_TYPES and second in {'I', 'J', 'K', 'L', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'}:
      return fmtPackedProvisionalComet
    # Ancient comet: comet type + 3 digit year + half + order + fragment
    if first in COMET_TYPES and second >= '0' and second <= '9':
      return fmtPackedAncientComet
    if first == 'S' and second in {'I', 'J', 'K', 'L'}:
      return fmtPackedSatellite

  # Packed provisional comet with 2-letter fragment (9 chars)
  if len == 9:
    let second = desig[1]
    if first in COMET_TYPES and second in {'I', 'J', 'K', 'L', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'}:
      return fmtPackedProvisionalComet

  # Unpacked numbered asteroid (all digits)
  if desig.isAllDigits:
    return fmtUnpackedPermanent

  # Unpacked survey
  if ' ' in desig:
    let parts = desig.split(' ')
    if parts.len == 2 and parts[1] in ["P-L", "T-1", "T-2", "T-3"]:
      return fmtUnpackedSurvey

  # Unpacked numbered comet (with optional fragment like "73P-A" or "73P-AA")
  if len >= 2:
    # Check for fragment: "73P-A" or "73P-AA"
    let dashIdx = desig.find('-')
    if dashIdx > 0:
      let basePart = desig[0..<dashIdx]
      if basePart.len >= 2 and basePart[^1] in {'P', 'D'} and basePart[0..^2].isAllDigits:
        let fragPart = desig[dashIdx+1..^1]
        if fragPart.len in {1, 2}:
          var allUpper = true
          for c in fragPart:
            if c notin {'A'..'Z'}:
              allUpper = false
              break
          if allUpper:
            return fmtUnpackedNumberedComet
    # Without fragment
    if desig[^1] in {'P', 'D', 'C', 'X', 'A'} and desig[0..^2].isAllDigits:
      return fmtUnpackedNumberedComet

  # Unpacked provisional comet (must have space for year separation)
  if len >= 2 and first in {'P', 'D', 'C', 'X', 'A'} and desig[1] == '/' and ' ' in desig:
    return fmtUnpackedProvisionalComet

  # Unpacked satellite
  if desig.startsWith("S/") and ' ' in desig:
    return fmtUnpackedSatellite

  # Unpacked provisional asteroid (year + space + designation)
  if len >= 7 and desig[4] == ' ' and desig[0..3].isAllDigits:
    let halfMonth = desig[5]
    if halfMonth >= 'A' and halfMonth <= 'Y' and halfMonth != 'I':
      return fmtUnpackedProvisional

  # Old-style provisional asteroid: A908 CJ or B842 FA
  if len == 7 and first in {'A', 'B'} and desig[1] >= '0' and desig[1] <= '9' and desig[4] == ' ':
    return fmtUnpackedProvisional

  fmtUnknown

#==============================================================================
# Main conversion function
#==============================================================================

proc convertSimple*(desig: string): string =
  let fmt = detectFormat(desig)

  case fmt
  of fmtPackedPermanent: unpackPermanent(desig)
  of fmtUnpackedPermanent: packPermanent(parseInt(desig))
  of fmtPackedProvisional: unpackProvisional(desig)
  of fmtUnpackedProvisional: packProvisional(desig)
  of fmtPackedExtendedProvisional: unpackExtendedProvisional(desig)
  of fmtPackedSurvey: unpackSurvey(desig)
  of fmtUnpackedSurvey: packSurvey(desig)
  of fmtPackedNumberedComet: unpackNumberedComet(desig)
  of fmtUnpackedNumberedComet: packNumberedComet(desig)
  of fmtPackedProvisionalComet: unpackProvisionalComet(desig)
  of fmtUnpackedProvisionalComet: packProvisionalComet(desig)
  of fmtPackedAncientComet: unpackAncientComet(desig)
  of fmtPackedSatellite: unpackSatellite(desig)
  of fmtUnpackedSatellite: packSatellite(desig)
  of fmtUnknown: "ERROR: Unknown format: " & desig

#==============================================================================
# Helper functions
#==============================================================================

proc toReportFormat*(minimal: string): string =
  ## Convert minimal packed format to 12-character MPC observation report format
  let len = minimal.len

  # Numbered comet with fragment (6-7 chars: ####P/Df or ####P/Dff)
  if (len == 6 or len == 7) and minimal[0..3].isAllDigits and minimal[4] in {'P', 'D'}:
    let base = minimal[0..4]  # "0073P"
    let fragment = minimal[5..^1]  # "a" or "aa"
    if len == 6:
      return base & "      " & fragment  # 5 + 6 spaces + 1 = 12
    else:
      return base & "     " & fragment  # 5 + 5 spaces + 2 = 12

  # Numbered comet without fragment (5 chars: ####P/D)
  if len == 5 and minimal[0..3].isAllDigits and minimal[4] in {'P', 'D'}:
    return minimal & "       "  # 5 + 7 spaces = 12

  # Numbered asteroid or provisional (5-8 chars) - right-align to 12 chars
  let padding = 12 - len
  if padding > 0:
    result = spaces(padding) & minimal
  else:
    result = minimal

proc fromReportFormat*(report: string): string =
  ## Convert 12-character MPC report format to minimal packed format
  # Handle numbered comet with fragment in columns 11-12
  if report.len >= 5 and report[0..3].isAllDigits and report[4] in {'P', 'D'}:
    let base = report[0..4]
    let rest = report[5..^1].strip()  # fragment is at the end
    if rest.len > 0:
      return base & rest
    return base

  # Standard format: strip leading spaces
  result = report.strip(leading = true, trailing = true)

proc hasFragment*(desig: string): bool =
  ## Returns true if designation has a comet fragment suffix
  let len = desig.len
  if len < 2: return false

  # Unpacked format: "73P-A", "73P-AA", "D/1993 F2-A"
  let dashIdx = desig.find('-')
  if dashIdx > 0:
    let basePart = desig[0..<dashIdx]
    # Numbered comet with fragment
    if basePart.len >= 2 and basePart[^1] in {'P', 'D'} and basePart[0..^2].isAllDigits:
      return true
    # Provisional comet with fragment (has space before the dash position)
    if ' ' in basePart and basePart[0] in COMET_TYPES and basePart[1] == '/':
      return true

  # Packed numbered comet with fragment (6-7 chars)
  if (len == 6 or len == 7) and desig[0..3].isAllDigits and desig[4] in {'P', 'D'}:
    let fragmentPart = desig[5..^1]
    for c in fragmentPart:
      if c notin {'a'..'z'}: return false
    return true

  # Packed provisional comet with fragment (8 chars ending in lowercase, 9 chars for 2-letter)
  if len == 8 and desig[0] in COMET_TYPES and desig[1] in {'A'..'L'}:
    if desig[7] in {'a'..'z'}:
      return true

  if len == 9 and desig[0] in COMET_TYPES and desig[1] in {'A'..'L'}:
    if desig[7] in {'a'..'z'} and desig[8] in {'a'..'z'}:
      return true

  false

proc getFragment*(desig: string): string =
  ## Extract fragment suffix from comet designation (returns uppercase)
  let len = desig.len
  if len < 2: return ""

  # Unpacked format: "73P-A", "73P-AA"
  let dashIdx = desig.find('-')
  if dashIdx > 0:
    let fragPart = desig[dashIdx+1..^1]
    # Check if it looks like a fragment (1-2 uppercase letters)
    if fragPart.len in {1, 2}:
      for c in fragPart:
        if c notin {'A'..'Z'}: return ""
      return fragPart

  # Packed numbered comet with fragment (6-7 chars)
  if (len == 6 or len == 7) and desig[0..3].isAllDigits and desig[4] in {'P', 'D'}:
    let fragmentPart = desig[5..^1]
    for c in fragmentPart:
      if c notin {'a'..'z'}: return ""
    return toUpperAscii(fragmentPart)

  # Packed provisional comet with fragment (8 chars ending in lowercase)
  if len == 8 and desig[0] in COMET_TYPES and desig[1] in {'A'..'L'}:
    if desig[7] in {'a'..'z'}:
      return $toUpperAscii(desig[7])

  # Packed provisional comet with 2-letter fragment (9 chars)
  if len == 9 and desig[0] in COMET_TYPES and desig[1] in {'A'..'L'}:
    if desig[7] in {'a'..'z'} and desig[8] in {'a'..'z'}:
      return toUpperAscii(desig[7..8])

  ""

proc getParent*(desig: string): string =
  ## Returns parent comet designation without fragment suffix
  let len = desig.len
  if len < 2: return desig

  # Unpacked format: "73P-A" -> "73P"
  let dashIdx = desig.find('-')
  if dashIdx > 0:
    let basePart = desig[0..<dashIdx]
    # Numbered comet with fragment
    if basePart.len >= 2 and basePart[^1] in {'P', 'D'} and basePart[0..^2].isAllDigits:
      return basePart
    # Provisional comet with fragment
    if ' ' in basePart and basePart[0] in COMET_TYPES and basePart[1] == '/':
      return basePart

  # Packed numbered comet with fragment (6-7 chars)
  if (len == 6 or len == 7) and desig[0..3].isAllDigits and desig[4] in {'P', 'D'}:
    let fragmentPart = desig[5..^1]
    var allLower = true
    for c in fragmentPart:
      if c notin {'a'..'z'}:
        allLower = false
        break
    if allLower:
      return desig[0..4]

  # Packed provisional comet with fragment (8 chars ending in lowercase)
  if len == 8 and desig[0] in COMET_TYPES and desig[1] in {'A'..'L'}:
    if desig[7] in {'a'..'z'}:
      return desig[0..6] & "0"  # Replace fragment with '0'

  # Packed provisional comet with 2-letter fragment (9 chars)
  if len == 9 and desig[0] in COMET_TYPES and desig[1] in {'A'..'L'}:
    if desig[7] in {'a'..'z'} and desig[8] in {'a'..'z'}:
      return desig[0..6] & "0"  # Replace fragment with '0'

  desig

proc designationsEqual*(d1, d2: string): bool =
  ## Returns true if two designations refer to the same object
  try:
    var packed1 = d1
    var packed2 = d2

    # Convert to packed format if needed
    let fmt1 = detectFormat(d1)
    let fmt2 = detectFormat(d2)

    if fmt1 in {fmtUnpackedPermanent, fmtUnpackedProvisional, fmtUnpackedSurvey,
                fmtUnpackedNumberedComet, fmtUnpackedProvisionalComet, fmtUnpackedSatellite}:
      packed1 = convertSimple(d1)

    if fmt2 in {fmtUnpackedPermanent, fmtUnpackedProvisional, fmtUnpackedSurvey,
                fmtUnpackedNumberedComet, fmtUnpackedProvisionalComet, fmtUnpackedSatellite}:
      packed2 = convertSimple(d2)

    packed1 == packed2
  except:
    false

#==============================================================================
# CLI entry point
#==============================================================================

when isMainModule:
  import os

  if paramCount() == 0 or paramStr(1) in ["-h", "--help"]:
    echo "Usage: mpc_designation <designation> [designation ...]"
    echo "Converts between packed and unpacked MPC designations."
    echo ""
    echo "Examples:"
    echo "  mpc_designation 00001          # -> 1"
    echo "  mpc_designation '1995 XA'      # -> J95X00A"
    echo "  mpc_designation 1P             # -> 0001P"
    quit(0)

  for i in 1..paramCount():
    echo convertSimple(paramStr(i))
