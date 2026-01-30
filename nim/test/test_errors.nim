## test_errors.nim - Test suite for MPC Designation Converter (Nim)

import ../src/mpc_designation
import strutils

var passed = 0
var failed = 0

proc testConvert(input, expected: string) =
  let result = convertSimple(input)
  if result == expected:
    inc passed
  else:
    inc failed
    echo "FAIL: '", input, "' -> '", result, "' (expected '", expected, "')"

echo "Running Nim MPC Designation tests..."
echo ""

# Permanent asteroids - packed to unpacked
testConvert("00001", "1")
testConvert("00123", "123")
testConvert("99999", "99999")
testConvert("A0001", "100001")
testConvert("Z9999", "359999")
testConvert("a0001", "360001")
testConvert("z9999", "619999")
testConvert("~0000", "620000")
testConvert("~000z", "620061")

# Permanent asteroids - unpacked to packed
testConvert("1", "00001")
testConvert("99999", "99999")
testConvert("100001", "A0001")
testConvert("359999", "Z9999")
testConvert("360001", "a0001")
testConvert("619999", "z9999")
testConvert("620000", "~0000")
testConvert("620061", "~000z")

# Provisional designations
testConvert("J95X00A", "1995 XA")
testConvert("K24AB3B", "2024 AB113")
testConvert("1995 XA", "J95X00A")
testConvert("2024 AB113", "K24AB3B")

# Survey designations
testConvert("PLS2040", "2040 P-L")
testConvert("T1S1234", "1234 T-1")
testConvert("T2S0001", "1 T-2")
testConvert("2040 P-L", "PLS2040")
testConvert("1234 T-1", "T1S1234")

# Numbered comets
testConvert("0001P", "1P")
testConvert("0002P", "2P")
testConvert("1P", "0001P")
testConvert("2P", "0002P")

# Provisional comets
testConvert("CJ95O010", "C/1995 O1")
testConvert("C/1995 O1", "CJ95O010")
testConvert("DJ93F02b", "D/1993 F2-B")
testConvert("D/1993 F2-B", "DJ93F02b")

# Natural satellites
testConvert("SK19S220", "S/2019 S 22")
testConvert("S/2019 S 22", "SK19S220")

echo ""
echo "Passed: ", passed
echo "Failed: ", failed

if failed == 0:
  echo "All tests passed!"
  quit(0)
else:
  quit(1)
