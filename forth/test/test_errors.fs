\ test_errors.fs - Comprehensive tests for MPC Designation Converter
\
\ Tests all supported designation formats in both directions.

require ../src/mpc_designation.fs

variable pass-count
variable fail-count
variable test-num

256 constant TEST-BUF-LEN
create expected-buf TEST-BUF-LEN allot
variable expected-len

: test-convert ( input-addr input-len expected-addr expected-len -- )
  1 test-num +!
  \ Save expected string to buffer first
  dup expected-len !
  expected-buf swap move       \ Stack: input-addr input-len
  \ Convert input
  convert-simple               \ Stack: result-addr result-len
  \ Compare lengths
  dup expected-len @ = if
    expected-buf expected-len @ s= if
      1 pass-count +!
    else
      1 fail-count +!
      ." FAIL #" test-num @ . ." : content mismatch" cr
    then
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : length mismatch, expected " expected-len @ .
    ."  got " dup . cr
    2drop
  then ;

: run-tests
  0 pass-count !
  0 fail-count !
  0 test-num !

  \ ============================================================
  \ Permanent asteroids - packed to unpacked
  \ ============================================================
  s" 00001" s" 1" test-convert
  s" 00012" s" 12" test-convert
  s" 00123" s" 123" test-convert
  s" 01234" s" 1234" test-convert
  s" 12345" s" 12345" test-convert
  s" 99999" s" 99999" test-convert

  \ Extended permanent (letter prefix)
  s" A0000" s" 100000" test-convert
  s" A0001" s" 100001" test-convert
  s" A9999" s" 109999" test-convert
  s" B0000" s" 110000" test-convert
  s" Z9999" s" 359999" test-convert
  s" a0000" s" 360000" test-convert
  s" z9999" s" 619999" test-convert

  \ Extended permanent (tilde format >= 620000)
  s" ~0000" s" 620000" test-convert
  s" ~0001" s" 620001" test-convert
  s" ~000z" s" 620061" test-convert
  s" ~0010" s" 620062" test-convert

  \ ============================================================
  \ Permanent asteroids - unpacked to packed
  \ ============================================================
  s" 1" s" 00001" test-convert
  s" 12" s" 00012" test-convert
  s" 123" s" 00123" test-convert
  s" 1234" s" 01234" test-convert
  s" 12345" s" 12345" test-convert
  s" 99999" s" 99999" test-convert

  s" 100000" s" A0000" test-convert
  s" 100001" s" A0001" test-convert
  s" 109999" s" A9999" test-convert
  s" 110000" s" B0000" test-convert
  s" 359999" s" Z9999" test-convert
  s" 360000" s" a0000" test-convert
  s" 619999" s" z9999" test-convert

  s" 620000" s" ~0000" test-convert
  s" 620001" s" ~0001" test-convert
  s" 620061" s" ~000z" test-convert
  s" 620062" s" ~0010" test-convert

  \ ============================================================
  \ Provisional designations - packed to unpacked
  \ ============================================================
  s" J95X00A" s" 1995 XA" test-convert
  s" J95X01A" s" 1995 XA1" test-convert
  s" J95X10A" s" 1995 XA10" test-convert
  s" J95XA0A" s" 1995 XA100" test-convert
  s" K00A00A" s" 2000 AA" test-convert
  s" K24AB3B" s" 2024 AB113" test-convert
  s" K24KG7S" s" 2024 KS167" test-convert

  \ ============================================================
  \ Provisional designations - unpacked to packed
  \ ============================================================
  s" 1995 XA" s" J95X00A" test-convert
  s" 1995 XA1" s" J95X01A" test-convert
  s" 1995 XA10" s" J95X10A" test-convert
  s" 1995 XA100" s" J95XA0A" test-convert
  s" 2000 AA" s" K00A00A" test-convert
  s" 2024 AB113" s" K24AB3B" test-convert
  s" 2024 KS167" s" K24KG7S" test-convert

  \ Short provisional (no cycle number)
  s" 2024 GT" s" K24G00T" test-convert
  s" K24G00T" s" 2024 GT" test-convert

  \ ============================================================
  \ Extended provisional (cycle >= 620)
  \ ============================================================
  s" _OA004S" s" 2024 AB631" test-convert
  s" 2024 AB631" s" _OA004S" test-convert

  \ ============================================================
  \ Survey designations
  \ ============================================================
  \ Palomar-Leiden Survey
  s" PLS2040" s" 2040 P-L" test-convert
  s" 2040 P-L" s" PLS2040" test-convert
  s" PLS1234" s" 1234 P-L" test-convert
  s" 1234 P-L" s" PLS1234" test-convert

  \ Trojan surveys
  s" T1S2040" s" 2040 T-1" test-convert
  s" 2040 T-1" s" T1S2040" test-convert
  s" T2S3138" s" 3138 T-2" test-convert
  s" 3138 T-2" s" T2S3138" test-convert
  s" T3S1000" s" 1000 T-3" test-convert
  s" 1000 T-3" s" T3S1000" test-convert

  \ ============================================================
  \ Numbered comets
  \ ============================================================
  s" 0001P" s" 1P" test-convert
  s" 1P" s" 0001P" test-convert
  s" 0002P" s" 2P" test-convert
  s" 2P" s" 0002P" test-convert
  s" 0354P" s" 354P" test-convert
  s" 354P" s" 0354P" test-convert

  \ Defunct comets
  s" 0003D" s" 3D" test-convert
  s" 3D" s" 0003D" test-convert

  \ ============================================================
  \ Comet provisional designations
  \ ============================================================
  s" CJ95O010" s" C/1995 O1" test-convert
  s" C/1995 O1" s" CJ95O010" test-convert
  s" PK24F010" s" P/2024 F1" test-convert
  s" P/2024 F1" s" PK24F010" test-convert
  s" DJ93F020" s" D/1993 F2" test-convert
  s" D/1993 F2" s" DJ93F020" test-convert

  \ Comet with fragment
  s" DJ93F02b" s" D/1993 F2-B" test-convert
  s" D/1993 F2-B" s" DJ93F02b" test-convert

  \ ============================================================
  \ Natural satellites
  \ ============================================================
  s" SK19S220" s" S/2019 S 22" test-convert
  s" S/2019 S 22" s" SK19S220" test-convert
  s" SK00J010" s" S/2000 J 1" test-convert
  s" S/2000 J 1" s" SK00J010" test-convert

  \ Different planets
  s" SK19U010" s" S/2019 U 1" test-convert
  s" S/2019 U 1" s" SK19U010" test-convert
  s" SK19N010" s" S/2019 N 1" test-convert
  s" S/2019 N 1" s" SK19N010" test-convert

  \ ============================================================
  \ Results
  \ ============================================================
  cr
  ." Passed: " pass-count @ . cr
  ." Failed: " fail-count @ . cr
  fail-count @ 0= if
    ." All tests passed!" cr
  else
    1 (bye)
  then ;

run-tests
bye
