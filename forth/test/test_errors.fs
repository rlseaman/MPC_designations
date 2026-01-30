\ test_errors.fs - Basic tests for MPC Designation Converter

require ../src/mpc_designation.fs

variable pass-count
variable fail-count

256 constant TEST-BUF-LEN
create expected-buf TEST-BUF-LEN allot
variable expected-len

: test-convert ( input-addr input-len expected-addr expected-len -- )
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
      ." FAIL: content mismatch" cr
    then
  else
    1 fail-count +!
    ." FAIL: length mismatch, expected " expected-len @ .
    ."  got " dup . cr
    2drop
  then ;

: run-tests
  0 pass-count !
  0 fail-count !

  \ Permanent asteroids - packed to unpacked
  s" 00001" s" 1" test-convert
  s" 00123" s" 123" test-convert
  s" A0001" s" 100001" test-convert
  s" ~0000" s" 620000" test-convert

  \ Permanent asteroids - unpacked to packed
  s" 1" s" 00001" test-convert
  s" 100001" s" A0001" test-convert
  s" 620000" s" ~0000" test-convert

  \ Provisional designations
  s" J95X00A" s" 1995 XA" test-convert
  s" 1995 XA" s" J95X00A" test-convert
  s" K24AB3B" s" 2024 AB113" test-convert
  s" 2024 AB113" s" K24AB3B" test-convert

  ." Passed: " pass-count @ . cr
  ." Failed: " fail-count @ . cr
  fail-count @ 0= if
    ." All tests passed!" cr
  else
    1 (bye)
  then ;

run-tests
bye
