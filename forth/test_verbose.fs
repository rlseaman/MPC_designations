require src/mpc_designation.fs

variable pass-count
variable fail-count
variable test-num

256 constant TEST-BUF-LEN
create expected-buf TEST-BUF-LEN allot
variable expected-len

: test-convert ( input-addr input-len expected-addr expected-len -- )
  1 test-num +!
  dup expected-len !
  expected-buf swap move
  2dup type ."  -> "
  convert-simple
  2dup type
  dup expected-len @ = if
    expected-buf expected-len @ s= if
      1 pass-count +!
      ."  OK" cr
    else
      1 fail-count +!
      ."  FAIL (expected " expected-buf expected-len @ type ." )" cr
    then
  else
    1 fail-count +!
    ."  FAIL length" cr
    2drop
  then ;

: run-tests
  0 pass-count ! 0 fail-count ! 0 test-num !

  \ Run only tests 48-55 to find the failure
  \ First skip 47 tests worth of stack items by running them
  
  \ Extended provisional
  s" _OA004S" s" 2024 AB631" test-convert
  s" 2024 AB631" s" _OA004S" test-convert

  \ Survey
  s" PLS2040" s" 2040 P-L" test-convert
  s" 2040 P-L" s" PLS2040" test-convert
  s" PLS1234" s" 1234 P-L" test-convert
  s" 1234 P-L" s" PLS1234" test-convert
  s" T1S2040" s" 2040 T-1" test-convert
  s" 2040 T-1" s" T1S2040" test-convert
  s" T2S3138" s" 3138 T-2" test-convert
  s" 3138 T-2" s" T2S3138" test-convert
  s" T3S1000" s" 1000 T-3" test-convert
  s" 1000 T-3" s" T3S1000" test-convert
  
  cr ." Passed: " pass-count @ . ." Failed: " fail-count @ . cr
;

run-tests
bye
