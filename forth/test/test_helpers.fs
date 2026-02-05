\ test_helpers.fs - Test helper functions in MPC Designation Converter
\
\ Tests format conversion (minimal <-> 12-char report format),
\ fragment extraction, and designation comparison functions.

require ../src/mpc_designation.fs

variable pass-count
variable fail-count
variable test-num

256 constant TEST-BUF-LEN
create expected-buf TEST-BUF-LEN allot
variable expected-len

: test-to-report ( input-addr input-len expected-addr expected-len -- )
  1 test-num +!
  dup expected-len !
  expected-buf swap move
  to-report-format
  dup expected-len @ = if
    expected-buf expected-len @ s= if
      1 pass-count +!
      ." PASS: to-report-format" cr
    else
      1 fail-count +!
      ." FAIL #" test-num @ . ." : content mismatch" cr
    then
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : length mismatch" cr
    2drop
  then ;

: test-from-report ( input-addr input-len expected-addr expected-len -- )
  1 test-num +!
  dup expected-len !
  expected-buf swap move
  from-report-format
  dup expected-len @ = if
    expected-buf expected-len @ s= if
      1 pass-count +!
      ." PASS: from-report-format" cr
    else
      1 fail-count +!
      ." FAIL #" test-num @ . ." : content mismatch" cr
    then
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : length mismatch" cr
    2drop
  then ;

: test-has-fragment ( input-addr input-len expected-f -- )
  1 test-num +!
  -rot has-fragment
  = if
    1 pass-count +!
    ." PASS: has-fragment" cr
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : has-fragment mismatch" cr
  then ;

: test-get-fragment ( input-addr input-len expected-addr expected-len -- )
  1 test-num +!
  dup expected-len !
  expected-buf swap move
  get-fragment
  dup expected-len @ = if
    expected-len @ 0= if
      drop
      1 pass-count +!
      ." PASS: get-fragment (empty)" cr
    else
      expected-buf expected-len @ s= if
        1 pass-count +!
        ." PASS: get-fragment" cr
      else
        1 fail-count +!
        ." FAIL #" test-num @ . ." : get-fragment content mismatch" cr
      then
    then
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : get-fragment length mismatch" cr
    2drop
  then ;

: test-get-parent ( input-addr input-len expected-addr expected-len -- )
  1 test-num +!
  dup expected-len !
  expected-buf swap move
  get-parent
  dup expected-len @ = if
    expected-buf expected-len @ s= if
      1 pass-count +!
      ." PASS: get-parent" cr
    else
      1 fail-count +!
      ." FAIL #" test-num @ . ." : get-parent content mismatch" cr
    then
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : get-parent length mismatch" cr
    2drop
  then ;

: test-equal ( addr1 len1 addr2 len2 expected-f -- )
  1 test-num +!
  >r
  designations-equal
  r> = if
    1 pass-count +!
    ." PASS: designations-equal" cr
  else
    1 fail-count +!
    ." FAIL #" test-num @ . ." : designations-equal mismatch" cr
  then ;

: run-tests
  0 pass-count !
  0 fail-count !
  0 test-num !

  ." === MPC Designation Helper Function Tests (Forth) ===" cr cr

  \ ============================================================
  \ to-report-format tests
  \ ============================================================
  ." --- to-report-format ---" cr

  \ Numbered asteroids
  s" 00001" s"        00001" test-to-report
  s" 00433" s"        00433" test-to-report
  s" 99999" s"        99999" test-to-report
  s" A0000" s"        A0000" test-to-report
  s" ~0000" s"        ~0000" test-to-report

  \ Provisional asteroids
  s" J95X00A" s"      J95X00A" test-to-report
  s" K24A12B" s"      K24A12B" test-to-report

  \ Survey designations
  s" PLS2040" s"      PLS2040" test-to-report
  s" T3S3141" s"      T3S3141" test-to-report

  \ Numbered comets
  s" 0001P" s" 0001P       " test-to-report
  s" 0073P" s" 0073P       " test-to-report

  \ Numbered comets with fragments
  s" 0073Pa" s" 0073P      a" test-to-report
  s" 0073Pb" s" 0073P      b" test-to-report
  s" 0073Paa" s" 0073P     aa" test-to-report
  s" 0073Paz" s" 0073P     az" test-to-report
  s" 0073Pzz" s" 0073P     zz" test-to-report

  \ Provisional comets
  s" CJ95O010" s"     CJ95O010" test-to-report
  s" DJ93F020" s"     DJ93F020" test-to-report
  s" DJ93F02a" s"     DJ93F02a" test-to-report

  \ ============================================================
  \ from-report-format tests
  \ ============================================================
  cr ." --- from-report-format ---" cr

  \ Numbered asteroids
  s"        00001" s" 00001" test-from-report
  s"        00433" s" 00433" test-from-report
  s"        A0000" s" A0000" test-from-report

  \ Provisional asteroids
  s"      J95X00A" s" J95X00A" test-from-report

  \ Numbered comets
  s" 0073P       " s" 0073P" test-from-report

  \ Numbered comets with fragments
  s" 0073P      a" s" 0073Pa" test-from-report
  s" 0073P     aa" s" 0073Paa" test-from-report
  s" 0073P     az" s" 0073Paz" test-from-report

  \ Provisional comets
  s"     CJ95O010" s" CJ95O010" test-from-report

  \ ============================================================
  \ has-fragment tests
  \ ============================================================
  cr ." --- has-fragment ---" cr

  \ Unpacked with fragments
  s" 73P-A" true test-has-fragment
  s" 73P-AA" true test-has-fragment
  s" D/1993 F2-A" true test-has-fragment
  s" P/1930 J1-AA" true test-has-fragment

  \ Unpacked without fragments
  s" 73P" false test-has-fragment
  s" C/1995 O1" false test-has-fragment

  \ Packed with fragments
  s" 0073Pa" true test-has-fragment
  s" 0073Paa" true test-has-fragment
  s" DJ93F02a" true test-has-fragment

  \ Packed without fragments
  s" 0073P" false test-has-fragment
  s" CJ95O010" false test-has-fragment

  \ Non-comets
  s" 1995 XA" false test-has-fragment
  s" 00001" false test-has-fragment

  \ ============================================================
  \ get-fragment tests
  \ ============================================================
  cr ." --- get-fragment ---" cr

  \ Unpacked with fragments
  s" 73P-A" s" A" test-get-fragment
  s" 73P-AA" s" AA" test-get-fragment
  s" 73P-I" s" I" test-get-fragment
  s" D/1993 F2-B" s" B" test-get-fragment
  s" P/1930 J1-AZ" s" AZ" test-get-fragment

  \ Unpacked without fragments
  s" 73P" s" " test-get-fragment
  s" C/1995 O1" s" " test-get-fragment

  \ Packed with fragments
  s" 0073Pa" s" A" test-get-fragment
  s" 0073Paa" s" AA" test-get-fragment
  s" 0073Pi" s" I" test-get-fragment
  s" DJ93F02b" s" B" test-get-fragment

  \ Packed without fragments
  s" 0073P" s" " test-get-fragment
  s" CJ95O010" s" " test-get-fragment

  \ ============================================================
  \ get-parent tests
  \ ============================================================
  cr ." --- get-parent ---" cr

  \ Unpacked with fragments
  s" 73P-A" s" 73P" test-get-parent
  s" 73P-AA" s" 73P" test-get-parent
  s" D/1993 F2-B" s" D/1993 F2" test-get-parent
  s" P/1930 J1-AA" s" P/1930 J1" test-get-parent

  \ Unpacked without fragments
  s" 73P" s" 73P" test-get-parent
  s" C/1995 O1" s" C/1995 O1" test-get-parent

  \ Packed with fragments
  s" 0073Pa" s" 0073P" test-get-parent
  s" 0073Paa" s" 0073P" test-get-parent

  \ Packed without fragments
  s" 0073P" s" 0073P" test-get-parent

  \ Non-comets (should return as-is)
  s" 1995 XA" s" 1995 XA" test-get-parent
  s" 00001" s" 00001" test-get-parent

  \ ============================================================
  \ designations-equal tests
  \ ============================================================
  cr ." --- designations-equal ---" cr

  \ Same designation, different formats
  s" 1995 XA" s" J95X00A" true test-equal
  s" 73P" s" 0073P" true test-equal
  s" 73P-A" s" 0073Pa" true test-equal
  s" 73P-AA" s" 0073Paa" true test-equal
  s" 1" s" 00001" true test-equal
  s" C/1995 O1" s" CJ95O010" true test-equal

  \ Different designations
  s" 1995 XA" s" 1995 XB" false test-equal
  s" 73P-A" s" 73P-B" false test-equal
  s" 73P" s" 74P" false test-equal
  s" 1" s" 2" false test-equal

  \ Same designation (both packed or both unpacked)
  s" 1995 XA" s" 1995 XA" true test-equal
  s" J95X00A" s" J95X00A" true test-equal

  \ ============================================================
  \ Results
  \ ============================================================
  cr
  ." ==================================================" cr
  ." Total: " pass-count @ fail-count @ + . ." , "
  ." Passed: " pass-count @ . ." , "
  ." Failed: " fail-count @ . cr
  fail-count @ 0= if
    ." All tests passed!" cr
  else
    1 (bye)
  then ;

run-tests
bye
