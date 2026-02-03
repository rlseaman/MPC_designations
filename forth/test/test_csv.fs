\ test_csv.fs - CSV test suite for MPC Designation Converter
\
\ Usage: gforth test/test_csv.fs <csv_file>
\ Tests pack direction: unpacked -> packed
\
\ Note: Requires decompressed CSV file (not .gz)

require ../src/mpc_designation.fs

\ Buffer for reading lines
1024 constant LINE-BUF-LEN
create line-buf LINE-BUF-LEN allot

\ Buffer for input value (copy from line-buf to avoid aliasing)
256 constant INPUT-BUF-LEN
create input-buf INPUT-BUF-LEN allot
variable input-len

\ Buffer for expected value
256 constant EXPECT-BUF-LEN
create expect-buf EXPECT-BUF-LEN allot
variable expect-len

\ Counters
variable total-count
variable pass-count
variable fail-count

\ Timing
variable start-time
variable end-time

\ File handle
variable csv-fid

\ Parsing variables
variable line-length
variable comma-addr

\ Get current time in milliseconds (gforth-specific)
: ms@ utime 1000 um/mod nip ;

\ Test a single line
: test-line ( addr len -- )
  line-length !
  \ Find comma
  0 over line-length @ bounds ?do
    i c@ [char] , = if drop i leave then
  loop
  comma-addr !
  comma-addr @ 0= if drop 1 fail-count +! exit then
  \ Copy unpacked to input-buf
  comma-addr @ over - input-len !
  dup input-buf input-len @ move
  \ Copy expected to expect-buf
  dup line-length @ + comma-addr @ 1+ - expect-len !
  comma-addr @ 1+ expect-buf expect-len @ move
  drop
  \ Convert
  input-buf input-len @ convert-simple
  \ Compare
  dup expect-len @ = if
    expect-buf expect-len @ s= if 1 pass-count +! else 1 fail-count +! then
  else
    2drop 1 fail-count +!
  then ;

\ Process CSV file
: process-csv ( -- )
  \ Skip header line
  line-buf LINE-BUF-LEN csv-fid @ read-line drop 2drop

  begin
    line-buf LINE-BUF-LEN csv-fid @ read-line drop
  while
    \ Stack: line-len
    dup 0> if
      1 total-count +!
      line-buf swap test-line
      \ Progress every 100,000
      total-count @ 100000 mod 0= if
        ." Processed " total-count @ . ." entries..." cr
      then
    else
      drop
    then
  repeat ;

\ Main
: run-csv-test ( addr len -- )
  r/o open-file throw csv-fid !

  0 total-count !
  0 pass-count !
  0 fail-count !

  ms@ start-time !

  process-csv

  ms@ end-time !

  csv-fid @ close-file drop

  \ Results
  cr
  ." === Results ===" cr
  ." Passed: " pass-count @ . ." / " total-count @ . cr
  ." Failed: " fail-count @ . cr

  end-time @ start-time @ -
  dup ." Time: " . ." ms ("
  total-count @ 1000 * swap /
  . ." entries/sec)" cr

  cr
  fail-count @ 0= if
    ." All tests passed!" cr
  else
    1 (bye)
  then ;

\ Command line handling
: main
  argc @ 2 < if
    ." Usage: gforth test/test_csv.fs <csv_file>" cr
    1 (bye)
  then
  1 arg run-csv-test
  bye ;

main
