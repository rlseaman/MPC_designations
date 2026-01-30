\ mpc_designation.fs - MPC Designation Converter for Gforth (Minimal Version)
\
\ Handles: numbered asteroids, basic provisional designations
\ Uses gforth's built-in number parsing for simplicity

256 constant MAX-LEN
create out-buf MAX-LEN allot
create tmp-buf MAX-LEN allot

\ ============================================================================
\ Helpers
\ ============================================================================

: is-digit ( c -- f ) [char] 0 [char] 9 1+ within ;
: is-upper ( c -- f ) [char] A [char] Z 1+ within ;
: is-lower ( c -- f ) [char] a [char] z 1+ within ;

: s= ( a1 l1 a2 l2 -- f ) compare 0= ;

: all-digit? ( addr len -- f )
  dup 0= if 2drop false exit then
  true -rot
  bounds ?do
    i c@ is-digit 0= if drop false leave then
  loop ;

: has-space? ( addr len -- f )
  false -rot
  bounds ?do
    i c@ bl = if drop true leave then
  loop ;

\ Parse number from string using gforth's >number
: str>num ( addr len -- n )
  0. 2swap >number 2drop drop ;

\ ============================================================================
\ Permanent asteroid: pack/unpack
\ ============================================================================

: unpack-perm ( addr len -- addr' len' )
  \ Handle ~xxxx format (>=620000)
  over c@ [char] ~ = if
    \ Base-62 decode the 4 chars after ~
    \ Stack: addr len -> addr len accum
    0
    2 pick 1+ c@ dup is-digit if [char] 0 - else
      dup is-upper if 55 - else 61 - then then
    62 * +
    2 pick 2 + c@ dup is-digit if [char] 0 - else
      dup is-upper if 55 - else 61 - then then
    62 * +
    2 pick 3 + c@ dup is-digit if [char] 0 - else
      dup is-upper if 55 - else 61 - then then
    62 * +
    2 pick 4 + c@ dup is-digit if [char] 0 - else
      dup is-upper if 55 - else 61 - then then
    +
    620000 +
    -rot 2drop    \ drop addr len, keep number
  else
    \ Handle letter prefix or pure digits
    over c@ dup is-upper if
      55 - 10000 *
      2 pick 1+ 4 str>num +
      nip nip
    else dup is-lower if
      61 - 10000 *
      2 pick 1+ 4 str>num +
      nip nip
    else
      drop str>num
    then then
  then
  \ Convert number to string
  s>d <# #s #> out-buf swap 2dup 2>r move 2r> ;

: pack-perm ( addr len -- addr' len' )
  str>num
  dup 100000 < if
    \ 5-digit format
    s>d <# # # # # # #> out-buf swap 2dup 2>r move 2r>
  else dup 620000 < if
    \ Letter prefix format
    dup 10000 / dup 36 < if 55 + else 61 + then
    out-buf c!
    10000 mod
    s>d <# # # # # #> drop 4 out-buf 1+ swap move
    out-buf 5
  else
    \ Tilde + base62 format
    620000 -
    [char] ~ out-buf c!
    dup 62 62 62 * * / 62 mod
    dup 10 < if [char] 0 + else dup 36 < if 55 + else 61 + then then
    out-buf 1+ c!
    dup 62 62 * / 62 mod
    dup 10 < if [char] 0 + else dup 36 < if 55 + else 61 + then then
    out-buf 2 + c!
    dup 62 / 62 mod
    dup 10 < if [char] 0 + else dup 36 < if 55 + else 61 + then then
    out-buf 3 + c!
    62 mod
    dup 10 < if [char] 0 + else dup 36 < if 55 + else 61 + then then
    out-buf 4 + c!
    out-buf 5
  then then ;

\ ============================================================================
\ Provisional: basic pack/unpack
\ ============================================================================

: century>num ( c -- n|-1 )
  case
    [char] I of 18 endof
    [char] J of 19 endof
    [char] K of 20 endof
    [char] L of 21 endof
    -1 swap
  endcase ;

: num>century ( n -- c|0 )
  case
    18 of [char] I endof
    19 of [char] J endof
    20 of [char] K endof
    21 of [char] L endof
    0 swap
  endcase ;

: decode-cycle ( addr -- n )
  \ Decode 2-char cycle: first char base-62, second char digit 0-9
  dup c@ dup is-digit if [char] 0 - else
    dup is-upper if 55 - else 61 - then then
  10 *
  swap 1+ c@ [char] 0 - + ;

: encode-cycle ( n addr -- )
  \ Encode cycle count: first char base-62 (n/10), second char digit (n%10)
  over 10 / dup 10 < if [char] 0 + else
    dup 36 < if 55 + else 61 + then then
  over c!
  swap 10 mod [char] 0 + swap 1+ c! ;

: unpack-prov ( addr len -- addr' len' )
  \ Format: CyyHooL -> yyyy HL[n]
  \ Stack: addr len
  over c@ century>num 100 *       \ addr len year-partial
  2 pick 1+ c@ [char] 0 - 10 * +  \ addr len year-partial
  2 pick 2 + c@ [char] 0 - +      \ addr len year
  \ Year in stack, format it
  s>d <# # # # # #> tmp-buf swap move  \ addr len (year written to tmp-buf)
  bl tmp-buf 4 + c!
  over 3 + c@ tmp-buf 5 + c!      \ half-month (over=addr)
  over 6 + c@ tmp-buf 6 + c!      \ second letter
  over 4 + decode-cycle           \ addr len cycle
  dup 0= if
    drop tmp-buf 7
  else
    s>d <# #s #> tmp-buf 7 + swap 2dup 2>r move 2r>
    7 + tmp-buf swap
  then
  2swap 2drop ;

: pack-prov ( addr len -- addr' len' )
  \ Parse year - stack: addr len
  over 4 str>num               \ addr len year
  dup 100 / num>century out-buf c!
  100 mod
  dup 10 / [char] 0 + out-buf 1+ c!
  10 mod [char] 0 + out-buf 2 + c!  \ addr len
  \ Half-month and second letter
  over 5 + c@ out-buf 3 + c!     \ addr=over
  over 6 + c@ out-buf 6 + c!
  \ Cycle count (if any)
  dup 7 > if
    over 7 + over 7 - str>num  \ addr len cycle
  else
    0                            \ addr len 0
  then
  out-buf 4 + encode-cycle
  out-buf 7
  2swap 2drop ;

\ ============================================================================
\ Format detection and conversion
\ ============================================================================

: packed-perm? ( addr len -- f )
  nip 5 = ;

: packed-prov? ( addr len -- f )
  dup 7 = if
    drop c@ century>num 0>=
  else
    2drop false
  then ;

: unpacked-perm? ( addr len -- f )
  all-digit? ;

: unpacked-prov? ( addr len -- f )
  has-space? ;

: convert-simple ( addr len -- addr' len' )
  2dup packed-perm? if unpack-perm exit then
  2dup packed-prov? if unpack-prov exit then
  2dup unpacked-perm? if pack-perm exit then
  2dup unpacked-prov? if pack-prov exit then
  \ Unknown format - return as-is
  out-buf over move out-buf swap ;

\ ============================================================================
\ CLI support
\ ============================================================================

: process-arg ( addr len -- )
  convert-simple type cr ;

: print-usage
  ." Usage: gforth src/mpc_designation_cli.fs <designation>..." cr
  ." Converts between packed and unpacked MPC designations." cr
  ." Supports: numbered asteroids, provisional designations." cr ;
