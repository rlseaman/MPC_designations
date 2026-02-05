\ mpc_designation.fs - MPC Designation Converter for Gforth (Full Version)
\
\ Supports: numbered asteroids, provisional, survey, comets, satellites
\ Uses gforth's built-in facilities
\
\ ============================================================================
\ GFORTH 0.7.3 COMPATIBILITY NOTE
\ ============================================================================
\ This code has been tested with gforth 0.7.3 on macOS (Darwin). There is an
\ intermittent "Address alignment exception" bug in gforth 0.7.3 that can occur
\ during code loading/compilation. Key observations:
\
\ 1. The error occurs randomly during startup, not during runtime execution
\ 2. When it occurs, gforth prints a backtrace but often continues execution
\ 3. The error does not affect test correctness - all conversions work correctly
\ 4. The backtrace typically shows 'over' or 'lit' instructions in case statements
\ 5. Memory addresses in the backtrace vary between runs, suggesting the issue
\    is related to memory allocation/layout that changes between executions
\
\ Workaround: The Makefile includes a retry mechanism for the test-csv target.
\ If you experience this issue, simply re-run the tests - the code is correct.
\
\ See GFORTH_NOTES.txt in this directory for more details.
\ ============================================================================

256 constant MAX-LEN
create out-buf MAX-LEN allot
create tmp-buf MAX-LEN allot
create work-buf MAX-LEN allot

\ ============================================================================
\ Character classification helpers
\ ============================================================================

: is-digit ( c -- f ) [char] 0 [char] 9 1+ within ;
: is-upper ( c -- f ) [char] A [char] Z 1+ within ;
: is-lower ( c -- f ) [char] a [char] z 1+ within ;
: is-alpha ( c -- f ) dup is-upper swap is-lower or ;
: is-alnum ( c -- f ) dup is-digit swap is-alpha or ;

: s= ( a1 l1 a2 l2 -- f ) compare 0= ;

\ ============================================================================
\ String predicates
\ ============================================================================

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

: starts-with? ( addr len prefix-addr prefix-len -- f )
  2over 2over nip > if 2drop 2drop false exit then
  tuck 2swap drop swap s= ;

\ Parse number from string using gforth's >number
: str>num ( addr len -- n )
  0. 2swap >number 2drop drop ;

\ ============================================================================
\ Base-62 encoding/decoding
\ ============================================================================

: char>b62 ( c -- n )
  dup is-digit if [char] 0 - exit then
  dup is-upper if 55 - exit then
  61 - ;

: b62>char ( n -- c )
  dup 10 < if [char] 0 + exit then
  dup 36 < if 55 + exit then
  61 + ;

: b62-decode4 ( addr -- n )
  \ Decode 4 base-62 chars to number
  0
  over c@ char>b62 62 62 62 * * * +
  over 1+ c@ char>b62 62 62 * * +
  over 2 + c@ char>b62 62 * +
  swap 3 + c@ char>b62 + ;

: b62-encode4 ( n addr -- )
  \ Encode number to 4 base-62 chars
  over 62 62 62 * * / 62 mod b62>char over c!
  over 62 62 * / 62 mod b62>char over 1+ c!
  over 62 / 62 mod b62>char over 2 + c!
  swap 62 mod b62>char swap 3 + c! ;

\ ============================================================================
\ Century code encoding/decoding
\ ============================================================================

: century>num ( c -- n|-1 )
  case
    [char] A of 10 endof
    [char] B of 11 endof
    [char] C of 12 endof
    [char] D of 13 endof
    [char] E of 14 endof
    [char] F of 15 endof
    [char] G of 16 endof
    [char] H of 17 endof
    [char] I of 18 endof
    [char] J of 19 endof
    [char] K of 20 endof
    [char] L of 21 endof
    -1 swap
  endcase ;

: num>century ( n -- c|0 )
  case
    10 of [char] A endof
    11 of [char] B endof
    12 of [char] C endof
    13 of [char] D endof
    14 of [char] E endof
    15 of [char] F endof
    16 of [char] G endof
    17 of [char] H endof
    18 of [char] I endof
    19 of [char] J endof
    20 of [char] K endof
    21 of [char] L endof
    0 swap
  endcase ;

\ ============================================================================
\ BCE year encoding/decoding (for ancient comets)
\ ============================================================================

variable bce-prefix
variable bce-code

: encode-bce-year ( year -- prefix code )
  \ Input: negative astronomical year (e.g., -119)
  \ Output: prefix char (/ . -) and 2-digit code
  negate 1+                           \ convert to BC year (1-indexed)
  dup 100 <= if
    [char] / swap 100 swap -          \ 1-100 BC: /, 100-bc
  else dup 200 <= if
    [char] . swap 200 swap -          \ 101-200 BC: ., 200-bc
  else
    [char] - swap 300 swap -          \ 201-300 BC: -, 300-bc
  then then ;

: decode-bce-year ( prefix code -- year )
  \ Input: prefix char and 2-digit code
  \ Output: negative astronomical year
  swap case
    [char] / of 100 swap - endof      \ 1-100 BC
    [char] . of 200 swap - endof      \ 101-200 BC
    [char] - of 300 swap - endof      \ 201-300 BC
    drop 0 swap                       \ invalid
  endcase
  1- negate ;                         \ convert BC to astronomical year

: is-bce-prefix? ( c -- f )
  dup [char] / = swap dup [char] . = swap [char] - = or or ;

\ ============================================================================
\ Cycle count encoding/decoding (for provisional designations)
\ ============================================================================

: decode-cycle ( addr -- n )
  \ Decode 2-char cycle: first char base-62, second char digit 0-9
  dup c@ char>b62 10 *
  swap 1+ c@ [char] 0 - + ;

: encode-cycle ( n addr -- )
  \ Encode cycle count: first char base-62 (n/10), second char digit (n%10)
  over 10 / b62>char over c!
  swap 10 mod [char] 0 + swap 1+ c! ;

\ ============================================================================
\ Letter position utilities (for provisional second letters)
\ ============================================================================

: letter>pos ( c -- n )
  \ A=1, B=2, ..., H=8, J=9, ..., Z=25 (skip I)
  [char] A -
  dup 8 >= if 1- then
  1+ ;

: pos>letter ( n -- c )
  \ 1=A, 2=B, ..., 8=H, 9=J, ..., 25=Z (skip I)
  1-
  dup 8 >= if 1+ then
  [char] A + ;

\ ============================================================================
\ Permanent asteroid: pack/unpack
\ ============================================================================

: unpack-perm ( addr len -- addr' len' )
  \ Handle ~xxxx format (>=620000)
  over c@ [char] ~ = if
    over 1+ b62-decode4 620000 +
    nip nip
    s>d <# #s #> out-buf swap 2dup 2>r move 2r>
    exit
  then
  \ Handle letter prefix or pure digits
  over c@ dup is-upper if
    55 - 10000 *
    2 pick 1+ 4 str>num +
    nip nip
    s>d <# #s #> out-buf swap 2dup 2>r move 2r>
  else dup is-lower if
    61 - 10000 *
    2 pick 1+ 4 str>num +
    nip nip
    s>d <# #s #> out-buf swap 2dup 2>r move 2r>
  else
    drop str>num
    s>d <# #s #> out-buf swap 2dup 2>r move 2r>
  then then ;

: pack-perm ( addr len -- addr' len' )
  str>num
  dup 100000 < if
    s>d <# # # # # # #> out-buf swap 2dup 2>r move 2r>
  else dup 620000 < if
    dup 10000 / b62>char out-buf c!
    10000 mod
    s>d <# # # # # #> drop 4 out-buf 1+ swap move
    out-buf 5
  else
    620000 -
    [char] ~ out-buf c!
    out-buf 1+ b62-encode4
    out-buf 5
  then then ;

\ ============================================================================
\ Survey designations (P-L, T-1, T-2, T-3)
\ ============================================================================

: is-survey-packed? ( addr len -- f )
  dup 7 <> if 2drop false exit then
  drop
  dup 3 s" PLS" s= if drop true exit then
  dup 3 s" T1S" s= if drop true exit then
  dup 3 s" T2S" s= if drop true exit then
  3 s" T3S" s= ;

: is-survey-unpacked? ( addr len -- f )
  \ Check for "nnnn P-L" or "nnnn T-n" format
  dup 6 < if 2drop false exit then
  \ Find space
  2dup + 1- c@ [char] L = if
    2dup + 3 - c@ [char] P = if
      2dup + 2 - c@ [char] - = if
        2drop true exit
      then
    then
  then
  2dup + 1- c@ is-digit if
    2dup + 2 - c@ [char] - = if
      2dup + 3 - c@ [char] T = if
        2drop true exit
      then
    then
  then
  2drop false ;

variable survey-num
variable survey-addr
variable survey-len
variable survey-digit

: unpack-survey ( addr len -- addr' len' )
  \ Format: PLSnnnn -> nnnn P-L, TnSnnnn -> nnnn T-n
  over c@ [char] P = if
    \ Palomar-Leiden: PLSnnnn -> nnnn P-L
    over 3 + 4 str>num survey-num !
    survey-num @ s>d <# #s #>    \ convert to string: addr len
    dup survey-len !             \ save length
    out-buf swap move            \ copy number to out-buf
    survey-len @                 \ get saved length (index)
    bl over out-buf + c!         \ add space at out-buf+idx
    1+
    [char] P over out-buf + c!   \ add P
    1+
    [char] - over out-buf + c!   \ add -
    1+
    [char] L over out-buf + c!   \ add L
    1+
    out-buf swap
    2swap 2drop
  else
    \ Trojan: TnSnnnn -> nnnn T-n
    over 1+ c@ survey-digit !    \ save survey digit (1, 2, or 3)
    over 3 + 4 str>num survey-num !
    survey-num @ s>d <# #s #>    \ convert to string: addr len
    dup survey-len !             \ save length
    out-buf swap move            \ copy number to out-buf
    survey-len @                 \ get saved length (index)
    bl over out-buf + c!         \ add space
    1+
    [char] T over out-buf + c!   \ add T
    1+
    [char] - over out-buf + c!   \ add -
    1+
    survey-digit @ over out-buf + c!  \ add digit
    1+
    out-buf swap
    2swap 2drop
  then ;

: pack-survey ( addr len -- addr' len' )
  \ Parse: "nnnn P-L" or "nnnn T-n"
  dup survey-len ! dup >r
  over survey-addr !
  \ Find the space to get number length
  0 survey-addr @ r> bounds ?do
    i c@ bl = if drop i survey-addr @ - leave then
  loop
  \ Parse the number
  survey-addr @ over str>num survey-num !
  drop
  \ Check last 3 chars for survey type
  survey-addr @ survey-len @ + 3 - 3
  2dup s" P-L" s= if
    2drop
    s" PLS" out-buf swap move
    survey-num @ s>d <# # # # # #> out-buf 3 + swap move
    out-buf 7
  else 2dup s" T-1" s= if
    2drop
    s" T1S" out-buf swap move
    survey-num @ s>d <# # # # # #> out-buf 3 + swap move
    out-buf 7
  else 2dup s" T-2" s= if
    2drop
    s" T2S" out-buf swap move
    survey-num @ s>d <# # # # # #> out-buf 3 + swap move
    out-buf 7
  else s" T-3" s= if
    s" T3S" out-buf swap move
    survey-num @ s>d <# # # # # #> out-buf 3 + swap move
    out-buf 7
  else
    2drop out-buf 0
  then then then then ;

\ ============================================================================
\ Provisional asteroid designations
\ ============================================================================

variable prov-year
variable prov-cycle
variable prov-outlen

: unpack-prov ( addr len -- addr' len' )
  \ Format: CyyHooL -> yyyy HL[n] or A908 CJ for pre-1925
  \ Calculate year
  over c@ century>num 100 *
  2 pick 1+ c@ [char] 0 - 10 * +
  2 pick 2 + c@ [char] 0 - +
  prov-year !
  \ Get cycle count
  over 4 + decode-cycle prov-cycle !
  \ Check for pre-1925 A-prefix format
  \ Note: Only 'A' prefix is used by MPC (years 1800-1924)
  prov-year @ 1925 < if
    \ A-prefix format: A908 CJ instead of 1908 CJ
    [char] A out-buf c!
    \ 3-digit year remainder (e.g., 908 for 1908)
    prov-year @ 1000 mod
    s>d <# # # # #> drop 3 out-buf 1+ swap move
    \ Space
    bl out-buf 4 + c!
    \ Half-month letter
    over 3 + c@ out-buf 5 + c!
    \ Second letter
    over 6 + c@ out-buf 6 + c!
    \ Cycle count (if non-zero)
    prov-cycle @ dup 0= if
      drop 7 prov-outlen !
    else
      s>d <# #s #>
      dup prov-outlen !
      out-buf 7 + swap move
      prov-outlen @ 7 + prov-outlen !
    then
  else
    \ Standard format
    \ Year (4 digits)
    prov-year @ s>d <# # # # # #> out-buf swap move
    \ Space
    bl out-buf 4 + c!
    \ Half-month letter
    over 3 + c@ out-buf 5 + c!
    \ Second letter
    over 6 + c@ out-buf 6 + c!
    \ Cycle count (if non-zero)
    prov-cycle @ dup 0= if
      drop 7 prov-outlen !
    else
      s>d <# #s #>                  \ addr len of cycle string
      dup prov-outlen !             \ save cycle string length
      out-buf 7 + swap move         \ copy cycle to out-buf+7
      prov-outlen @ 7 + prov-outlen !
    then
  then
  out-buf prov-outlen @
  2swap 2drop ;

: pack-prov-std ( addr len -- addr' len' )
  \ Standard format: yyyy HL[n] -> CyyHooL
  over 4 str>num               \ year
  dup 100 / num>century out-buf c!
  100 mod
  dup 10 / [char] 0 + out-buf 1+ c!
  10 mod [char] 0 + out-buf 2 + c!
  \ Half-month and second letter
  over 5 + c@ out-buf 3 + c!
  over 6 + c@ out-buf 6 + c!
  \ Cycle count (if any)
  dup 7 > if
    over 7 + over 7 - str>num
  else
    0
  then
  \ Check for extended format (cycle >= 620)
  dup 620 >= if
    \ Extended format: _YHbbbb
    \ Stack: addr len cycle
    \ Year code (base-62 of year % 100)
    2 pick 4 str>num 100 mod b62>char
    [char] _ out-buf c!
    out-buf 1+ c!
    \ Stack: addr len cycle
    2 pick 5 + c@ out-buf 2 + c!  \ half-month
    \ Stack: addr len cycle
    \ Calculate base sequence: (cycle-620)*25 + letter_pos - 1
    620 - 25 *                    \ addr len partial_seq
    2 pick 6 + c@ letter>pos + 1- \ addr len sequence
    out-buf 3 + b62-encode4
    out-buf 7
    2swap 2drop
  else
    out-buf 4 + encode-cycle
    out-buf 7
    2swap 2drop
  then ;

: is-old-style? ( addr len -- f )
  \ Check for A-prefix format (7 characters): A908 CJ, A873 OA, etc.
  \ Note: Only 'A' prefix is used by MPC for pre-1925 designations (years 1800-1924)
  7 <> if drop false exit then
  dup c@ [char] A <> if drop false exit then
  dup 4 + c@ bl <> if drop false exit then
  drop true ;

: pack-prov-old ( addr len -- addr' len' )
  \ A-prefix format: A908 CJ -> I08C00J, A900 DA -> J00D00A
  \ Only 'A' prefix is used by MPC for pre-1925 designations (years 1800-1924)
  \ Century = 10 (for A=1xxx) + hundreds digit
  \ Stack: addr len
  10                            \ A prefix means millennium 1 (1xxx)
  2 pick 1+ c@ [char] 0 - +    \ add hundreds digit to get century
  num>century out-buf c!
  over 2 + c@ out-buf 1+ c!     \ decade
  over 3 + c@ out-buf 2 + c!    \ year
  over 5 + c@ out-buf 3 + c!    \ half-month
  [char] 0 out-buf 4 + c!
  [char] 0 out-buf 5 + c!
  over 6 + c@ out-buf 6 + c!    \ second letter
  out-buf 7
  2swap 2drop ;

: pack-prov ( addr len -- addr' len' )
  2dup is-survey-unpacked? if pack-survey exit then
  2dup is-old-style? if pack-prov-old exit then
  pack-prov-std ;

\ ============================================================================
\ Extended provisional format (_YHbbbb)
\ ============================================================================

: is-extended-packed? ( addr len -- f )
  dup 7 <> if 2drop false exit then
  drop c@ [char] _ = ;

variable ext-year
variable ext-cycle
variable ext-letter
variable ext-outlen

: unpack-extended ( addr len -- addr' len' )
  \ Format: _YHbbbb -> yyyy HL[n]
  \ Calculate year
  over 1+ c@ char>b62 2000 + ext-year !
  \ Get half-month letter (position 2)
  over 2 + c@ ext-letter !
  \ Decode sequence (positions 3-6)
  over 3 + b62-decode4
  dup 25 / 620 + ext-cycle !
  25 mod 1+ pos>letter         \ second letter
  \ Build output in out-buf
  ext-year @ s>d <# # # # # #> out-buf swap move
  bl out-buf 4 + c!
  ext-letter @ out-buf 5 + c!  \ half-month
  out-buf 6 + c!               \ second letter (was on stack)
  \ Add cycle number
  ext-cycle @ s>d <# #s #>
  out-buf 7 + swap dup ext-outlen ! move
  ext-outlen @ 7 +
  2swap 2drop
  out-buf swap ;

\ ============================================================================
\ Numbered comet designations (1P, 354P)
\ ============================================================================

: is-comet-numbered-packed? ( addr len -- f )
  \ Check for 5-7 char format: 0001P, 0073Pa, 0073Paa
  dup 5 < over 7 > or if 2drop false exit then
  over 4 + c@ dup [char] P = swap [char] D = or 0= if 2drop false exit then
  over 4 all-digit? 0= if 2drop false exit then
  \ Check fragment suffix if present (must be lowercase)
  dup 5 > if
    over 5 + c@ is-lower 0= if 2drop false exit then
    dup 7 = if
      over 6 + c@ is-lower 0= if 2drop false exit then
    then
  then
  2drop true ;

variable comet-dash-pos
variable comet-check-addr
variable comet-check-len

: is-comet-numbered-unpacked? ( addr len -- f )
  dup 2 < if 2drop false exit then
  comet-check-len ! comet-check-addr !
  \ Find dash position if any
  0 comet-dash-pos !
  comet-check-addr @ comet-check-len @ bounds ?do
    i c@ [char] - = if i comet-dash-pos ! leave then
  loop
  comet-dash-pos @ if
    \ Has dash - check format: digits + P/D + dash + letters
    comet-dash-pos @ comet-check-addr @ -  \ dash offset from addr
    dup 2 < if drop false exit then        \ need at least 2 chars before dash (e.g. "1P")
    \ Check char before dash is P or D
    comet-dash-pos @ 1- c@ dup [char] P = swap [char] D = or 0= if
      drop false exit then
    \ Check all chars before P/D are digits
    comet-check-addr @ swap 1- all-digit?
  else
    \ No dash - simple format: digits + P/D
    comet-check-addr @ comet-check-len @ + 1- c@
    dup [char] P = swap [char] D = or 0= if false exit then
    \ Check all chars before type letter are digits
    comet-check-addr @ comet-check-len @ 1- all-digit?
  then ;

variable comet-num-len
variable comet-frag-len

: unpack-comet-numbered ( addr len -- addr' len' )
  \ Format: 0001P -> 1P, 0073Pa -> 73P-A, 0073Paa -> 73P-AA
  over 4 str>num                   \ addr len number
  s>d <# #s #>                     \ addr len num-str-addr num-str-len
  dup comet-num-len !              \ save length
  out-buf swap move                \ copy number string to out-buf
  \ Stack: addr len
  over 4 + c@                      \ get type letter (P or D)
  out-buf comet-num-len @ + c!     \ append to out-buf
  comet-num-len @ 1+ comet-num-len ! \ update length
  \ Check for fragment (6 or 7 chars)
  dup 5 > if
    [char] - out-buf comet-num-len @ + c!
    comet-num-len @ 1+ comet-num-len !
    \ Convert fragment to uppercase
    over 5 + c@ [char] a - [char] A +
    out-buf comet-num-len @ + c!
    comet-num-len @ 1+ comet-num-len !
    dup 7 = if
      over 6 + c@ [char] a - [char] A +
      out-buf comet-num-len @ + c!
      comet-num-len @ 1+ comet-num-len !
    then
  then
  2drop
  out-buf comet-num-len @ ;

variable pcn-dash-addr
variable pcn-type-char
variable pcn-number

: pack-comet-numbered ( addr len -- addr' len' )
  \ Format: 1P -> 0001P, 73P-A -> 0073Pa, 73P-AA -> 0073Paa
  \ Find dash for fragment
  0 pcn-dash-addr !
  0 comet-frag-len !
  2dup bounds ?do
    i c@ [char] - = if i pcn-dash-addr ! leave then
  loop
  pcn-dash-addr @ if
    \ Has fragment
    \ Fragment length = total len - dash offset - 1
    2dup + pcn-dash-addr @ - 1- comet-frag-len !
    \ Get type char (char before dash)
    pcn-dash-addr @ 1- c@ pcn-type-char !
    \ Get number (from start to type char)
    over pcn-dash-addr @ over - 1- str>num pcn-number !
    \ Build packed format
    pcn-number @ s>d <# # # # # #> out-buf swap move
    pcn-type-char @ out-buf 4 + c!
    \ Add fragment (convert to lowercase)
    comet-frag-len @ 1 = if
      pcn-dash-addr @ 1+ c@ [char] A - [char] a + out-buf 5 + c!
      2drop
      out-buf 6
    else
      pcn-dash-addr @ 1+ c@ [char] A - [char] a + out-buf 5 + c!
      pcn-dash-addr @ 2 + c@ [char] A - [char] a + out-buf 6 + c!
      2drop
      out-buf 7
    then
  else
    \ No fragment - simple format
    2dup + 1- c@ pcn-type-char !   \ get type char
    over swap 1- str>num pcn-number ! \ parse number
    pcn-number @ s>d <# # # # # #> out-buf swap move
    pcn-type-char @ out-buf 4 + c!
    out-buf 5
  then ;

\ ============================================================================
\ Comet provisional designations (1995 O1, 1995 O1-B)
\ ============================================================================

variable comet-prov-len

: unpack-comet-prov ( addr len -- addr' len' )
  \ Format: J95O010 -> 1995 O1 (or with fragment)
  \ Calculate year from century code + 2 digits
  over c@ century>num 100 *
  2 pick 1+ c@ [char] 0 - 10 * +
  2 pick 2 + c@ [char] 0 - +
  \ Stack: addr len year
  s>d <# # # # # #> out-buf swap move
  \ Space and half-month
  bl out-buf 4 + c!
  over 3 + c@ out-buf 5 + c!
  \ Order number (positions 4-5)
  over 4 + decode-cycle
  s>d <# #s #>
  \ Stack: addr len order-addr order-len
  dup 6 + comet-prov-len !         \ save total length so far
  out-buf 6 + swap move
  \ Stack: addr len
  \ Check for fragment (position 6)
  over 6 + c@ dup [char] 0 <> if
    \ Has fragment - add "-X"
    [char] - out-buf comet-prov-len @ + c!
    comet-prov-len @ 1+ comet-prov-len !
    \ Fragment letter: convert lowercase to uppercase
    [char] a - [char] A + out-buf comet-prov-len @ + c!
    comet-prov-len @ 1+ comet-prov-len !
    \ Check for 2-letter fragment (8-char input)
    dup 8 = if
      over 7 + c@ [char] a - [char] A +
      out-buf comet-prov-len @ + c!
      comet-prov-len @ 1+ comet-prov-len !
    then
  else
    drop
  then
  2drop
  out-buf comet-prov-len @ ;

variable pcp-addr
variable pcp-len
variable pcp-order
variable pcp-dash

: pack-comet-prov ( addr len -- addr' len' )
  \ Format: 1995 O1 -> J95O010, 1995 O1-B -> J95O01b
  pcp-len ! pcp-addr !
  \ Year -> century code
  pcp-addr @ 4 str>num
  dup 100 / num>century out-buf c!
  100 mod
  dup 10 / [char] 0 + out-buf 1+ c!
  10 mod [char] 0 + out-buf 2 + c!
  \ Half-month letter
  pcp-addr @ 5 + c@ out-buf 3 + c!
  \ Find dash for fragment (if any)
  0 pcp-dash !
  pcp-addr @ pcp-len @ + pcp-addr @ 6 + ?do
    i c@ [char] - = if i pcp-dash ! leave then
  loop
  \ Parse order number
  pcp-dash @ 0= if
    \ No fragment: order is from position 6 to end
    pcp-addr @ 6 + pcp-len @ 6 - str>num pcp-order !
    pcp-order @ out-buf 4 + encode-cycle
    [char] 0 out-buf 6 + c!
    out-buf 7
  else
    \ Has fragment: order is from position 6 to dash
    pcp-addr @ 6 + pcp-dash @ pcp-addr @ 6 + - str>num pcp-order !
    pcp-order @ out-buf 4 + encode-cycle
    \ Fragment letter(s) after dash
    pcp-addr @ pcp-len @ + pcp-dash @ - 1- \ fragment length
    dup 1 = if
      drop pcp-dash @ 1+ c@ [char] A - [char] a + out-buf 6 + c!
      out-buf 7
    else
      pcp-dash @ 1+ c@ [char] A - [char] a + out-buf 6 + c!
      pcp-dash @ 2 + c@ [char] A - [char] a + out-buf 7 + c!
      out-buf 8
    then
  then ;

\ ============================================================================
\ BCE and Ancient comet provisional designations
\ ============================================================================

variable bce-year
variable bce-order
variable bce-frag-len
variable ancient-year

variable bce-order-len

: unpack-bce-comet-prov ( addr len -- addr' len' )
  \ Format: .80K010 -> -119 K1 (BCE comet)
  \ Position 0: BCE prefix (/ . -)
  \ Position 1-2: 2-digit code
  \ Position 3: half-month
  \ Position 4-5: order (cycle encoded)
  \ Position 6: fragment or 0
  over c@ swap                       \ prefix on stack
  over 1+ 2 str>num                  \ code
  decode-bce-year bce-year !         \ decode to negative year
  \ Format year
  bce-year @ negate
  s>d <# #s [char] - hold #>         \ format as "-119"
  dup comet-prov-len !
  out-buf swap move
  \ Space and half-month
  bl out-buf comet-prov-len @ + c!
  comet-prov-len @ 1+ comet-prov-len !
  over 3 + c@ out-buf comet-prov-len @ + c!
  comet-prov-len @ 1+ comet-prov-len !
  \ Order number
  over 4 + decode-cycle
  s>d <# #s #>
  \ Stack: addr len order-addr order-len
  dup bce-order-len !
  out-buf comet-prov-len @ + swap move
  bce-order-len @ comet-prov-len +!
  \ Fragment (position 6)
  over 6 + c@ dup [char] 0 <> if
    [char] - out-buf comet-prov-len @ + c!
    comet-prov-len @ 1+ comet-prov-len !
    [char] a - [char] A + out-buf comet-prov-len @ + c!
    comet-prov-len @ 1+ comet-prov-len !
  else
    drop
  then
  2drop
  out-buf comet-prov-len @ ;

variable ancient-order-len

: unpack-ancient-comet-prov ( addr len -- addr' len' )
  \ Format: 422F010 -> 422 F1 (ancient year 1-999 AD)
  \ Position 0-2: 3-digit year
  \ Position 3: half-month
  \ Position 4-5: order (cycle encoded)
  \ Position 6: fragment or 0
  over 3 str>num ancient-year !
  \ Format year
  ancient-year @ s>d <# #s #>
  dup comet-prov-len !
  out-buf swap move
  \ Space and half-month
  bl out-buf comet-prov-len @ + c!
  comet-prov-len @ 1+ comet-prov-len !
  over 3 + c@ out-buf comet-prov-len @ + c!
  comet-prov-len @ 1+ comet-prov-len !
  \ Order number
  over 4 + decode-cycle
  s>d <# #s #>
  \ Stack: addr len order-addr order-len
  dup ancient-order-len !
  out-buf comet-prov-len @ + swap move
  ancient-order-len @ comet-prov-len +!
  \ Fragment (position 6)
  over 6 + c@ dup [char] 0 <> if
    [char] - out-buf comet-prov-len @ + c!
    comet-prov-len @ 1+ comet-prov-len !
    [char] a - [char] A + out-buf comet-prov-len @ + c!
    comet-prov-len @ 1+ comet-prov-len !
  else
    drop
  then
  2drop
  out-buf comet-prov-len @ ;

variable pcp-space-pos
variable pcp-year-len

: find-space ( addr len -- pos | 0 )
  \ Find position of first space, or 0 if not found
  0 -rot bounds ?do
    i c@ bl = if drop i leave then
  loop ;

: pack-bce-comet-prov ( addr len -- addr' len' )
  \ Format: -119 K1 -> .80K010
  pcp-len ! pcp-addr !
  \ Find space to get year length
  pcp-addr @ pcp-len @ find-space
  dup 0= if drop out-buf 0 exit then
  pcp-addr @ - pcp-year-len !
  \ Parse negative year (skip the minus sign)
  pcp-addr @ 1+ pcp-year-len @ 1- str>num negate bce-year !
  \ Encode BCE year
  bce-year @ encode-bce-year         \ -> prefix code
  swap out-buf c!                    \ store prefix
  s>d <# # # #> out-buf 1+ swap move \ store 2-digit code
  \ Half-month letter (after space)
  pcp-addr @ pcp-year-len @ + 1+ c@ out-buf 3 + c!
  \ Find dash for fragment
  0 pcp-dash !
  pcp-addr @ pcp-len @ + pcp-addr @ pcp-year-len @ + 2 + ?do
    i c@ [char] - = if i pcp-dash ! leave then
  loop
  \ Order number and fragment
  pcp-dash @ 0= if
    \ No fragment
    pcp-addr @ pcp-year-len @ + 2 + pcp-len @ pcp-year-len @ - 2 - str>num pcp-order !
    pcp-order @ out-buf 4 + encode-cycle
    [char] 0 out-buf 6 + c!
    out-buf 7
  else
    \ Has fragment
    pcp-addr @ pcp-year-len @ + 2 + pcp-dash @ pcp-addr @ pcp-year-len @ + 2 + - str>num pcp-order !
    pcp-order @ out-buf 4 + encode-cycle
    pcp-addr @ pcp-len @ + pcp-dash @ - 1- bce-frag-len !
    bce-frag-len @ 1 = if
      pcp-dash @ 1+ c@ [char] A - [char] a + out-buf 6 + c!
      out-buf 7
    else
      pcp-dash @ 1+ c@ [char] A - [char] a + out-buf 6 + c!
      pcp-dash @ 2 + c@ [char] A - [char] a + out-buf 7 + c!
      out-buf 8
    then
  then ;

: pack-ancient-comet-prov ( addr len -- addr' len' )
  \ Format: 422 F1 -> 422F010
  pcp-len ! pcp-addr !
  \ Find space to get year length
  pcp-addr @ pcp-len @ find-space
  dup 0= if drop out-buf 0 exit then
  pcp-addr @ - pcp-year-len !
  \ Parse year
  pcp-addr @ pcp-year-len @ str>num ancient-year !
  \ Format 3-digit year
  ancient-year @ s>d <# # # # #> out-buf swap move
  \ Half-month letter
  pcp-addr @ pcp-year-len @ + 1+ c@ out-buf 3 + c!
  \ Find dash for fragment
  0 pcp-dash !
  pcp-addr @ pcp-len @ + pcp-addr @ pcp-year-len @ + 2 + ?do
    i c@ [char] - = if i pcp-dash ! leave then
  loop
  \ Order number and fragment
  pcp-dash @ 0= if
    \ No fragment
    pcp-addr @ pcp-year-len @ + 2 + pcp-len @ pcp-year-len @ - 2 - str>num pcp-order !
    pcp-order @ out-buf 4 + encode-cycle
    [char] 0 out-buf 6 + c!
    out-buf 7
  else
    \ Has fragment
    pcp-addr @ pcp-year-len @ + 2 + pcp-dash @ pcp-addr @ pcp-year-len @ + 2 + - str>num pcp-order !
    pcp-order @ out-buf 4 + encode-cycle
    pcp-addr @ pcp-len @ + pcp-dash @ - 1- bce-frag-len !
    bce-frag-len @ 1 = if
      pcp-dash @ 1+ c@ [char] A - [char] a + out-buf 6 + c!
      out-buf 7
    else
      pcp-dash @ 1+ c@ [char] A - [char] a + out-buf 6 + c!
      pcp-dash @ 2 + c@ [char] A - [char] a + out-buf 7 + c!
      out-buf 8
    then
  then ;

\ ============================================================================
\ Full comet designations (C/1995 O1, 1P/1995 O1)
\ ============================================================================

: is-comet-type? ( c -- f )
  case
    [char] P of true endof
    [char] C of true endof
    [char] D of true endof
    [char] X of true endof
    [char] A of true endof
    [char] I of true endof
    false swap
  endcase ;

: is-comet-bce-packed? ( addr len -- f )
  \ 8-char format: type + BCE prefix (/ . -) + 2-digit code + provisional
  dup 8 <> if 2drop false exit then
  over c@ is-comet-type? 0= if 2drop false exit then
  over 1+ c@ is-bce-prefix? 0= if 2drop false exit then
  \ Positions 2-3 must be digits (year code)
  over 2 + c@ is-digit 0= if 2drop false exit then
  over 3 + c@ is-digit 0= if 2drop false exit then
  \ Position 4 must be uppercase letter (half-month)
  over 4 + c@ is-upper
  if 2drop true else 2drop false then ;

: is-comet-ancient-packed? ( addr len -- f )
  \ 8-char format: type + 3-digit year + provisional (year < 1000)
  dup 8 <> if 2drop false exit then
  over c@ is-comet-type? 0= if 2drop false exit then
  \ Check positions 1-3 are all digits
  over 1+ c@ is-digit 0= if 2drop false exit then
  over 2 + c@ is-digit 0= if 2drop false exit then
  over 3 + c@ is-digit 0= if 2drop false exit then
  \ Check position 4 is letter (half-month), not digit (which would be standard)
  over 4 + c@ is-upper
  if 2drop true else 2drop false then ;

: is-comet-full-packed? ( addr len -- f )
  dup 8 < if 2drop false exit then
  \ Check for BCE format first (type + BCE prefix)
  2dup is-comet-bce-packed? if 2drop true exit then
  \ Check for ancient format (type + 3-digit year)
  2dup is-comet-ancient-packed? if 2drop true exit then
  \ Exclude unpacked format (has slash at position 1 that's not BCE)
  over 1+ c@ [char] / = if 2drop false exit then
  dup 8 = if
    \ 8-char: type + 7-char provisional
    drop c@ is-comet-type? exit
  then
  dup 9 = if
    \ 9-char: type + 8-char provisional (2-letter fragment)
    drop c@ is-comet-type? exit
  then
  2drop false ;

: is-comet-full-unpacked? ( addr len -- f )
  \ Check for X/yyyy pattern or nX/yyyy pattern
  \ Find slash
  0 2 pick 2 pick bounds ?do
    i c@ [char] / = if drop i leave then
  loop
  dup 0= if drop 2drop false exit then
  \ Check char before slash is comet type
  1- c@ is-comet-type?
  -rot 2drop ;

variable comet-full-len
variable comet-full-type

\ Detect asteroid-style unpacked provisional: "1995 XA" or "2013 AL76"
\ Second character after space is uppercase letter (not digit)
: is-asteroid-style-unpacked? ( addr len -- f )
  \ Find space - returns address of space or 0
  2dup find-space
  dup 0= if drop 2drop false exit then
  \ Stack: addr len space-addr
  \ Position after space is half-month letter, next should be second letter or digit
  \ Space-addr + 2 is second char after space (half-month + second-letter position)
  2 +  \ addr of char after half-month
  \ Check if this address is within bounds
  2 pick 2 pick + over > if
    c@ is-upper  \ char is uppercase = asteroid-style
  else
    drop false   \ out of bounds
  then
  -rot 2drop ;   \ cleanup original addr len

\ Detect asteroid-style packed provisional: last char is uppercase (A-Z)
\ Comet-style ends with digit (order) or lowercase (fragment)
: is-asteroid-style-packed? ( addr len -- f )
  + 1- c@ is-upper ;

: unpack-comet-full ( addr len -- addr' len' )
  \ Format: CJ95O010 -> C/1995 O1, or 0001PJ95O010 -> 1P/1995 O1
  \ Also handles BCE: C.80K010 -> C/-119 K1
  \ Also handles ancient: C422F010 -> C/422 F1
  \ Also handles asteroid-style: PK13A76L -> P/2013 AL76
  dup 8 = over 9 = or if
    \ Short format: type + provisional (or BCE/ancient/asteroid-style)
    over c@ comet-full-type !
    \ Check for BCE format (position 1 is BCE prefix)
    2dup is-comet-bce-packed? if
      over 1+ over 1- unpack-bce-comet-prov
      dup comet-full-len !
      out-buf tmp-buf comet-full-len @ move
      comet-full-type @ out-buf c!
      [char] / out-buf 1+ c!
      tmp-buf out-buf 2 + comet-full-len @ move
      2drop 2drop
      out-buf comet-full-len @ 2 +
      exit
    then
    \ Check for ancient format (position 1-3 are digits for year < 1000)
    2dup is-comet-ancient-packed? if
      over 1+ over 1- unpack-ancient-comet-prov
      dup comet-full-len !
      out-buf tmp-buf comet-full-len @ move
      comet-full-type @ out-buf c!
      [char] / out-buf 1+ c!
      tmp-buf out-buf 2 + comet-full-len @ move
      2drop 2drop
      out-buf comet-full-len @ 2 +
      exit
    then
    \ Check for asteroid-style (7-char provisional ends with uppercase)
    over 1+ over 1- is-asteroid-style-packed? if
      over 1+ over 1- unpack-prov
      dup comet-full-len !
      out-buf tmp-buf comet-full-len @ move
      comet-full-type @ out-buf c!
      [char] / out-buf 1+ c!
      tmp-buf out-buf 2 + comet-full-len @ move
      2drop 2drop
      out-buf comet-full-len @ 2 +
      exit
    then
    \ Standard comet format: type + 7-char provisional
    over 1+ over 1- unpack-comet-prov
    dup comet-full-len !
    out-buf tmp-buf comet-full-len @ move
    comet-full-type @ out-buf c!
    [char] / out-buf 1+ c!
    tmp-buf out-buf 2 + comet-full-len @ move
    2drop 2drop
    out-buf comet-full-len @ 2 +
  else
    \ Long format: number + type + provisional (not common)
    out-buf 0
  then ;

variable comet-type-char

variable pcf-prov-addr
variable pcf-prov-len
variable pcf-space-pos

: pack-comet-full-prov ( addr len -- addr' len' )
  \ Dispatch to appropriate packer based on format
  \ Input: provisional part (e.g., "-119 K1" or "422 F1" or "1995 O1" or "2013 AL76")
  pcf-prov-len ! pcf-prov-addr !
  \ Check for BCE (starts with minus)
  pcf-prov-addr @ c@ [char] - = if
    pcf-prov-addr @ pcf-prov-len @ pack-bce-comet-prov exit
  then
  \ Find space to determine year length
  pcf-prov-addr @ pcf-prov-len @ find-space
  dup 0= if drop out-buf 0 exit then
  pcf-prov-addr @ - pcf-space-pos !
  \ If year is 1-3 digits, it's ancient
  pcf-space-pos @ 4 < if
    pcf-prov-addr @ pcf-prov-len @ pack-ancient-comet-prov exit
  then
  \ Check for asteroid-style (two letters after half-month)
  pcf-prov-addr @ pcf-prov-len @ is-asteroid-style-unpacked? if
    pcf-prov-addr @ pcf-prov-len @ pack-prov exit
  then
  \ Standard comet format
  pcf-prov-addr @ pcf-prov-len @ pack-comet-prov ;

: pack-comet-full ( addr len -- addr' len' )
  \ Find the slash
  2dup
  0 -rot bounds ?do
    i c@ [char] / = if drop i leave then
  loop
  \ Stack: addr len slash-addr
  dup 0= if drop out-buf 0 2swap 2drop exit then
  2 pick -                      \ offset to slash = slash-addr - addr
  dup 1 = if
    \ Simple format: X/yyyy prov -> XJ95O010
    drop
    over c@ comet-type-char !   \ save type character
    over 2 + over 2 - pack-comet-full-prov
    \ Stack: addr len out-buf prov-len
    \ Copy result to tmp-buf
    over tmp-buf 2 pick move    \ move(out-buf, tmp-buf, prov-len)
    \ Put type at position 0
    comet-type-char @ out-buf c!
    \ Copy from tmp-buf back to out-buf+1
    tmp-buf out-buf 1+ 2 pick move
    \ Stack: addr len out-buf prov-len
    \ Return out-buf with length = prov-len + 1
    1+ >r                       \ save prov-len+1 to return stack
    2drop drop                  \ drop out-buf, len, addr
    out-buf r>
  else
    \ Numbered format: nnnX/yyyy prov
    2 pick over 1- str>num      \ comet number
    s>d <# # # # # #> tmp-buf swap move
    2 pick over + 1- c@ comet-type-char ! \ save type
    2 pick over + 1+ 2 pick over - 1- pack-comet-full-prov
    \ Now build result: 4-digit number + type + prov result
    tmp-buf out-buf 4 move      \ copy number
    comet-type-char @ out-buf 4 + c! \ type at position 4
    out-buf 5 + over move       \ copy prov result
    out-buf swap 5 +            \ return with length
    2swap 2drop
  then ;

\ ============================================================================
\ Natural satellite designations (S/2019 S 22)
\ ============================================================================

: is-satellite-packed? ( addr len -- f )
  dup 8 <> if 2drop false exit then
  drop
  dup c@ [char] S <> if drop false exit then
  4 + c@ case
    [char] J of true endof
    [char] S of true endof
    [char] U of true endof
    [char] N of true endof
    false swap
  endcase ;

: is-satellite-unpacked? ( addr len -- f )
  \ Minimum: "S/yyyy X n" = 10 chars
  dup 10 < if 2drop false exit then
  over c@ [char] S <> if 2drop false exit then
  over 1+ c@ [char] / <> if 2drop false exit then
  2drop true ;

variable sat-num-len

: unpack-satellite ( addr len -- addr' len' )
  \ Format: SK19S220 -> S/2019 S 22
  [char] S out-buf c!
  [char] / out-buf 1+ c!
  \ Year from century code + 2 digits
  over 1+ c@ century>num 100 *
  2 pick 2 + c@ [char] 0 - 10 * +
  2 pick 3 + c@ [char] 0 - +
  s>d <# # # # # #> out-buf 2 + swap move
  bl out-buf 6 + c!
  over 4 + c@ out-buf 7 + c!    \ planet
  bl out-buf 8 + c!
  \ Satellite number (positions 5-6)
  over 5 + decode-cycle
  s>d <# #s #>
  dup sat-num-len !
  out-buf 9 + swap move
  2drop
  out-buf sat-num-len @ 9 + ;

: pack-satellite ( addr len -- addr' len' )
  \ Format: S/2019 S 22 -> SK19S220
  [char] S out-buf c!
  over 2 + 4 str>num            \ year
  dup 100 / num>century out-buf 1+ c!
  100 mod
  dup 10 / [char] 0 + out-buf 2 + c!
  10 mod [char] 0 + out-buf 3 + c!
  over 7 + c@ out-buf 4 + c!    \ planet
  over 9 + over 9 - str>num     \ satellite number
  out-buf 5 + encode-cycle
  [char] 0 out-buf 7 + c!
  out-buf 8
  2swap 2drop ;

\ ============================================================================
\ Format detection
\ ============================================================================

: packed-perm? ( addr len -- f )
  dup 5 <> if 2drop false exit then
  over c@ [char] ~ = if
    \ Tilde format
    2drop true exit
  then
  over c@ is-digit if
    all-digit?
  else over c@ is-alpha if
    drop 1+ 4 all-digit?
  else
    2drop false
  then then ;

: packed-prov? ( addr len -- f )
  dup 7 <> if 2drop false exit then
  over c@ [char] _ = if 2drop true exit then  \ extended format
  over c@ century>num 0< if 2drop false exit then
  \ Exclude old-style unpacked format (has space at position 4)
  2dup has-space? if 2drop false exit then
  2drop true ;

: unpacked-perm? ( addr len -- f )
  all-digit? ;

: unpacked-prov? ( addr len -- f )
  has-space? ;

\ ============================================================================
\ Main conversion function
\ ============================================================================

: convert-simple ( addr len -- addr' len' )
  \ Check packed formats first
  2dup packed-perm? if unpack-perm exit then
  2dup is-extended-packed? if unpack-extended exit then
  2dup is-survey-packed? if unpack-survey exit then
  2dup is-comet-numbered-packed? if unpack-comet-numbered exit then
  2dup is-satellite-packed? if unpack-satellite exit then
  2dup is-comet-full-packed? if unpack-comet-full exit then
  2dup packed-prov? if unpack-prov exit then

  \ Check unpacked formats
  2dup is-comet-numbered-unpacked? if pack-comet-numbered exit then
  2dup is-satellite-unpacked? if pack-satellite exit then
  2dup is-comet-full-unpacked? if pack-comet-full exit then
  2dup unpacked-perm? if pack-perm exit then
  2dup unpacked-prov? if pack-prov exit then

  \ Unknown format - return as-is
  out-buf over move out-buf swap ;

\ ============================================================================
\ Helper functions for format conversion and fragment handling
\ ============================================================================

create report-buf 12 allot
variable report-len

variable trf-addr
variable trf-len

: to-report-format ( addr len -- addr' len' )
  \ Convert minimal packed format to 12-character MPC report format
  \ Numbered comets go at start, everything else right-aligned
  trf-len ! trf-addr !
  report-buf 12 bl fill
  trf-len @ 5 >= trf-len @ 7 <= and if
    trf-addr @ 4 + c@ dup [char] P = swap [char] D = or if
      trf-addr @ 4 all-digit? if
        \ Numbered comet: 0001P -> "0001P       "
        \ Or with fragment: 0073Pa -> "0073P      a"
        trf-addr @ report-buf 5 move        \ copy first 5 chars (number+type)
        trf-len @ 6 = if
          trf-addr @ 5 + c@ report-buf 11 + c!    \ single letter at pos 11
        then
        trf-len @ 7 = if
          trf-addr @ 5 + c@ report-buf 10 + c!    \ first letter at pos 10
          trf-addr @ 6 + c@ report-buf 11 + c!    \ second letter at pos 11
        then
        report-buf 12 exit
      then
    then
  then
  \ Everything else: right-align to 12 chars
  trf-addr @ 12 trf-len @ - report-buf + trf-len @ move
  report-buf 12 ;

variable frf-addr
variable frf-len
variable frf-start

: from-report-format ( addr len -- addr' len' )
  \ Convert 12-character report format to minimal packed format
  \ Format for numbered comets: "0073P       " or "0073P      a" or "0073P     aa"
  \ Fragment letters are in positions 10-11 (0-indexed)
  frf-len ! frf-addr !
  frf-len @ 12 <> if frf-addr @ frf-len @ exit then  \ return as-is if not 12 chars
  \ Check for numbered comet format
  frf-addr @ 4 + c@ dup [char] P = swap [char] D = or if
    frf-addr @ 4 all-digit? if
      \ Numbered comet - check for fragment in columns 10-11 (positions 10, 11)
      frf-addr @ report-buf 5 move           \ copy first 5 chars
      5 report-len !
      \ Check position 11 first (single letter fragment goes there)
      frf-addr @ 11 + c@ bl <> if
        \ Check if position 10 is also non-space (two-letter fragment)
        frf-addr @ 10 + c@ bl <> if
          \ Two-letter fragment at positions 10-11
          frf-addr @ 10 + c@ report-buf 5 + c!
          frf-addr @ 11 + c@ report-buf 6 + c!
          7 report-len !
        else
          \ Single-letter fragment at position 11
          frf-addr @ 11 + c@ report-buf 5 + c!
          6 report-len !
        then
      then
      report-buf report-len @ exit
    then
  then
  \ Other formats: strip leading spaces
  frf-addr @ frf-start !
  frf-addr @ frf-len @ bounds ?do
    i c@ bl <> if i frf-start ! leave then
  loop
  frf-start @ frf-addr @ <> if
    \ Found non-space: copy from there to end
    frf-addr @ frf-len @ + frf-start @ -    \ length of remaining string
    frf-start @ report-buf 2 pick move
    report-buf swap
  else
    \ All spaces or starts with non-space
    frf-addr @ report-buf frf-len @ move
    report-buf frf-len @
  then ;

variable has-frag-result

: has-fragment ( addr len -- f )
  \ Check if designation has a comet fragment suffix
  false has-frag-result !
  \ Unpacked numbered comet with fragment: nP-X or nP-XX
  2dup
  0 2 pick 2 pick bounds ?do
    i c@ [char] - = if drop i leave then
  loop
  dup if
    dup 1- c@ dup [char] P = swap [char] D = or if
      true has-frag-result !
    then
  then
  drop 2drop
  \ Packed numbered comet with fragment: 0073Pa (6) or 0073Paa (7)
  has-frag-result @ 0= if
    2dup
    dup 6 >= over 7 <= and if
      over 4 + c@ dup [char] P = swap [char] D = or if
        over 4 all-digit? if
          over 5 + c@ is-lower if
            true has-frag-result !
          then
        then
      then
    then
    2drop
  then
  \ Packed provisional comet with fragment: DJ93F02a (8 with lowercase last)
  has-frag-result @ 0= if
    2dup
    dup 8 = if
      over c@ is-comet-type? if
        over 7 + c@ dup is-lower swap [char] 0 <> and if
          true has-frag-result !
        then
      then
    then
    2drop
  then
  \ Packed provisional comet with 2-letter fragment: PJ30J01aa (9)
  has-frag-result @ 0= if
    2dup
    dup 9 = if
      over c@ is-comet-type? if
        over 7 + c@ is-lower if
          over 8 + c@ is-lower if
            true has-frag-result !
          then
        then
      then
    then
    2drop
  then
  \ Unpacked provisional comet with fragment: C/1995 O1-A
  has-frag-result @ 0= if
    2dup
    dup 11 >= if
      over 1+ c@ [char] / = if
        2dup + 2 - c@ [char] - = if
          true has-frag-result !
        else 2dup + 3 - c@ [char] - = if
          true has-frag-result !
        then then
      then
    then
    2drop
  then
  2drop
  has-frag-result @ ;

create frag-buf 2 allot
variable frag-len

: get-fragment ( addr len -- addr' len' )
  \ Extract fragment suffix (uppercase)
  0 frag-len !
  \ Unpacked numbered comet: nP-X or nP-XX
  2dup
  0 2 pick 2 pick bounds ?do
    i c@ [char] - = if drop i leave then
  loop
  dup if
    dup 1- c@ dup [char] P = swap [char] D = or if
      1+                                    \ addr after dash
      2 pick 2 pick + over -                \ fragment length
      dup frag-len !
      frag-buf swap move
      2drop 2drop
      frag-buf frag-len @ exit
    then
  then
  drop 2drop
  \ Packed numbered comet: 0073Pa or 0073Paa
  2dup
  dup 6 >= over 7 <= and if
    over 4 + c@ dup [char] P = swap [char] D = or if
      over 4 all-digit? if
        over 5 + c@ is-lower if
          over 5 + c@ [char] a - [char] A + frag-buf c!
          1 frag-len !
          dup 7 = if
            over 6 + c@ [char] a - [char] A + frag-buf 1+ c!
            2 frag-len !
          then
          2drop 2drop
          frag-buf frag-len @ exit
        then
      then
    then
  then
  2drop
  \ Packed provisional comet: DJ93F02a or PJ30J01aa
  2dup
  dup 8 = if
    over c@ is-comet-type? if
      over 7 + c@ dup is-lower swap [char] 0 <> and if
        over 7 + c@ [char] a - [char] A + frag-buf c!
        1 frag-len !
        2drop 2drop
        frag-buf frag-len @ exit
      then
    then
  then
  dup 9 = if
    over c@ is-comet-type? if
      over 7 + c@ is-lower if
        over 7 + c@ [char] a - [char] A + frag-buf c!
        over 8 + c@ [char] a - [char] A + frag-buf 1+ c!
        2 frag-len !
        2drop 2drop
        frag-buf frag-len @ exit
      then
    then
  then
  2drop
  \ Unpacked provisional comet: C/1995 O1-A or C/1995 O1-AA
  2dup
  dup 11 >= if
    over 1+ c@ [char] / = if
      2dup + 2 - c@ [char] - = if
        2dup + 1- c@ frag-buf c!
        1 frag-len !
        2drop 2drop
        frag-buf frag-len @ exit
      then
      2dup + 3 - c@ [char] - = if
        2dup + 2 - c@ frag-buf c!
        2dup + 1- c@ frag-buf 1+ c!
        2 frag-len !
        2drop 2drop
        frag-buf frag-len @ exit
      then
    then
  then
  2drop
  2drop
  frag-buf 0 ;

create parent-buf MAX-LEN allot
variable parent-len
variable gp-addr
variable gp-len
variable gp-dash

: get-parent ( addr len -- addr' len' )
  \ Get parent comet without fragment suffix
  gp-len ! gp-addr !

  \ Unpacked numbered comet: 73P-A -> 73P
  0 gp-dash !
  gp-addr @ gp-len @ bounds ?do
    i c@ [char] - = if i gp-dash ! leave then
  loop
  gp-dash @ if
    gp-dash @ 1- c@ dup [char] P = swap [char] D = or if
      gp-dash @ gp-addr @ - parent-len !
      gp-addr @ parent-buf parent-len @ move
      parent-buf parent-len @ exit
    then
  then

  \ Packed numbered comet: 0073Pa -> 0073P
  gp-len @ 6 >= gp-len @ 7 <= and if
    gp-addr @ 4 + c@ dup [char] P = swap [char] D = or if
      gp-addr @ 4 all-digit? if
        gp-addr @ 5 + c@ is-lower if
          gp-addr @ parent-buf 5 move
          parent-buf 5 exit
        then
      then
    then
  then

  \ Packed provisional comet with fragment: DJ93F02a -> DJ93F020
  gp-len @ 8 = if
    gp-addr @ c@ is-comet-type? if
      gp-addr @ 7 + c@ dup is-lower swap [char] 0 <> and if
        gp-addr @ parent-buf 7 move
        [char] 0 parent-buf 7 + c!
        parent-buf 8 exit
      then
    then
  then
  gp-len @ 9 = if
    gp-addr @ c@ is-comet-type? if
      gp-addr @ 7 + c@ is-lower if
        gp-addr @ parent-buf 7 move
        [char] 0 parent-buf 7 + c!
        parent-buf 8 exit
      then
    then
  then

  \ Unpacked provisional comet: D/1993 F2-B -> D/1993 F2
  gp-len @ 11 >= if
    gp-addr @ 1+ c@ [char] / = if
      \ Find dash from position 6 onwards
      0 gp-dash !
      gp-addr @ gp-len @ + gp-addr @ 6 + ?do
        i c@ [char] - = if i gp-dash ! then
      loop
      gp-dash @ if
        gp-dash @ gp-addr @ - parent-len !
        gp-addr @ parent-buf parent-len @ move
        parent-buf parent-len @ exit
      then
    then
  then

  \ No fragment - return as-is
  gp-addr @ parent-buf gp-len @ move
  parent-buf gp-len @ ;

create pack1-buf MAX-LEN allot
create pack2-buf MAX-LEN allot
create input1-buf MAX-LEN allot
create input2-buf MAX-LEN allot
variable pack1-len
variable pack2-len
variable de-len1
variable de-len2
variable pd-buf
variable pd-len

: is-packed? ( addr len -- f )
  \ Check if designation is in packed format
  2dup packed-perm? if 2drop true exit then
  2dup is-extended-packed? if 2drop true exit then
  2dup is-survey-packed? if 2drop true exit then
  2dup is-comet-numbered-packed? if 2drop true exit then
  2dup is-satellite-packed? if 2drop true exit then
  2dup is-comet-full-packed? if 2drop true exit then
  2dup packed-prov? if 2drop true exit then
  2drop false ;

variable pd-addr
variable pd-result-addr
variable pd-result-len

: pack-to-buf ( addr len buf -- len' )
  \ Pack a designation, store in buf, return length
  pd-buf !
  pd-len !
  pd-addr !
  \ Check if already packed
  pd-addr @ pd-len @ is-packed? if
    \ Already packed - just copy: move(src, dest, count)
    pd-addr @ pd-buf @ pd-len @ move
    pd-len @
  else
    \ Not packed - convert then copy result
    pd-addr @ pd-len @ convert-simple     \ -> result-addr result-len
    pd-result-len !
    pd-result-addr !
    \ move(src, dest, count)
    pd-result-addr @ pd-buf @ pd-result-len @ move
    pd-result-len @
  then ;

: designations-equal ( addr1 len1 addr2 len2 -- f )
  \ Check if two designations refer to the same object
  \ Pack both to normalized format and compare
  \ First copy inputs to local buffers to preserve them
  de-len2 !
  input2-buf de-len2 @ move               \ copy addr2 to input2-buf
  de-len1 !
  input1-buf de-len1 @ move               \ copy addr1 to input1-buf
  \ Pack first designation
  input1-buf de-len1 @ pack1-buf pack-to-buf pack1-len !
  \ Pack second designation
  input2-buf de-len2 @ pack2-buf pack-to-buf pack2-len !
  \ Compare
  pack1-buf pack1-len @ pack2-buf pack2-len @ s= ;

\ ============================================================================
\ CLI support
\ ============================================================================

: process-arg ( addr len -- )
  convert-simple type cr ;

: print-usage
  ." Usage: gforth src/mpc_designation_cli.fs <designation>..." cr
  ." Converts between packed and unpacked MPC designations." cr
  ." " cr
  ." Supports:" cr
  ."   - Numbered asteroids (1, 100001, 620000)" cr
  ."   - Provisional asteroids (1995 XA, 2024 AB123)" cr
  ."   - Survey designations (2040 P-L, 3138 T-1)" cr
  ."   - Comets (1P, C/1995 O1, D/1993 F2-B)" cr
  ."   - Natural satellites (S/2019 S 22)" cr ;
