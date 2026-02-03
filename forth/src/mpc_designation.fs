\ mpc_designation.fs - MPC Designation Converter for Gforth (Full Version)
\
\ Supports: numbered asteroids, provisional, survey, comets, satellites
\ Uses gforth's built-in facilities

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
  \ Format: CyyHooL -> yyyy HL[n]
  \ Calculate year
  over c@ century>num 100 *
  2 pick 1+ c@ [char] 0 - 10 * +
  2 pick 2 + c@ [char] 0 - +
  prov-year !
  \ Get cycle count
  over 4 + decode-cycle prov-cycle !
  \ Build output string in out-buf
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
  \ Check for A908 CJ or B842 FA format
  8 <> if drop false exit then
  dup c@ dup [char] A = swap [char] B = or 0= if drop false exit then
  dup 4 + c@ bl <> if drop false exit then
  drop true ;

: pack-prov-old ( addr len -- addr' len' )
  \ Old-style: A908 CJ -> I08C00J
  over c@ case
    [char] A of [char] I endof  \ 1800s
    [char] B of [char] J endof  \ 1900s (B0xx = 190x)
    0 swap
  endcase
  out-buf c!
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
  dup 5 <> if 2drop false exit then
  drop
  dup 4 + c@ dup [char] P = swap [char] D = or 0= if drop false exit then
  4 all-digit? ;

: is-comet-numbered-unpacked? ( addr len -- f )
  dup 2 < if 2drop false exit then
  2dup + 1- c@ dup [char] P = swap [char] D = or 0= if 2drop false exit then
  1- all-digit? ;

variable comet-num-len

: unpack-comet-numbered ( addr len -- addr' len' )
  \ Format: 0001P -> 1P
  over 4 str>num                   \ addr len number
  s>d <# #s #>                     \ addr len num-str-addr num-str-len
  dup comet-num-len !              \ save length
  out-buf swap move                \ copy number string to out-buf
  \ Stack: addr len
  over 4 + c@                      \ get type letter (P or D)
  out-buf comet-num-len @ + c!     \ append to out-buf
  2drop
  out-buf comet-num-len @ 1+ ;

: pack-comet-numbered ( addr len -- addr' len' )
  \ Format: 1P -> 0001P
  2dup + 1- c@                     \ addr len type-char
  -rot                             \ type-char addr len
  1- str>num                       \ type-char number
  s>d <# # # # # #> out-buf swap move
  out-buf 4 + c!                   \ store type letter
  out-buf 5 ;

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

: is-comet-full-packed? ( addr len -- f )
  dup 8 < if 2drop false exit then
  \ Exclude unpacked format (has slash at position 1)
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

: unpack-comet-full ( addr len -- addr' len' )
  \ Format: CJ95O010 -> C/1995 O1, or 0001PJ95O010 -> 1P/1995 O1
  dup 8 = over 9 = or if
    \ Short format: type + provisional
    \ Save type character before calling unpack-comet-prov
    over c@ comet-full-type !
    \ Unpack the provisional part (positions 1 to end)
    over 1+ over 1- unpack-comet-prov
    \ Stack: addr len prov-addr prov-len
    \ Result is in out-buf, save length
    dup comet-full-len !
    \ Copy to tmp-buf first, then back with prefix
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
    over 2 + over 2 - pack-comet-prov
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
    2 pick over + 1+ 2 pick over - 1- pack-comet-prov
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
