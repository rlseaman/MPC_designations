# Complete MPC Designation Reference

Comprehensive annotated list of all MPC designation classes, subclasses, packing
rules, and edge cases. Based on analysis of official MPC documentation, 6 language
implementations (Python, TCL, C, Go, Java, Rust), and 2M+ test cases.

## Sources

1. **MPC Official**: https://www.minorplanetcenter.net/iau/info/PackedDes.html
2. **Repository test data**: 2,021,090 provisional designation pairs + 114 error cases
3. **Language implementations**: Python, TCL, C, Go, Java, Rust (all produce identical results)

---

## Part 1: Permanent (Numbered) Minor Planets

### 1.1 Three-Tier Encoding System

| Number Range | Packed Format | Encoding Method |
|-------------|---------------|-----------------|
| 1 - 99,999 | `00001` - `99999` | 5-digit zero-padded decimal |
| 100,000 - 619,999 | `A0000` - `z9999` | Letter prefix + 4-digit remainder |
| 620,000 - 15,396,335 | `~0000` - `~zzzz` | Tilde + 4-char base-62 |

### 1.2 Letter Prefix Encoding (100,000-619,999)

**Formula**: `letter_value = number / 10000`, `remainder = number % 10000`

| Letter | Value | Number Range | Example |
|--------|-------|--------------|---------|
| A | 10 | 100,000-109,999 | 100345 → A0345 |
| B | 11 | 110,000-119,999 | 119999 → B9999 |
| ... | ... | ... | ... |
| Z | 35 | 350,000-359,999 | 350000 → Z0000 |
| a | 36 | 360,000-369,999 | 360001 → a0001 |
| b | 37 | 370,000-379,999 | 379999 → b9999 |
| ... | ... | ... | ... |
| z | 61 | 610,000-619,999 | 619999 → z9999 |

**Important**: Uppercase A-Z = values 10-35; lowercase a-z = values 36-61.

### 1.3 Tilde Format (≥620,000)

**Formula**: `packed = "~" + base62(number - 620000)`

The 4-character base-62 string encodes numbers 0-14,776,335.
Maximum encodable asteroid: **15,396,335** (= 620,000 + 62^4 - 1).

**Base-62 character set** (order is critical):
```
Index:  0-9   10-35  36-61
Chars:  0-9   A-Z    a-z
```

| Number | Calculation | Packed |
|--------|-------------|--------|
| 620,000 | base62(0) = 0000 | ~0000 |
| 620,001 | base62(1) = 0001 | ~0001 |
| 620,062 | base62(62) = 0010 | ~0010 |
| 3,140,113 | base62(2520113) = AZaz | ~AZaz |
| 15,396,335 | base62(14776335) = zzzz | ~zzzz |

### 1.4 Edge Cases - Permanent Numbers

| Edge Case | Number | Packed | Status |
|-----------|--------|--------|--------|
| Minimum valid | 1 | 00001 | ✓ All implementations |
| Last 5-digit | 99,999 | 99999 | ✓ Boundary validated |
| First letter | 100,000 | A0000 | ✓ Boundary validated |
| Last uppercase | 359,999 | Z9999 | ✓ Boundary validated |
| First lowercase | 360,000 | a0000 | ✓ Boundary validated |
| Last letter | 619,999 | z9999 | ✓ Boundary validated |
| First tilde | 620,000 | ~0000 | ✓ Boundary validated |
| Maximum | 15,396,335 | ~zzzz | ✓ Boundary validated |
| Zero | 0 | (invalid) | ✗ Rejected by all |
| Negative | -1 | (invalid) | ✗ Rejected by all |
| Above max | 15,396,336 | (invalid) | ✗ Rejected by all |

---

## Part 2: Provisional Minor Planet Designations

### 2.1 Standard Format (7 characters, cycle 0-619)

**Structure**: `[Century][YY][Half-month][Cycle-tens][Cycle-ones][Letter]`

| Position | Field | Values | Notes |
|----------|-------|--------|-------|
| 1 | Century code | I-L | I=18xx, J=19xx, K=20xx, L=21xx |
| 2-3 | Year (2 digit) | 00-99 | Within century |
| 4 | Half-month | A-Y (not I) | 24 values |
| 5-6 | Cycle count | 00-z9 | 0-619 encoded |
| 7 | Second letter | A-Z (not I) | 25 values |

### 2.2 Century Codes

| Code | Century | Year Range | Example Year |
|------|---------|------------|--------------|
| A | 10 | 1000-1099 | (historical only) |
| B | 11 | 1100-1199 | (historical only) |
| C | 12 | 1200-1299 | (historical only) |
| D | 13 | 1300-1399 | (historical only) |
| E | 14 | 1400-1499 | (historical only) |
| F | 15 | 1500-1599 | (historical only) |
| G | 16 | 1600-1699 | (historical only) |
| H | 17 | 1700-1799 | (historical only) |
| I | 18 | 1800-1899 | 1850 |
| J | 19 | 1900-1999 | 1995 |
| K | 20 | 2000-2099 | 2024 |
| L | 21 | 2100-2199 | (future) |

**Note**: Century codes A-H are defined but rarely used (ancient designations).

### 2.3 Half-Month Codes (24 values, I is skipped)

| Code | Dates | Code | Dates |
|------|-------|------|-------|
| A | Jan 1-15 | N | Jul 1-15 |
| B | Jan 16-31 | O | Jul 16-31 |
| C | Feb 1-15 | P | Aug 1-15 |
| D | Feb 16-28/29 | Q | Aug 16-31 |
| E | Mar 1-15 | R | Sep 1-15 |
| F | Mar 16-31 | S | Sep 16-30 |
| G | Apr 1-15 | T | Oct 1-15 |
| H | Apr 16-30 | U | Oct 16-31 |
| J | May 1-15 | V | Nov 1-15 |
| K | May 16-31 | W | Nov 16-30 |
| L | Jun 1-15 | X | Dec 1-15 |
| M | Jun 16-30 | Y | Dec 16-31 |

**Critical**: Letter "I" is never used (confusion with numeral 1).

### 2.4 Cycle Count Encoding (0-619)

Each half-month can have up to 15,500 discoveries (25 letters × 620 cycles).

**Tens digit encoding**:
| Value | Character | Value | Character |
|-------|-----------|-------|-----------|
| 0 | 0 | 31 | V |
| 1 | 1 | 32 | W |
| ... | ... | ... | ... |
| 9 | 9 | 35 | Z |
| 10 | A | 36 | a |
| 11 | B | 37 | b |
| ... | ... | ... | ... |
| 26 | Q | 61 | z |

**Ones digit**: Always 0-9.

**Examples**:
| Cycle | Tens | Ones | Packed |
|-------|------|------|--------|
| 0 | 0 | 0 | 00 |
| 9 | 0 | 9 | 09 |
| 10 | 1 | 0 | 10 |
| 99 | 9 | 9 | 99 |
| 100 | A (10) | 0 | A0 |
| 359 | Z (35) | 9 | Z9 |
| 360 | a (36) | 0 | a0 |
| 619 | z (61) | 9 | z9 |

### 2.5 Extended Provisional Format (cycle ≥ 620)

For discoveries beyond cycle 619, an extended 7-character format is used.

**Structure**: `_[Year62][Half-month][Sequence-4char]`

| Position | Field | Values | Notes |
|----------|-------|--------|-------|
| 1 | Marker | _ | Underscore indicates extended |
| 2 | Year code | 0-9,A-z | base62(year % 100) |
| 3 | Half-month | A-Y (not I) | Same as standard |
| 4-7 | Sequence | 0000-zzzz | 4-char base-62 |

**Sequence calculation**:
```
sequence = (cycle - 620) × 25 + letter_position
```
Where letter_position: A=0, B=1, ..., Z=24 (skipping I).

**Examples from test data**:
| Unpacked | Packed | Breakdown |
|----------|--------|-----------|
| 2015 BA620 | _FB0000 | Year 15→F, B=half-month, seq=(620-620)×25+0=0 |
| 2015 BB620 | _FB0001 | seq=(620-620)×25+1=1 |
| 2015 BP620 | _FB000E | seq=(620-620)×25+14=14 (E in base62) |
| 2024 AB631 | _OA004R | Year 24→O, A=half-month, seq=(631-620)×25+1=276 |

### 2.6 Old-Style Provisional Designations

**⚠️ CRITICAL AMBIGUITY - REQUIRES MPC CLARIFICATION**

There are TWO different "old-style" designation formats that are often confused:

#### 2.6.1 A-Prefix Modern Notation (Retrospective)

The MPC uses "A" or "B" prefix notation to represent pre-1925 discoveries in the
MODERN designation system. From [MPC documentation](https://www.minorplanetcenter.net/iau/info/PackedDes.html):

> "This scheme is now also used retrospectively for pre-1925 discoveries.
> For these, the first digit of the year is replaced by an A."

**Format**: `[A|B][century-digit][YY] [half-month][letter]`

| Element | Meaning |
|---------|---------|
| A or B | Replaces first digit of year (A=1xxx, B=2xxx theoretically) |
| Century digit | 8=1800s, 9=1900s |
| Half-month | Modern half-month code (A-Y, not I) |
| Letter | Second letter in modern sense |

**Examples (A-prefix modern notation)**:
| Unpacked | Packed | Meaning |
|----------|--------|---------|
| A908 CJ | J08C00J | Year 1908, Feb 1-15 (C), letter J |
| A873 OA | I73O00A | Year 1873, Jul 16-31 (O), letter A |
| A801 AA | I01A00A | (1 Ceres) Year 1801, Jan 1-15, letter A |

#### 2.6.2 Original Historical Designations (pre-1925)

The ORIGINAL designation system used before 1925 was completely different.
From [MPC DesDoc](https://www.minorplanetcenter.net/iau/info/DesDoc.html):

- **1892**: Year + single letter (1892 A, 1892 B, ..., omitting I)
- **1893+**: Double letters when 25 proved insufficient (1893 AA, 1893 AB, ...)
- Letters were **sequential**, NOT based on half-month of discovery
- The double-letter sequence continued across years until 1916 ZZ, then restarted

**Critical Example - These are NOT the same object**:
| Original Designation | Modern Equivalent | Asteroid |
|---------------------|-------------------|----------|
| 1893 AP | A893 XA | (378) Holmia |

The object historically designated "1893 AP" (the 16th double-letter designation
of 1893) is now known as (378) Holmia. Its MODERN provisional designation is
"A893 XA" (discovered in second half of November), NOT "A893 AP".

#### 2.6.3 Current Implementation Behavior

The implementations in this repository treat A-prefix format as modern notation:
- `A908 CJ` → `J08C00J` (correct for A-prefix modern notation)
- Unpacking `J08C00J` → `1908 CJ` (normalizes to 4-digit year)

**⚠️ LIMITATION**: The implementations do NOT handle original historical designations
like "1893 AP". These cannot be round-tripped because:
1. They use a completely different lettering scheme
2. No algorithm exists to convert between original and modern designations
3. Conversion requires a lookup table of historical assignments

#### 2.6.4 Questions for MPC Help Desk

1. Is there a published mapping between original historical designations
   (1893 AP style) and modern A-prefix designations (A893 XA style)?

2. Should implementations reject original historical format input, or is there
   an expected conversion behavior?

3. What is the canonical unpacked representation for pre-1925 objects:
   "A908 CJ" or "1908 CJ"?

---

## Part 3: Survey Designations

Four historical survey programs have special packed formats.

### 3.1 Survey Codes

| Survey | Packed Prefix | Unpacked Suffix | Year |
|--------|---------------|-----------------|------|
| Palomar-Leiden | PLS | P-L | 1960 |
| Trojan Survey 1 | T1S | T-1 | 1971 |
| Trojan Survey 2 | T2S | T-2 | 1973 |
| Trojan Survey 3 | T3S | T-3 | 1977 |

### 3.2 Format

**Packed**: `[Survey-prefix][4-digit-number]` (7 characters)
**Unpacked**: `[4-digit-number] [Survey-suffix]`

| Unpacked | Packed |
|----------|--------|
| 2040 P-L | PLS2040 |
| 3138 T-1 | T1S3138 |
| 1010 T-2 | T2S1010 |
| 4101 T-3 | T3S4101 |

**Constraints**:
- Survey number must be 1-9999 (0 is invalid)
- Number is zero-padded to 4 digits

---

## Part 4: Comet Designations

### 4.1 Comet Type Codes

| Code | Type | Definition |
|------|------|------------|
| P | Periodic | Period < 200 years or multiple apparitions |
| C | Non-periodic | Long-period or single-apparition |
| D | Defunct | Periodic comet that has disappeared |
| X | Uncertain | Cannot compute meaningful orbit |
| A | Asteroid-like | Minor planet in cometary orbit |
| I | Interstellar | Object from outside solar system |

### 4.2 Numbered (Periodic) Comets

**Unpacked**: `[1-9999][P|D]` with optional `/name`
**Packed**: `[4-digit-zero-padded][P|D]` (5 characters)

| Unpacked | Packed | Notes |
|----------|--------|-------|
| 1P | 0001P | Halley's Comet |
| 1P/Halley | 0001P | Name is informational |
| 1D | 0001D | Defunct periodic |
| 354P | 0354P | 354th periodic comet |
| 9999P | 9999P | Maximum numbered |

**Constraints**: Number 1-9999 only; 0 is invalid.

### 4.3 Comet Provisional - Modern (year ≥ 1000)

**Structure** (8 characters):
`[Type][Century][YY][Half-month][Order-tens][Order-ones][Fragment]`

| Position | Field | Values |
|----------|-------|--------|
| 1 | Type | P, C, D, X, A, I |
| 2 | Century | I-L (same as asteroids) |
| 3-4 | Year | 00-99 |
| 5 | Half-month | A-Y (not I) |
| 6-7 | Order | 00-z9 (same encoding as cycle) |
| 8 | Fragment | 0 (none) or a-z (A-Z unpacked) |

**Fragment handling**:
- `0` = no fragment
- Lowercase letter = single fragment (A→a, B→b, etc.)
- For two-letter fragments, 9 characters total

| Unpacked | Packed | Notes |
|----------|--------|-------|
| C/1995 O1 | CJ95O010 | No fragment |
| P/2019 A4 | PK19A040 | No fragment |
| D/1993 F2-A | DJ93F02a | Single fragment |
| D/1993 F2-B | DJ93F02b | Single fragment |
| P/1930 J1-AA | PJ30J01aa | Two-letter fragment (9 chars) |
| P/1930 J1-AZ | PJ30J01az | Two-letter fragment |

### 4.4 Comet Provisional - Ancient (1 ≤ year < 1000)

For years before 1000 CE, the century code is replaced with a 3-digit year.

**Structure** (8 characters):
`[Type][3-digit-year][Half-month][Order-tens][Order-ones][Fragment]`

| Unpacked | Packed | Notes |
|----------|--------|-------|
| C/240 V1 | C240V010 | Year 240 CE |
| C/837 F1 | C837F010 | Year 837 CE |
| C/574 A1 | C574A010 | Year 574 CE |

### 4.5 Comet Provisional - BCE (year < 0)

BCE years use special prefix encoding.

**Structure** (8 characters):
`[Type][BCE-prefix][Code][Half-month][Order-tens][Order-ones][Fragment]`

**BCE prefix and code calculation**:
| BCE Range | Prefix | Code Calculation |
|-----------|--------|------------------|
| -1 to -99 | / | 99 - abs(year) |
| -100 to -199 | . | 99 - (abs(year) - 100) |
| -200 to -299 | - | 99 - (abs(year) - 200) |

| Unpacked | Year | Prefix | Code | Packed |
|----------|------|--------|------|--------|
| C/-43 K1 | -43 | / | 56 (=99-43) | C/56K010 |
| C/-87 A1 | -87 | / | 12 (=99-87) | C/12A010 |
| C/-146 P1 | -146 | . | 53 (=99-46) | C.53P010 |
| C/-240 V1 | -240 | - | 59 (=99-40) | C-59V010 |

**Note**: BCE encoding uses the complement method to fit in 2 digits.

### 4.6 Full Comet Designations (with periodic number)

Comets with both a periodic number and provisional designation.

**Structure** (12 characters):
`[4-digit-number][Type][7-char-provisional]`

| Unpacked | Packed |
|----------|--------|
| 1P/1986 F1 | 0001PJ86F010 |
| 29P/1993 F1 | 0029PJ93F010 |

### 4.7 Comet Fragment Packing Rules

**⚠️ REQUIRES MPC CLARIFICATION FOR TWO-LETTER FRAGMENTS**

#### 4.7.1 Single-Letter Fragments (Documented)

From [MPC PackedDes.html](https://www.minorplanetcenter.net/iau/info/PackedDes.html):

> "Column 7 will normally be '0', except for split comets, when the fragment
> designation is stored there as a lower-case letter."

**Packing rule for single-letter fragments**:
- Fragment letter A-Z in unpacked form
- Stored as lowercase a-z in position 8 of packed format
- Position 8 = `0` means no fragment

| Unpacked | Packed (8 chars) | Fragment |
|----------|------------------|----------|
| D/1993 F2 | DJ93F020 | None |
| D/1993 F2-A | DJ93F02a | A → a |
| D/1993 F2-B | DJ93F02b | B → b |
| D/1993 F2-G | DJ93F02g | G → g |
| D/1993 F2-W | DJ93F02w | W → w |

**Real-world example**: Comet Shoemaker-Levy 9 (D/1993 F2) broke into 21 fragments
labeled A through W. Each fragment was designated D/1993 F2-A, D/1993 F2-B, etc.

#### 4.7.2 Two-Letter Fragments (NOT Documented by MPC)

The MPC PackedDes.html does **NOT** document how two-letter fragments (like -AA,
-AB, -AZ) should be packed. The implementations in this repository use an
**inferred** 9-character format:

**Inferred packing rule for two-letter fragments**:
- Two lowercase letters in positions 8-9
- Total packed length: 9 characters (vs. 8 for single-letter)

| Unpacked | Packed (9 chars) | Fragment | Status |
|----------|------------------|----------|--------|
| P/1930 J1-AA | PJ30J01aa | AA → aa | ⚠️ INFERRED |
| P/1930 J1-AB | PJ30J01ab | AB → ab | ⚠️ INFERRED |
| P/1930 J1-AZ | PJ30J01az | AZ → az | ⚠️ INFERRED |

**Questions about two-letter fragments**:
1. What is the official packed format for two-letter comet fragments?
2. Are there any comets with two-letter fragment designations in the MPC database?
3. Is the 9-character format (8 + 2 lowercase letters) correct?

#### 4.7.3 Fragment Packing for Different Comet Types

Fragments can apply to any comet designation type:

| Type | Unpacked | Packed | Notes |
|------|----------|--------|-------|
| Provisional modern | D/1993 F2-B | DJ93F02b | 8 chars |
| Provisional ancient | C/240 V1-A | C240V01a | 8 chars |
| Provisional BCE | C/-43 K1-A | C/56K01a | 8 chars |
| Full with number | 1P/1986 F1-A | 0001PJ86F01a | 12 chars |

#### 4.7.4 Questions for MPC Help Desk - Comet Fragments

1. **Two-letter fragments**: What is the official packed representation for
   comet fragments with two letters (e.g., D/1993 F2-AA if it existed)?

2. **Maximum fragments**: Is there a maximum number of fragments a comet can
   have? The single-letter scheme allows 26 (A-Z), but what happens beyond Z?

3. **Fragment ordering**: Are fragment letters always sequential (A, B, C, ...)
   or can they skip letters?

4. **Lowercase in packed format**: Is it correct that fragments are always
   lowercase in the packed format (A→a, B→b)?

---

## Part 5: Natural Satellite Designations

### 5.1 Planet Codes

| Code | Planet |
|------|--------|
| J | Jupiter |
| S | Saturn |
| U | Uranus |
| N | Neptune |

### 5.2 Provisional Satellites

**Unpacked**: `S/[YYYY] [Planet] [Number]`
**Packed**: `S[Century][YY][Planet][Number-encoded]0` (8 characters)

The number is encoded using the same tens/ones system as cycle counts.

| Unpacked | Packed | Breakdown |
|----------|--------|-----------|
| S/2019 S 22 | SK19S220 | K=2000s, S=Saturn, 22=number |
| S/2003 J 2 | SK03J020 | K=2000s, J=Jupiter |
| S/2018 U 1 | SK18U010 | U=Uranus |
| S/2020 N 1 | SK20N010 | N=Neptune |

**Constraints**:
- Satellite number must be ≥ 1 (0 is invalid)
- Final position is always '0' (padding)

### 5.3 Permanent Satellites

**MPC documentation mentions** permanent satellite format:
`[Planet][3-digit-Roman-numeral]S`

| Unpacked | Packed | Notes |
|----------|--------|-------|
| Jupiter XIII | J013S | Leda |
| Saturn X | S010S | Janus |

**Implementation status**: This format may not be implemented in all language
versions. The test data focuses on provisional satellites.

---

## Part 6: Implementation Verification

### 6.1 Cross-Language Consistency

All 6 implementations (Python, TCL, C, Go, Java, Rust) produce **identical results**
for all 2,021,090 test cases.

### 6.2 Known Limitations

| Limitation | Affected | Notes |
|------------|----------|-------|
| C string null bytes | C | C strings terminate at null; cannot detect embedded nulls |
| Fortran identifier length | SPP | 6-char limit causes "mapping not unique" warnings |
| Unicode input | All | Non-ASCII characters (>126) are rejected |

### 6.3 Boundary Tests Validated

All implementations correctly handle:
- ✓ Asteroid number boundaries (99999↔100000, 619999↔620000)
- ✓ Cycle count boundaries (619↔620 triggering extended format)
- ✓ Maximum values (asteroid 15,396,335, comet 9999P)
- ✓ Year boundaries (centuries I-L)
- ✓ All 24 half-month codes
- ✓ All 25 secondary letters

---

## Part 7: Error Handling

### 7.1 Invalid Input Categories

| Category | Example | Expected Error |
|----------|---------|----------------|
| Empty/whitespace | "", "   " | format |
| Double spaces | "1995  XA" | format |
| Tabs/newlines | "1995\tXA" | format |
| Non-printable | "1995\x00XA" | format |
| Extended ASCII | "1995\x80XA" | format |
| Number zero | "0" (asteroid) | range |
| Negative | "-1" | range |
| Above maximum | "15396336" | range |
| Invalid century | "M95X00A" | format |
| Invalid half-month | "K24I00A" | format (I not used) |
| Lowercase letters | "1995 xa" | format |

### 7.2 Error Handling Differences

| Language | Error Type | Notes |
|----------|-----------|-------|
| Python | ValueError with message | Most detailed |
| TCL | error with message | Standard Tcl error |
| C | Return code + empty output | Buffer not modified on error |
| Go | error type with context | Idiomatic Go |
| Java | Exception with message | Standard Java |
| Rust | Result<T, E> | Type-safe error handling |

---

## Part 8: Rare Edge Cases - Verified

### 8.1 Extended Provisional (cycle ≥ 620)

**Test coverage**: 2015 BA620 through high cycles
**All implementations**: Handle correctly with underscore format

### 8.2 Two-Letter Comet Fragments

**Format**: 9-character packed (standard is 8)
**Test cases**: P/1930 J1-AA → PJ30J01aa
**All implementations**: Handle correctly

### 8.3 BCE Comets

**Test cases**: C/-43 K1, C/-146 P1
**All implementations**: Handle prefix encoding correctly

### 8.4 Ancient Comets (year < 1000)

**Test cases**: C/240 V1, C/837 F1
**All implementations**: Handle 3-digit year format correctly

### 8.5 Case Sensitivity

- Century codes: UPPERCASE only (I, J, K, L)
- Half-month codes: UPPERCASE only (A-Y, not I)
- Secondary letters: UPPERCASE in unpacked, mixed in packed cycle encoding
- Fragments: Lowercase in packed, uppercase in unpacked

---

## Part 9: Potential Ambiguities

### 9.1 Survey vs. Provisional

The packed format `T1S3138` could theoretically be ambiguous:
- Survey: 3138 T-1
- Provisional: Would be `T1S3138` but T1S is recognized as survey prefix

**Resolution**: Survey prefixes (PLS, T1S, T2S, T3S) take precedence.

### 9.2 Old-Style Detection

Old-style `A908 CJ` has similar structure to provisional:
- Space in position 5 distinguishes from 7-char packed
- Century digit (8, 9, 0) identifies era

### 9.3 Comet vs. Asteroid

Provisional comet `CJ95O010` could look like packed asteroid:
- First character C, D, P, X, A, I indicates comet type
- Standard provisionals start with century code I, J, K, L

**Resolution**: Character 1 determines type (comet letter vs. century code).

---

## Part 10: Questions for MPC Help Desk

The following questions require clarification from the Minor Planet Center.
These represent gaps in documentation or inferred behaviors that need verification.

### 10.1 Old-Style vs. A-Prefix Designations

**Background**: Pre-1925 designations used a sequential lettering system
(1892 A, 1892 B, ..., 1893 AA, 1893 AB, ...) that is fundamentally different
from the modern half-month + letter system.

**Questions**:

1. **Mapping table**: Is there a published mapping between original historical
   designations (e.g., "1893 AP") and their modern equivalents (e.g., "A893 XA"
   for asteroid 378 Holmia)?

2. **Input handling**: Should software implementations:
   - Accept only A-prefix format ("A908 CJ") for pre-1925 objects?
   - Accept only the original format ("1908 CJ")?
   - Accept both, treating them as equivalent?
   - Reject original format since it cannot be algorithmically converted?

3. **Canonical representation**: For pre-1925 objects, what is the canonical
   unpacked representation: "A908 CJ" or "1908 CJ"?

4. **Year digit meaning**: In "A908 CJ", does the "A" prefix simply replace
   the first digit of the year (making it equivalent to "1908 CJ"), or does
   it have additional semantic meaning?

### 10.2 Comet Fragment Packed Format

**Background**: [PackedDes.html](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
documents single-letter fragments stored as lowercase in position 8, but does not
address two-letter fragments.

**Questions**:

1. **Two-letter format**: What is the official packed representation for
   two-letter comet fragments (e.g., if D/1993 F2-AA existed)?
   - Is it 9 characters with two lowercase letters (e.g., "DJ93F02aa")?
   - Or is there a different encoding scheme?

2. **Do two-letter fragments exist?**: Are there any comets in the MPC database
   with two-letter fragment designations (AA, AB, etc.)?

3. **Fragment maximum**: What happens when a comet has more than 26 fragments?
   - Are fragments always assigned A through Z?
   - Can letters be skipped (e.g., A, B, D, skipping C)?

4. **Case conversion**: Is lowercase for packed fragments mandatory?
   Specifically: D/1993 F2-A always packs to "DJ93F02a" (lowercase), correct?

### 10.3 BCE Comet Encoding

**Background**: The implementations use a complement encoding for BCE years:
- Prefix `/` for years -1 to -99, code = 99 - abs(year)
- Prefix `.` for years -100 to -199, code = 99 - (abs(year) - 100)
- Prefix `-` for years -200 to -299, code = 99 - (abs(year) - 200)

**Questions**:

1. **Documentation**: Is this encoding scheme documented anywhere officially?
   We inferred it from patterns in the data.

2. **Year range**: What is the minimum BCE year that can be encoded?
   The current scheme supports -1 to -299. Are older comets possible?

3. **Prefix characters**: Are `/`, `.`, `-` the correct prefix characters for
   BCE year ranges? These overlap with path separators and may cause issues.

### 10.4 Permanent Natural Satellites

**Background**: [PackedDes.html](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
mentions permanent satellite format `[Planet][3-digit-number]S` (e.g., "J013S"
for Jupiter XIII), but this is not well documented.

**Questions**:

1. **Format confirmation**: Is "J013S" the correct packed format for Jupiter XIII?

2. **Number encoding**: Is the Roman numeral converted to decimal and zero-padded
   to 3 digits?

3. **Maximum**: What is the maximum satellite number that can be encoded?

---

## Summary

This reference covers all known MPC designation formats:
- **Permanent asteroids**: 3 encoding tiers (1-15,396,335)
- **Provisional asteroids**: Standard (0-619 cycle) and extended (620+)
- **Old-style provisional**: A-prefix format for pre-1925 discoveries
- **Survey designations**: 4 historical survey programs
- **Comets**: Numbered, provisional (modern/ancient/BCE), fragments, full format
- **Natural satellites**: Provisional format

All implementations in this repository handle these formats identically,
validated by 2,021,090+ test cases with zero discrepancies.

**⚠️ Areas requiring MPC clarification**:
1. Old-style vs. A-prefix designation relationship (Section 10.1)
2. Two-letter comet fragment encoding (Section 10.2)
3. BCE comet year encoding verification (Section 10.3)
4. Permanent satellite packed format (Section 10.4)

---

## Sources

1. [MPC Packed Designation Format](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
2. [MPC Old-Style Designations](https://www.minorplanetcenter.net/iau/info/DesDoc.html)
3. [MPC Provisional Designation Definition](https://www.minorplanetcenter.net/mpcops/documentation/provisional-designation-definition/)
4. [PDS Small Bodies Node - Target Names](https://pds-smallbodies.astro.umd.edu/howto/sbn_target_names.shtml)
5. Repository test data: 2,021,090 designation pairs + 114 error cases
