# Claude Notes: MPC Designation Library Updates

This document captures the context and rationale for changes made to the TCL implementation.
These notes should inform updates to all other language implementations (Python, C, Go, Java,
Rust, JavaScript, etc.) to ensure consistent behavior across the library.

## Summary of Changes

### 1. Numbered Comet Fragment Support

**What**: Added support for numbered comets with fragment designations (e.g., 73P-A, 73P-AA).

**Format**:
- Unpacked: `73P-A` (single-letter) or `73P-AA` (two-letter)
- Packed: `0073Pa` (6 chars) or `0073Paa` (7 chars)

**Rules**:
- Fragment letters are A-Z (including I - see note below)
- Fragment is appended as lowercase to the 5-char numbered comet format
- Single-letter fragments: 6 characters total (e.g., `0073Pa`)
- Two-letter fragments: 7 characters total (e.g., `0073Paa`)

**Regex patterns**:
```
Unpacked: ^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$
Packed:   ^(\d{4})([PD])([a-z]{1,2})?$
```

### 2. Provisional Comet Two-Letter Fragments

**What**: Two-letter fragments on provisional comets (e.g., P/1930 J1-AA) now pack normally.

**Format**:
- Unpacked: `P/1930 J1-AA`
- Packed: `PJ30J01aa` (9 characters)

**Note**: The MPC Help Desk indicated there's "no official packed format policy" for two-letter
fragments, but the MPC data itself contains these packed formats. We follow the data.

### 3. Fragment Letter I is NOT Skipped

**Important**: Unlike half-month codes where I is skipped, fragment letters include I.

The MPC data contains designations like:
- `P/1930 J1-AI` → `PJ30J01ai`
- `P/1930 J1-BI` → `PJ30J01bi`

All implementations must accept A-Z (including I) for fragment letters.

### 4. Pre-1925 Designation Unpacking (A-Prefix Format)

**What**: When unpacking pre-1925 asteroid provisionals, output uses A-prefix format.

**Rationale**: Per MPC Help Desk, A-prefix format (e.g., `A893 XA`) is the PRIMARY designation
for pre-1925 objects, not a conversion from historical format.

**Behavior**:
- Pack: `A908 CJ` → `J08C00J` (unchanged)
- Unpack: `J08C00J` → `A908 CJ` (NOT `1908 CJ`)
- Unpack: `I01A00A` → `A801 AA` (NOT `1801 AA`)

**Implementation**:
```
if year < 1925:
    first_digit = year[0]  # '1' or '2'
    rest = year[1:]        # e.g., '908' for 1908
    if first_digit == '1':
        prefix = 'A'
    elif first_digit == '2':
        prefix = 'B'
    return f"{prefix}{rest} {half_month}{letter}{cycle}"
```

### 5. Century Code Validation

**What**: Added validation for century codes in packed provisional designations.

**Asteroid Provisionals**:
- Valid century codes: I, J, K, L
- Valid year range: 1800-2199
- Reject: A-H (years before 1800)

**Comet Provisionals**:
- Valid century codes: A, B, C, D, E, F, G, H, I, J, K, L
- Valid year range: 1000-2199
- Historical comets exist back to year 1014 (e.g., C/1014 C1 → CA14C010)

**Century code mapping**:
```
A=10 (1000s), B=11 (1100s), C=12 (1200s), D=13 (1300s),
E=14 (1400s), F=15 (1500s), G=16 (1600s), H=17 (1700s),
I=18 (1800s), J=19 (1900s), K=20 (2000s), L=21 (2100s)
```

### 6. Removed Passthrough Logic

**What**: Earlier implementation had "passthrough" logic where designations without official
packed formats would be returned unchanged. This has been removed.

**Rationale**: The MPC data contains packed formats for all designation types we encountered,
including two-letter fragments on provisional comets. We follow the data rather than
theoretical "no official format" concerns.

## Test Data Updates

### prov_unpack_to_pack.csv

The master test file was updated by merging:
- `primary_objects.csv` (1,538,116 entries)
- `secondary_designations.csv` (2,022,382 entries)
- `comet_names.csv` (4,295 entries)

**Final merged file**: 2,022,404 unique entries

**8 entries in primary but not secondary**:
- 1977 UB, 1999 RE70, 2000 EC98, 2003 BM80, 2005 QN173, 2008 GO98, C/2002 VQ94, C/2012 KA51

**14 comets unique to comet_names.csv**:
- 1981 RF1, 1981 UH18, 1982 YG3, 1983 RD6, 1986 TD4, 1986 TF, 1986 WP5, 471P,
  C/2019 T5, P/1975 C1, P/1979 M2, P/1980 P1, P/1985 G1, P/2006 VW139

### error_test_cases.csv

Updated test cases:
- `missing_space_survey`: Changed from `2040P-L` (now valid as numbered comet with fragment)
  to `T-1 3138` (invalid survey format)
- `min_year`: Changed from `1000 AA` to `1800 AA` (minimum valid asteroid year)

## Implementation Checklist for Other Languages

When updating Python, C, Go, Java, Rust, JavaScript, etc.:

- [ ] Add numbered comet fragment support (pack/unpack)
- [ ] Add two-letter fragment support for provisional comets
- [ ] Include letter I in fragment validation (A-Z, not A-H,J-Z)
- [ ] Update unpackProvisional to output A-prefix format for years < 1925
- [ ] Add century code validation:
  - Asteroids: I-L only (1800-2199)
  - Comets: A-L (1000-2199)
- [ ] Update regex patterns to match new formats
- [ ] Add/update fragment test cases
- [ ] Validate against updated prov_unpack_to_pack.csv (2,022,404 entries)

## Key Regex Patterns (Reference)

### Numbered Comet with Fragment
```
Unpacked: ^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$
Packed:   ^(\d{4})([PD])([a-z]{1,2})?$
```

### Provisional Comet with Fragment
```
Unpacked: ^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$
Packed 7-char: ^[A-L]\d{2}[A-Z]\d{2}[0-9a-z]$
Packed 8-char: ^[A-L]\d{2}[A-Z]\d{2}[a-z]{2}$
```

## MPC Help Desk Clarifications

1. **A-prefix designations**: Only accept A-prefix format for pre-1925 objects. The "A" means
   the first digit of year is 1. These are MPC-assigned primary designations.

2. **Two-letter comet fragments**: Exist only for numbered comets in practice (73P has 74+
   fragments). No official packed format policy exists, but data uses lowercase letters.

3. **BCE comet encoding**: Current inferred format acceptable until MPC documents it.

4. **Permanent satellites**: J013S format confirmed (planet + 3-digit decimal + S).
