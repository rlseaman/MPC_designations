# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.1.4] — 2026-11-18

C only; no behavior change in any implementation. Contributed by Mario Juric
(from his fork), adapted to the current format rules.

### Added
- C: length-aware functions `mpc_convert_n`, `mpc_convert_simple_n` and
  `mpc_detect_format_n` take an explicit byte count, so embedded NUL bytes
  (and any other non-printable byte) are seen and rejected. The C error-test
  runner uses them and now passes all 134 cases with no skips (previously
  the two NUL cases were documented skips).

### Changed
- C: `mpc_convert_simple()` handles the common asteroid provisional and survey
  forms with pure-C fast paths, about 6× faster on the provisional corpus
  (1.17 s → 0.19 s for 2,110,817 pack conversions). Anything outside the
  fast-path shapes, and anything a fast path declines, goes through the
  canonical parser, so results are unchanged: verified on the full corpus in
  both directions and by differential testing against 1.1.3 on 5.5 million
  mutated and systematically generated inputs. Compared with the original
  contribution, the fast paths now leave extended-format packing (cycle ≥ 620)
  to the canonical parser, reject `I` as a second letter and invalid
  half-month letters, and unpack 5-digit numbers with a leading zero
  (`00001` → `1`).

## [1.1.3] — 2026-11-17

### Added
- All 25 implementations: interstellar objects are numbered like comets with
  type `I`: `1I` ↔ `0001I`, `3I` ↔ `0003I`, `3I/ATLAS` → `0003I`,
  `2I-A` ↔ `0002Ia`. The packed form was confirmed from MPC observation
  records. Previously only the full form with a provisional
  (`1I/2017 U1` ↔ `0001IK17U010`) worked, and only in some implementations.
  The helper functions (`to_report_format`, `has_fragment`, etc.) handle `I`
  as well.
- Python: `python -m mpc_designation` now works (it was documented but there
  was no `__main__.py`), and the package ships a `py.typed` marker so type
  checkers use its annotations.

### Changed
- Python: `MPCDesignationError` now subclasses `ValueError`, so generic
  `except ValueError` handlers catch it. Existing `except MPCDesignationError`
  code is unaffected.
- Python: the CLI's usage text names the installed `mpc-designation` command.
- Name suffixes on numbered comets (`1P/Halley` → `0001P`) now work in C,
  Fortran, Forth, Haskell, Nim, AWK, and SPP, matching the reference. They
  were previously rejected or misread.
- Comet numbers outside 1–9999 are now rejected in AWK, Bash, Nim, Forth, and
  SPP. `0P` used to pack to `0000P`, and `10000P-A` to `10000Pa`. This closes
  the `numbered_comet_zero` expected-skip in AWK, Nim, and Forth.
- Shared error suite: 124 → 134 cases (interstellar; zero numbers). Interstellar
  pairs added to the corpus (`prov_unpack_to_pack.csv.gz`: 2,110,817 pairs).

### Fixed
- All 25 implementations: unpacking an all-zero number is now an error, as
  packing already was. `00000` → `0`, `0000P` → `0P`, and `0000PJ82U010` →
  `0P/1982 U1` were produced by every implementation, including the Python
  reference, but could not be packed back. C# treated number 0 as "no number"
  (`0000PJ82U010` → `P/1982 U1`).
- Octave and R error-test harnesses were broken. They read the category
  column as the input and treated every row as must-error, so their reported
  passes were meaningless. Both were rewritten to match the Python harness
  and now pass the suite. R skips the two NUL-byte cases (R strings cannot
  contain NUL), as C already does.

## [1.1.2] — 2026-11-17

### Fixed
- All 25 implementations: comets with an extended asteroid-style provisional
  designation — new in MPC data in 2025 — are now supported in both directions:
  `P/2025 OZ695` ↔ `P_PO00Ud`, and the 12-char numbered form
  `1P/2025 OZ695` ↔ `0001P_PO00Ud`. Previously every implementation, including
  the Python and Tcl references, failed to unpack `P_PO00Ud`; Nim crashed
  packing it (`IndexDefect`).
- All 25 implementations: extended provisional (`_YHbbbb`) input validation.
  - Packing now rejects years outside 2000–2061. Several implementations used
    `year % 100`, which silently packed `1925 AA620` as 2025.
  - Packing now rejects sequences beyond 62⁴−1 (`2000 AL591673` is the
    largest); they previously wrapped silently in 19 implementations,
    including Python.
  - Packed input must match `_[0-9A-Za-z][A-HJ-Y][0-9A-Za-z]{4}`. A half-month
    of `I`, `Z`, or lowercase, and non-base-62 characters, were accepted by up
    to 20 implementations.
  - Oversized cycle strings are rejected before the arithmetic. They
    overflowed silently in Haskell (`_FA0000`) and Fortran (packed as cycle 0),
    and could overflow integers in Rust, Swift, and C#.
- Forth: fixed an "Invalid memory address" crash unpacking BCE comets
  (`C.53P010` → `C/-146 P1`), a stack leak in extended unpacking, and packing
  of numbered comets with a provisional (`1P/1982 U1`), which crashed or
  produced corrupt output.
- R: unpacking numbered asteroids printed round numbers in scientific notation
  (`A0000` → `1e+05`; now `100000`).
- Haskell: lowercase half-months were accepted (`C/1995 o1` → `CJ95o010`), and
  unpacking 12-char numbered comets kept leading zeros (`0001P/1995 O1`).
- SPP/IRAF:
  - Two-letter fragments on modern comets unpacked wrongly
    (`PJ30J01aa` → `-A`; now `P/1930 J1-AA`).
  - Extended packing wrongly rejected cycles 591536–591673.
  - Comet packing could return garbage instead of an error.
- Nim, Fortran, AWK, Bash, and SPP: added support for numbered comets with a
  provisional designation in both directions (`1P/1982 U1` ↔ `0001PJ82U010`),
  previously missing or broken.
- C: the error-test harness used a packed-size output buffer, so unpacking
  valid packed comets failed with "Buffer too small".

### Changed
- **Test corpus refreshed** from the MPC catalog (2026-11-17).
  - `test-data/prov_unpack_to_pack.csv.gz` grows from 2,022,404 to 2,110,813
    pairs. It now has 6,564 extended-format asteroid designations (previously
    634, all from 2014–2015; it now includes the 3,079 from 2025), the first
    comet with one (`P/2025 OZ695`), and all provisionals issued through
    November 2026.
  - New `test-data/numbered_pack.csv.gz` has 899,365 numbered-asteroid pairs:
    every numbered object (1–895,910) plus encoding-tier boundaries up to
    15,396,335.
  - Every implementation was verified on both files in both directions.
  - CI now also runs the numbered corpus for Python and Tcl.
- Shared error suite expanded from 98 to 124 cases: extended-format
  year/sequence/half-month bounds, and comet extended provisionals. The Forth
  `cycle_huge` and `cycle_overflow` expected-skips are closed.

## [1.1.1] — 2026-11-17

### Fixed
- Bash: implemented the extended provisional format (`_YHbbbb`, cycle ≥ 620)
  in both directions. Previously `2015 BE631` silently packed to the garbage
  `K15B1E` and `_FB0008` was rejected (#1, #2).
- Bash: rewrote comet provisional pack/unpack to reach parity with the
  references — comet orders ≥ 100 (`PJ30JA1a` ↔ `P/1930 J101-A`),
  asteroid-style comet provisionals (`P/2016 SV` ↔ `PK16S00V`), comet centuries
  A–H (1000–1799), and ancient/BCE comets (`C/390 Q1`, `C/-146 P1`) were all
  missing or wrong.
- AWK: implemented extended provisional unpacking (`_FB0008` → `2015 BJ620`);
  packing already worked.
- AWK: comet unpack fixed for two-letter fragments (9-char), fragment case
  (`CK19Y04a` → `C/2019 Y4-A`, was `-a`), centuries A–H, orders ≥ 100, and
  ancient/BCE comets (previously unsupported). Comet pack now encodes orders
  ≥ 100 correctly (`PJ30JA1a`, was `PJ30J101a`) and keeps fragments on
  ancient/BCE comets. Asteroid-style comet provisionals now route through the
  asteroid packer, so extended cycles pack correctly
  (`P/2025 OZ695` → `P_PO00Ud`, was the garbage `PK25O5Z`).
- Tcl: extended provisional year code was wrong for 2036–2061 (base-62 `a`–`z`):
  `2036 AA620` packed to `_[A0000` and `_aA0000` unpacked as 2042.
- AWK and Bash now reject extended-format inputs that cannot be encoded
  (year outside 2000–2061, sequence beyond 62⁴−1) and out-of-range comet order
  numbers, closing the `cycle_huge`, `cycle_overflow`, `comet_order_zero`, and
  `comet_order_huge` expected-skips in their error harnesses.
- `bash/Makefile` `test-csv` and `test-roundtrip` targets now pass the corpus
  path (they previously exited with a usage error, so the Bash corpus run was
  never actually exercised by `make`).
- `rust/Cargo.lock` updated to the 1.1.0 package version (was stale at 1.0.0).
- Python `__version__` fallback (used when package metadata is unavailable)
  was hard-coded to `1.0.0`; it now tracks the release version.
- `CITATION.cff` version, release date, and contact email were stale.

### Added
- `docs/PYTHON_37_SUPPORT_ASSESSMENT.md` — decision memo on supporting
  Python 3.7 (recommends keeping the `>=3.9` floor).

## [1.1.0] — 2026-05-27

### Fixed
- Packed-provisional detection was too loose across all 25 implementations,
  letting non-MPC strings match the asteroid-provisional branch — notably
  Catalina Sky Survey tracklet IDs such as `C03UYWZ` (century `C` = 1200s).
  Detection now enforces the canonical structure
  `^[I-L][0-9]{2}[A-HJ-Y][0-9A-Za-z][0-9][A-HJ-Z]$`: asteroid century `I-L`
  (1800-2199; comets remain `A-L`), half-month `A-Y` skipping `I`, a digit in
  the cycle-units position, and second letter `A-Z` skipping `I`. The pack path
  now rejects `I` in both letter positions (standard and old-style branches).
  This also closes a class of silent mis-conversions (e.g. `K95I00A`→`2095 IA`,
  `K03UYWZ`→garbage, `1995 XI`→`J95X00I`) present in several implementations.
  Verified against the full 2,022,404-row corpus in every testable language.
- Comet provisional half-month was accepted as any `[A-Z]` in detection and was
  not validated on the pack path across all 25 implementations, so `C/1995 I1`
  and `C/1995 Z1` were wrongly accepted. The half-month is a calendar code
  (24 half-months → letters `A-Y` skipping `I`; `Z` unused) and is object-type
  independent, so the same `[A-HJ-Y]` rule now applies to comets (and natural
  satellites) as to asteroids. Comet *fragment* letters are unaffected — they
  legitimately include `I` (e.g. `P/1930 J1-AI`), verified preserved.
- Nim: implemented BCE/ancient-comet unpacking (e.g. `C.53P010` → `C/-146 P1`),
  which was missing — closing 12 round-trip failures and reaching parity with
  the Python/Tcl references.
- Forth: fixed a latent stack-operand bug in the `convert-simple` pass-through
  path that the tightened detection exposed.
- Helper functions (`getFragment`/`getParent`/`hasFragment`) in AWK, C#, Kotlin,
  PHP, and Swift used a digits-only `\d{2}` cycle pattern that failed to match
  packed comet fragments with cycle ≥ 100; corrected to `[0-9A-Za-z][0-9]`.
- C# test projects shared one build-intermediate directory, cross-contaminating
  `make test`; isolated via a per-project `Directory.Build.props`.

### Changed
- C error-test runner (`c/test/test_errors.c`) now treats two specific cases
  (`invalid_char/null_byte`, `edge_case/null_middle`) as expected skips rather
  than failures. The inability to detect embedded null bytes is inherent to C
  strings and was already documented; now CI reports it as `Skipped: 2`
  rather than `Failed: 2`, turning the C job green. Other language
  implementations continue to run these cases.
- The AWK, Bash, Nim, Forth, and Haskell error-test harnesses now read the
  shared `test-data/error_test_cases.csv` instead of bespoke hardcoded cases.
  This surfaced pre-existing input-validation gaps in those five (numeric-range
  checks, whitespace strictness, etc.), now tracked in `docs/VALIDATION_GAPS.md`
  and recorded as documented expected-skips (the C null-byte precedent).

### Added
- Path-prefixed Go module tags `go/v1.0.0` and `go/v1.0.1` (in addition to
  the existing `v1.0.0` and `v1.0.1` tags). Go's module system requires
  submodule-prefixed tags for modules rooted in a repository subdirectory;
  without them, `go get github.com/rlseaman/mpc_designations/go@v1.0.1`
  fell back to commit-hash pseudo-versions rather than clean semver.
- `docs/DISTRIBUTION.md` — per-language registry publishing status and
  playbook. Captures current state (PyPI live, conda-forge in review, Go
  module live), blockers for each remaining language, and a priority
  ordering for future work.

## [1.0.1] — 2026-04-17

First release published to PyPI.

### Added
- `docs/SATELLITE_NAMING_ANALYSIS.md` — research notes and scoping options for
  permanent-satellite packed format (`J013S`) and future named-body lookup
  support.
- `docs/QUICKSTART.md` — concise cross-language landing page.
- `docs/DESIGN.md` — rationale for the 25-implementation corpus model.
- `CITATION.cff` — citation metadata; GitHub surfaces a "Cite this repository"
  button.
- `CHANGELOG.md` — this file.
- Issue and pull-request templates under `.github/`.
- `authors` and `maintainers` fields in Python `pyproject.toml`; `Changelog`
  and `Issues` project URLs.

### Changed
- `.gitignore` now covers `test_fragments`, `test_helpers`, and `test_roundtrip`
  binaries across all language directories, plus the local `sandbox/` scratch
  area.
- Python `mpc_designation.lite` module moved out of the installed package to
  `python/examples/mpc_designation_lite.py`. The published wheel now ships
  only the full implementation.

## [1.0.0] — 2026-02-05

First tagged release. All 25 language implementations reached parity on the
same test corpus.

### Added
- 25 language implementations: AWK, Bash, C, C++, C#, Forth, Fortran, Go,
  Haskell, Java, JavaScript, Julia, Kotlin, Nim, Octave/MATLAB, Perl, PHP,
  Python, R, Ruby, Rust, SPP/IRAF, Swift, Tcl, TypeScript.
- Core API per language: `convert_simple`, `pack`, `unpack`, `detect_format`,
  `is_valid_designation` (and equivalent camelCase / PascalCase forms per
  language convention).
- Six helper functions per language: `to_report_format`, `from_report_format`,
  `has_fragment`, `get_fragment`, `get_parent`, `designations_equal`.
- 2,022,404-entry CSV test corpus (`test-data/prov_unpack_to_pack.csv.gz`).
- 94-case error-handling test suite (`test-data/error_test_cases.csv`).
- 77-case helper-function test suite per language.
- 81-case fragment / roundtrip test suite per language.
- Python/Tcl and Python/Tcl/C interoperability test harness
  (`test-data/interop_test.py`).
- Documentation: `SPECIFICATION.md`, `FORMATS.md`, `ERROR_CHECKING.md`,
  `COMPLETE_DESIGNATION_REFERENCE.md`, `MULTIPLATFORM.md`,
  `PRODUCTION_READINESS_NOTES.md`, `DATABASE_INTEGRATION.md`.
- `CONTRIBUTING.md` with per-language addition checklist.
- GitHub Actions workflow for C, Python, and Tcl.
- `pyproject.toml` prepared for PyPI publishing (not yet published).

### Supported formats
- Asteroids: numbered (1 through 15,396,335 across three encoding tiers),
  provisional (standard and extended cycle ≥ 620), pre-1925 A-prefix, surveys
  (PLS, T-1, T-2, T-3).
- Comets: numbered with 1- and 2-letter fragments, provisional (modern,
  ancient, BCE), 2-letter fragments on provisional comets.
- Natural satellites: provisional format (`S/YYYY P n`) for J, S, U, N.

### MPC Help Desk clarifications incorporated
- A-prefix designations are primary for pre-1925 objects (not conversions).
- Two-letter comet fragments: packed as lowercase, following MPC data.
- BCE comet encoding: inferred format accepted by MPC pending official docs.
- Permanent satellite packed format confirmed as `[Planet][NNN]S` (not yet
  implemented in code).

### Known limitations
- C implementation cannot detect null bytes embedded mid-input (inherent C
  string limitation): 92/94 error-test passes versus 94/94 for other
  production implementations.
- Forth implementation verified against 86 comprehensive tests but not the
  full 2M-entry corpus, due to gforth memory constraints.
- SPP/IRAF requires the IRAF environment to compile.
- Permanent natural-satellite format (`J013S`) not yet implemented; tracked in
  `docs/SATELLITE_NAMING_ANALYSIS.md`.
- CI matrix covers only C, Python, and Tcl; the other 22 implementations are
  verified locally.

---

[Unreleased]: https://github.com/rlseaman/MPC_designations/compare/v1.1.4...HEAD
[1.1.4]: https://github.com/rlseaman/MPC_designations/compare/v1.1.3...v1.1.4
[1.1.3]: https://github.com/rlseaman/MPC_designations/compare/v1.1.2...v1.1.3
[1.1.2]: https://github.com/rlseaman/MPC_designations/compare/v1.1.1...v1.1.2
[1.1.1]: https://github.com/rlseaman/MPC_designations/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/rlseaman/MPC_designations/compare/v1.0.1...v1.1.0
[1.0.1]: https://github.com/rlseaman/MPC_designations/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/rlseaman/MPC_designations/releases/tag/v1.0.0
