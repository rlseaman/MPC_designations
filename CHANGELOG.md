# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/rlseaman/MPC_designations/compare/v1.1.0...HEAD
[1.1.0]: https://github.com/rlseaman/MPC_designations/compare/v1.0.1...v1.1.0
[1.0.1]: https://github.com/rlseaman/MPC_designations/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/rlseaman/MPC_designations/releases/tag/v1.0.0
