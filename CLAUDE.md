# CLAUDE.md — MPC_designations

Packed ↔ unpacked Minor Planet Center designation converter, implemented in 25
languages as an interoperability reference corpus (see `docs/DESIGN.md`).
Historical change rationale and the per-language feature checklist live in
`CLAUDE_NOTES.md`; validation gaps in `docs/VALIDATION_GAPS.md`.

## Ground rules

- **Python is the reference** (`python/src/mpc_designation/mpc_designation.py`),
  with Tcl as the second reference. Fix behavior there first, then mirror it to
  the other 24. Every behavior change must land in all 25 (including SPP).
- **Test data is the spec.** A change is done only when every implementation
  passes the shared error suite and the full corpus in *both* directions.
- Commit/push/tag/publish only when asked. Keep `VERSION` and all manifests in
  sync.

## Layout

- `<lang>/` — one directory per implementation, each with `src/`, `test/`,
  `Makefile`, `README.md`.
- `test-data/error_test_cases.csv` — shared error suite (134 cases;
  `category,subcategory,input,expected_error,description`, expected
  `valid|format|range`). Harnesses for awk, bash, nim, forth, haskell, C carry
  documented expected-skips.
- `test-data/prov_unpack_to_pack.csv.gz` (2,110,817 pairs) and
  `test-data/numbered_pack.csv.gz` (899,365 pairs) — `unpacked,packed` with one
  header row. Harnesses test the pack direction; to test unpack, swap columns.
  Some harnesses (C, Haskell, Octave, R) call `pack()` only, so an unpack run
  needs a variant that calls `convert_simple`.
- Large source extracts in `test-data/*.csv` (except the error suite) are
  git-ignored.

## Testing

- Per language: `make test` (errors + helpers); `make test-all` adds corpus and
  roundtrip. Rebuild compiled languages before running prebuilt test binaries
  (go's Makefile does not track source changes: use `make -B`).
- Speeds vary by ~4 orders of magnitude: C/Rust/SPP seconds; Bash ~3.7 hours for
  the full corpus (run it in the background, or on a subset containing every
  comet and extended pair).
- gforth drops into its interpreter and spins after an error: run with
  stdin from `/dev/null` and a timeout.
- SPP/IRAF builds locally with `xc`; linking needs `-/fsanitize=undefined`
  because the installed `libvops` was built with UBSan.
- CI (`.github/workflows/test.yml`) covers C, Python (3.9–3.12, pinned to
  ubuntu-24.04 because ubuntu-latest has no 3.9), and Tcl.

## Format gotchas (easy to get wrong)

- Half-month letters A–Y skip I (Z unused); the second provisional letter A–Z
  skips I; comet fragment letters include I.
- Asteroid provisional centuries I–L (1800–2199); comets A–L.
- Extended provisional `_YHbbbb` (cycle ≥ 620): year 2000–2061 only
  (`base62(year-2000)`, never `year % 100`); sequence ≤ 62⁴−1; reject, never
  wrap. Comets use it too: `P/2025 OZ695` ↔ `P_PO00Ud`,
  `1P/2025 OZ695` ↔ `0001P_PO00Ud`. A packed extended provisional can end in a
  lowercase base-62 char, so check for `_` before any "last char uppercase ⇒
  asteroid-style" test.
- Pre-1925 provisionals unpack to the MPC-primary A-prefix form
  (`I73O00A` → `A873 OA`).
- BCE comets: prefixes `/ . -` for 0–99, 100–199, 200–299 BCE with code
  `99 − (|y| % 100)`: `C/-146 P1` ↔ `C.53P010`.

## Refreshing the corpus from the MPC catalog

Export from the MPC database replica (tables `primary_objects`,
`current_identifications`, `numbered_identifications`, `comet_names`), union
with the existing corpus (keep pairs MPC no longer lists), verify every pair
against the Python reference in both directions, then replace the `.gz` files
and update the counts in `README.md`, `test-data/README.md`, `CONTRIBUTING.md`,
`docs/DESIGN.md`, `docs/COMPLETE_DESIGNATION_REFERENCE.md`, and the
`.github` templates.

## Release checklist

1. Roll the CHANGELOG `[Unreleased]` section into `[X.Y.Z] — date`; add the
   compare link.
2. Bump `VERSION`, `CITATION.cff` (version + date-released), `python/pyproject.toml`,
   the `__version__` fallback in `python/src/mpc_designation/__init__.py`,
   `rust/Cargo.toml` (+ `cargo build` to refresh `Cargo.lock`),
   `js/package.json`, `typescript/package.json`.
3. Commit "Release X.Y.Z"; tag `vX.Y.Z` **and** `go/vX.Y.Z`; push both tags.
4. `gh release create vX.Y.Z` with the CHANGELOG section as notes.
5. PyPI: build in a scratch venv (`python -m build`), `twine check`, smoke-test
   the wheel in a clean venv, `twine upload` (credentials in `~/.pypirc`),
   then verify `pip install mpc-designation==X.Y.Z`.
6. Update `docs/DISTRIBUTION.md` status rows.
