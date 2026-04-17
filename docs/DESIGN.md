# Design: Why 25 Parallel Implementations?

This repository is unusual. It contains the **same library, implemented 25
times**, in 25 different programming languages, each passing the same
2,022,404-entry conversion corpus and the same 94-case error suite. That is
deliberate. This document explains why.

## This is a corpus, not a product

Most open-source libraries are products: one canonical implementation, plus
whatever bindings the ecosystem demands. By contrast, this repository is an
**interoperability reference corpus**. Its purpose is to demonstrate —
concretely, exhaustively, and reproducibly — what a correct implementation of
the MPC designation format looks like, in whatever language you happen to
already be using.

The deliverable is not "install this dependency." The deliverable is:

1. A **specification by example**. The 2,022,404-entry test corpus is the
   ground truth. Any reading of the MPC spec that produces different output is
   wrong.
2. A **per-language reference implementation** a user can read, copy, fork, or
   port — without adopting a new runtime, package manager, or dependency graph.
3. A **cross-language validation harness** that catches subtle spec
   misinterpretations by comparing 25 independent implementations against the
   same inputs.

The single-product framing would lead to one library (likely Python or C) and
wrappers. That model fails for this domain because:

- The audience is small and multilingual. Astronomers write scripts in AWK,
  Tcl, Fortran, IRAF/SPP, Perl, R. Pipelines use Go, Rust, C. Notebooks use
  Python and Julia. No single target reaches them all.
- The spec is deceptively simple. Base-62 encoding, half-month codes, century
  letters, A-prefix pre-1925 designations, BCE comet offsets, fragment-letter
  quirks — every language implementation hits the same edge cases. Forcing 25
  independent authors (even if those authors are the same person) to
  rediscover each quirk catches ambiguity in the spec itself.
- Correctness is verifiable. "Does `J08C00J` unpack to `1908 CJ` or `A908 CJ`?"
  becomes a test, not an argument. The 25-way parity check is how we know the
  answer.

## Consequences of this choice

The corpus-as-product framing drives most design decisions:

### No cross-language dependencies

Each language's implementation depends on nothing outside its directory and
its own standard library. No shared build system, no FFI, no binding layer.
This is what makes them independent reference implementations rather than
wrappers around a canonical core.

### Pure grammar, no data tables

The library parses and emits a format. It does not know that `(1) Ceres` is
named "Ceres," nor that `1P` is "Halley." Adding name lookups would introduce
external reference data that must be refreshed, versioned, and synchronized
across 25 languages — transforming the library from a format specification
into a catalog service. That is a different project; see
[`SATELLITE_NAMING_ANALYSIS.md`](SATELLITE_NAMING_ANALYSIS.md) for the
scoping of such a future module.

### Identical API surface, within language norms

Every implementation exposes the same core functions: `convert_simple`,
`pack`, `unpack`, `detect_format`, plus six helpers (`to_report_format`,
`from_report_format`, `has_fragment`, `get_fragment`, `get_parent`,
`designations_equal`). Names are adapted to each language's convention
(`convertSimple` in Java/Tcl/JS, `convert_simple` in Python/Ruby/Rust,
`ConvertSimple` in Go/C#) but the behavior is bit-for-bit identical on the
test corpus.

### Tests are the spec

The authoritative definition of correct behavior lives in
`test-data/prov_unpack_to_pack.csv` (the 2M-entry conversion corpus) and
`test-data/error_test_cases.csv` (the 94 error cases). Per-language tests run
the corpus through each implementation and diff the output. A new
implementation is considered complete when its output matches the corpus
exactly and its error cases return errors rather than nonsense.

### Passthrough is a bug

Earlier drafts of some implementations had "passthrough" logic that returned
unrecognized inputs unchanged. This was removed. Silently accepting invalid
input is worse than rejecting it, because it masks spec misunderstandings.
The test corpus and error-test suite together are meant to prevent
passthrough from reappearing.

## What this repo is *not* trying to be

- **Not an astronomy package.** No ephemerides, no orbital elements, no
  observation planning. Designation-format conversion only.
- **Not an MPC client.** No network calls, no MPC database queries, no
  submission tooling.
- **Not a minor-planet catalog.** No name lookups, no cross-references,
  no discovery metadata.
- **Not a polished single-language product.** Each per-language directory is
  a reference implementation, not a curated library. If you want "pip install"
  convenience for Python specifically, that is a separate packaging concern
  tracked under distribution.

## What success looks like

Success is:

1. An astronomer in any of 25 communities can pick up the implementation for
   their language and use it without learning a new ecosystem.
2. When the MPC clarifies an ambiguous case (as has happened — see MPC help
   desk responses in `COMPLETE_DESIGNATION_REFERENCE.md` Part 11), fixing
   the corpus and propagating the fix through all 25 implementations is a
   straightforward, mechanical exercise, not a design debate.
3. The 25-way parity check on every commit makes it nearly impossible for a
   subtle regression to land in just one language.

## Trade-offs we accept

- **Maintenance cost scales with language count.** Adding a seventh helper
  function means 25 implementations to touch. We accept this because the
  alternative — one canonical implementation plus 24 wrappers — sacrifices
  independence, which is the whole point.
- **Performance varies widely.** The Go implementation runs ~3.5M
  entries/sec; Bash runs ~340/sec. We do not try to hide this. The point is
  correctness parity, not performance parity.
- **Some languages are not fully tested in CI.** Today only C, Python, and
  Tcl run in the GitHub Actions matrix. Expanding this is planned. For
  languages outside CI, the local `make test-<lang>` targets are the
  verification mechanism.

## Reading list for future maintainers

- [`SPECIFICATION.md`](SPECIFICATION.md) — format rules.
- [`COMPLETE_DESIGNATION_REFERENCE.md`](COMPLETE_DESIGNATION_REFERENCE.md) —
  deep reference including MPC help-desk clarifications.
- [`CONTRIBUTING.md`](../CONTRIBUTING.md) — how to add a new language.
- [`CLAUDE_NOTES.md`](../CLAUDE_NOTES.md) — per-language implementation
  checklist and the six-helper-function contract.
- [`DISTRIBUTION.md`](DISTRIBUTION.md) — per-language registry publishing
  status and playbook; revisit each release.
- [`SATELLITE_NAMING_ANALYSIS.md`](SATELLITE_NAMING_ANALYSIS.md) — scoping of
  a possible future names module, which would be a sibling project rather
  than an extension of this one.
