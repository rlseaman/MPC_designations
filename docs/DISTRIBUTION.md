# Per-Language Distribution: Status and Playbook

Notes for future work on publishing the 25 language implementations to their
respective package registries. This document is descriptive and
work-queue-shaped, not prescriptive — priorities should be revisited each
time you pick up this chore.

Current tracking version across packaging: **1.0.1**. Any fresh manifest work
should target this version to stay in sync.

## What's already published

| Ecosystem | Package identifier | Install command | Notes |
|---|---|---|---|
| PyPI | `mpc-designation` | `pip install mpc-designation` | v1.0.1 live as of 2026-04-17 |
| conda-forge | `mpc-designation` | `conda install -c conda-forge mpc-designation` | PR [#33021](https://github.com/conda-forge/staged-recipes/pull/33021) in review; auto-bumps via `regro-cf-autotick-bot` after merge |
| Go module proxy | `github.com/rlseaman/mpc_designations/go` | `go get github.com/rlseaman/mpc_designations/go@v1.0.1` | Path-prefixed tags `go/v1.0.0`, `go/v1.0.1` pushed; proxy indexes lazily |

## Priority order

For astronomy audience reach specifically, in rough order:

1. **Done** — Python (PyPI + conda-forge pending), Go.
2. **Worth doing when convenient** — Rust (manifest ready, low ceremony; real
   fix required first), Julia (real astronomy community), Homebrew for the C
   binary (macOS astronomers), npm (JS+TS bundle, useful if anyone writes an
   MPC web tool).
3. **Optional, only if a user asks** — R (specialized astrostatistics),
   everything in Group C below.

The "polyglot as reference corpus" framing (see `docs/DESIGN.md`) means the
marginal reader of each per-language implementation is small. Publishing
every single language to its registry brings limited absolute adoption but
costs real maintenance per channel. The per-channel calculus should weigh
actual user demand before scaffolding a manifest that doesn't exist yet.

## Cross-cutting concerns

- **Version sync.** The top-level `VERSION` file is the source of truth. Keep
  every per-language manifest's `version` field equal to it. A release that
  ships "1.0.1 on PyPI, 1.0.0 on crates.io" is a mess to support.
- **Author / maintainer identity.** All packaging should use
  `Rob Seaman <rseaman@arizona.edu>`. The stale `seaman@noao.edu` predates
  the NOIRLab/Arizona transition and should never appear in new manifests.
- **License string.** `CC0-1.0` everywhere; `license_family: PUBLIC-DOMAIN`
  where the registry distinguishes family.
- **Package name conventions.** Registries differ on hyphens vs. underscores.
  Where possible, the user-visible package name should match PyPI's
  `mpc-designation`. Language-level import names may use underscores where
  that's the ecosystem norm (Python, Rust).
- **"Tests as binaries" pattern.** Several languages (currently Rust, Go, C,
  etc.) have per-language test runners that are built as executable binaries.
  Most registry-publishing flows will try to install these alongside the real
  CLI. Before publishing, check that only the user-facing `mpc-designation`
  CLI ends up on the consumer's PATH — see the Rust section below for the
  concrete instance of this.

## Group A — manifest present, ready to publish after small fixes

### Rust → crates.io

**Manifest:** `rust/Cargo.toml` exists with good bones (metadata, keywords,
categories, CC0 license, lib + bin targets). Not publish-ready as-is.

**Blocker:** Four test binaries (`test_csv`, `test_errors`, `test_roundtrip`,
`test_helpers`) are declared as `[[bin]]` entries. `cargo install
mpc_designation` would put all five binaries on the user's PATH, including
the test runners that expect CSV files not present at install time.

**Fix:** add a dev feature and gate the test binaries behind it:

```toml
[features]
dev = []

[[bin]]
name = "test_csv"
path = "src/bin/test_csv.rs"
required-features = ["dev"]
# (same pattern for test_errors, test_roundtrip, test_helpers)
```

Then update `rust/Makefile` targets to use `--features dev` where needed.

**Other smaller fixes before publish:**

- `version = "1.0.0"` → `"1.0.1"` to match PyPI.
- `authors = ["MPC Designations Contributors"]` → `["Rob Seaman <rseaman@arizona.edu>"]`.
- Missing `readme = "README.md"` — without it, crates.io won't render the README.
- Missing `homepage` — add the GitHub repo URL.
- Consider renaming the crate `mpc_designation` → `mpc-designation` to match
  PyPI (Rust convention is hyphens in crate names, underscores in import
  paths). Neither name is currently taken on crates.io.

**Publish workflow (once fixed):**

```bash
cargo login                    # get token from https://crates.io/me
cd rust
cargo publish --dry-run        # validate metadata, build, check
cargo publish                  # real upload; version is then permanent
```

No staging/test registry. No TestPyPI equivalent. First publish reserves the
name on crates.io.

### JavaScript → npm

**Manifest:** `js/package.json` exists.

**Likely fixes:** verify `name`, `version`, `main`, `bin`, `repository`,
`author`, `license`, `keywords` are all set. Version should be `1.0.1`.
Author should match arizona.edu email.

**Publish workflow:**

```bash
cd js
npm login                      # create/use npm account
npm publish                    # first publish reserves name
```

Scoped vs. unscoped: if the name `mpc-designation` is taken on npm (check
first), fall back to scoped `@rlseaman/mpc-designation`. Scoped packages
require `npm publish --access public` on first upload.

### TypeScript → npm (same registry as JS)

**Manifest:** `typescript/package.json` exists.

**Note:** TS and JS share npm; the two directories in this repo are parallel
ports, not bindings. Recommend publishing one of the two, not both, to avoid
confusing users. TS is probably the right choice given modern ecosystem
preferences — the TypeScript package can serve JS consumers too via compiled
`.js` output + `.d.ts` types.

**Publish workflow:** same `npm publish` as JS. TS needs a build step first
(`npm run build`) to generate the compiled output in `dist/`.

### Go → already done

See "What's already published" above. Future releases need two tag pushes:
`vX.Y.Z` (repo-wide) and `go/vX.Y.Z` (submodule).

## Group B — no manifest, meaningful astronomy audience

### Julia → General registry

**Status:** no `julia/Project.toml` at package level.

**Scaffolding needed:**

```toml
# julia/Project.toml
name = "MPCDesignations"       # PascalCase per Julia convention
uuid = "<generate-with-Pkg.generate>"
version = "1.0.1"
authors = ["Rob Seaman <rseaman@arizona.edu>"]

[compat]
julia = "1.6"
```

A `julia/test/runtests.jl` file is also expected (our existing
`julia/test/test_*.jl` files need a one-line entry point).

**Publish workflow:** Julia uses a federated registry. You comment
`@JuliaRegistrator register` on the commit that tags the release; a bot
opens a PR against `JuliaRegistries/General`. Review is usually automated
for well-formed packages with CI.

**Audience:** the JuliaAstro org (AstroLib, Astro.jl, SkyCoords.jl, etc.)
is the relevant community. Julia astronomy adoption is growing but still
smaller than Python.

### R → r-universe or CRAN

**Status:** no `r/DESCRIPTION` file.

**Scaffolding needed:** standard R package skeleton — `DESCRIPTION`,
`NAMESPACE`, move sources under `R/`, add `tests/testthat/`, optionally
`man/` docs. The existing `r/src/mpc_designation.R` works as a flat script
but R's package system needs the canonical layout.

**Publish options:**

1. **r-universe** (recommended, easier) — git-driven. Create a
   `~.r-universe.dev/rlseaman` repo with a `packages.json` pointing at this
   repo's `r/` subdirectory. Builds and hosts binaries automatically.
2. **CRAN** (slower, pickier) — manual review, volunteer reviewers,
   rejections common for small style issues. Weeks to months. Worthwhile
   only if you want CRAN's specific discoverability.

**Audience:** astrostatistics community, uses CRAN as primary. r-universe
hosts most of them already.

## Group C — no manifest, niche astronomy audience, worthwhile eventually

| Language | Registry | Scaffolding effort | Notes |
|---|---|---|---|
| Ruby | [RubyGems](https://rubygems.org) | Low — write `mpc_designation.gemspec`, `gem push` | No natural astronomy audience |
| PHP | [Packagist](https://packagist.org) | Low — write `composer.json`, submit URL | Packagist auto-updates on git tags |
| Perl | [CPAN](https://www.cpan.org) | Medium — PAUSE account, `Dist::Zilla`, `dzil release` | Astronomy Perl exists (Starlink) but small |
| Haskell | [Hackage](https://hackage.haskell.org) | Low — write `.cabal` file, `cabal upload` | Niche |
| Java | [Maven Central](https://central.sonatype.com) via OSSRH | High — Sonatype signup, GPG-signed artifacts, strict metadata | Or **JitPack** (zero setup, just tag the repo) for low-ceremony distribution |
| Kotlin | Maven Central | High — same ceremony as Java | Shares the JVM registry |
| Nim | [Nimble directory](https://nimble.directory) | Low — write `.nimble` file, `nimble publish` | Small community |

## Group D — no standard registry

For these, distribution happens through OS package managers or "copy the
source" patterns, not a language-level registry.

- **C** — Homebrew formula (`brew install mpc-designation`), vcpkg, Conan.
  **Recommended first target:** a Homebrew tap. Formula is one `.rb` file
  defining the C source download URL, the build command, and the test. Submit
  to `homebrew-core` for global availability, or maintain a personal tap for
  low-ceremony distribution.
- **C++** — same infrastructure as C; vcpkg and Conan are the two serious
  C++ options.
- **Fortran** — [fpm](https://fpm.fortran-lang.org) has a registry; small
  audience. Most Fortran-writing astronomers build from source.
- **AWK, Bash, Forth, SPP/IRAF, Octave, Swift, C#, Tcl** — no natural
  registry for the scientific context. These remain as "copy from GitHub"
  reference implementations. SPP/IRAF in particular is IRAF-environment-
  specific, which is its own distribution channel.

## Workflow patterns that recur

All three "first publish" workflows so far have shared structure:

1. **Create registry account** (PyPI, crates.io, conda-forge via
   staged-recipes, npm, etc.). Most are free, most now require 2FA.
2. **Generate a scoped API token** for publishing. Store in a per-ecosystem
   config file (`~/.pypirc`, `~/.cargo/credentials`, `~/.npmrc`, etc.), not
   in shell history. Each file should be `chmod 600`.
3. **First publish may require "Entire account" scope** because the package
   doesn't exist yet. Rotate to a project-scoped token after the first
   successful publish.
4. **Version strings are forever.** Every registry allows "yanking" (hiding a
   version) but not replacement. Validate artifacts before uploading.
5. **Auto-bumping from source** is possible once the first release is in
   place: conda-forge has `regro-cf-autotick-bot`, Packagist auto-indexes
   git tags, Julia Registrator listens for comment triggers. Worth wiring
   up per ecosystem after first publish.

## Things that are NOT this document's scope

- **New-language implementations.** See `CONTRIBUTING.md` and
  `CLAUDE_NOTES.md` for adding a 26th language.
- **The corpus-vs-product design stance.** See `docs/DESIGN.md`.
- **Feature additions.** Permanent-satellite format, named-body lookups,
  etc. — see `docs/SATELLITE_NAMING_ANALYSIS.md`.
- **CI expansion.** The current GitHub Actions workflow tests C, Python,
  and Tcl only. Expanding the matrix is a separate concern from publishing.

## Revisit cadence

Review this document when:

- A new release of the library is about to ship (ensure every published
  channel gets the bump in lockstep).
- A new language implementation lands (decide if/how to publish it).
- A user opens an issue asking for `foo install mpc-designation` in an
  ecosystem not yet supported — that's real demand and flips a Group C
  candidate into the priority queue.
