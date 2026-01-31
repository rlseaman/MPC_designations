# Production Readiness Notes

## Session: 2026-01-31 - Tcl Production Preparation

### Issues Discovered and Fixed

1. **Example code using wrong API** - `example_usage.tcl` accessed dict keys incorrectly (`dict get $result format` instead of `dict get $result info format`). Examples should be tested as part of CI.

2. **Library/CLI coupling** - The library file exposed global procs (`main`, `printUsage`, `formatVerbose`) that could collide with user code. Production users need clean namespace isolation.

3. **Missing production use cases** - Examples didn't show handling of mixed valid/invalid input (common in batch processing where some identifiers are temporary/internal, not MPC format).

### Changes Made (Tcl)

- Split `mpc_designation.tcl` into:
  - `mpc_designation.tcl` - Pure library, only `MPCDesignation` namespace exposed
  - `mpc_designation_cli.tcl` - CLI wrapper for command-line use
- Updated library header with API documentation
- Added Example 8: Unpack mixed input (graceful handling of invalid identifiers)
- Added Example 9: Pack unpacked designations
- Updated all scripts that invoked CLI (`chain_test.py`, `validate_consistency.sh`)

---

## Checklist for Other Language Implementations

Apply these improvements to each language implementation:

### 1. Library/CLI Separation
Review each implementation for namespace pollution:

| Language | Library File | CLI File | Status |
|----------|-------------|----------|--------|
| Tcl | `mpc_designation.tcl` | `mpc_designation_cli.tcl` | Done |
| Python | `mpc_designation.py` | `mpc_designation_cli.py`? | Check |
| Ruby | `mpc_designation.rb` | `mpc_designation_cli.rb` | Check |
| JavaScript | `mpc_designation.js` | `mpc_designation_cli.js` | Check |
| TypeScript | `mpc_designation.ts` | `mpc_designation_cli.ts` | Check |
| Go | module | `main.go` | Likely OK (Go modules) |
| Rust | `lib.rs` | `main.rs` | Likely OK (Cargo structure) |
| Java | `MPCDesignation.java` | `MPCDesignationCLI.java` | Check |
| Kotlin | `MPCDesignation.kt` | `Main.kt` | Check |
| C# | `MPCDesignation.cs` | `Program.cs` | Check |
| C | `mpc_designation.c/.h` | CLI in same? | Check |
| C++ | `mpc_designation.cpp/.hpp` | CLI in same? | Check |
| Swift | `MPCDesignation.swift` | CLI in same? | Check |
| Nim | `mpc_designation.nim` | CLI in same? | Check |
| Haskell | `MPCDesignation.hs` | `Main.hs` | Check |
| Julia | `MPCDesignation.jl` | `mpc_designation_cli.jl` | Check |
| Perl | `MPC/Designation.pm` | CLI via `-e`? | Check |
| PHP | `MPCDesignation.php` | `mpc_designation_cli.php` | Check |
| AWK | `mpc_designation.awk` | `mpc_designation_main.awk` | Check |
| Fortran | module | CLI | Check |

**Questions to ask for each:**
- Does importing/requiring the library expose any unwanted globals?
- Is there a clean module/namespace/package boundary?
- Can users import just the conversion functions without CLI overhead?

### 2. Production Examples
Each language should have examples demonstrating:

- [ ] **Batch processing with error handling** - Process list of identifiers, some invalid, without crashing
- [ ] **Pack operation** - Human-readable to MPC packed format
- [ ] **Unpack operation** - MPC packed to human-readable format
- [ ] **Validation without exception** - Check if identifier is valid MPC format
- [ ] **Round-trip verification** - pack(unpack(x)) == x

### 3. API Consistency
Ensure each implementation exports these core functions (with language-appropriate naming):

| Function | Purpose |
|----------|---------|
| `pack(designation)` | Ensure packed format (convert if needed) |
| `unpack(designation)` | Ensure unpacked format (convert if needed) |
| `convert(designation)` | Auto-detect and convert, return with metadata |
| `convertSimple(designation)` | Auto-detect and convert, return string only |
| `isValid(designation)` | Check validity without throwing |
| `detectFormat(designation)` | Return format info without converting |

### 4. Documentation Updates
Each implementation should have:

- [ ] Header comment explaining library vs CLI usage
- [ ] API reference (what functions are public)
- [ ] Integration example (how to add to existing project)
- [ ] Error handling guidance

### 5. Test Coverage
Verify each implementation:

- [ ] Runs against full 2M+ test suite
- [ ] Examples execute without error
- [ ] CLI works standalone
- [ ] Library imports cleanly

---

## Enabling External Project Teams

### Recommended Integration Patterns

**For compiled languages (C, C++, Rust, Go, Nim):**
- Provide header files or module definitions
- Consider static library builds
- Document linking requirements

**For JVM languages (Java, Kotlin):**
- Publish JAR to Maven Central or provide build instructions
- Ensure clean package structure (`mpc.MPCDesignation`)

**For scripting languages (Python, Ruby, Perl, PHP, Tcl, Julia):**
- Single-file library that can be copied or sourced
- Consider package manager distribution (PyPI, RubyGems, CPAN, etc.)

**For JavaScript/TypeScript:**
- Consider npm package
- Provide both CommonJS and ES module exports

### Distribution Checklist

- [ ] Each implementation has standalone library file(s)
- [ ] Clear installation/integration instructions in per-language README
- [ ] License file present and compatible with common project licenses
- [ ] Version number accessible programmatically (`MPCDesignation.version`)
- [ ] Minimal dependencies (ideally zero for core functionality)

### Testing Integration

External teams should be able to:
1. Copy library file(s) to their project
2. Import/source/require the library
3. Call `pack()` or `unpack()` immediately
4. Handle errors gracefully in their own way

---

## Priority Order for Improvements

Based on likely usage and current state:

1. **Python** - Most likely to be used, check namespace isolation
2. **JavaScript/TypeScript** - Web applications, npm packaging
3. **Java/Kotlin** - Enterprise systems, JAR packaging
4. **C/C++** - Embedded or performance-critical, header files
5. **Go/Rust** - Modern systems, already have good module systems
6. **Ruby/Perl/PHP** - Legacy systems, single-file distribution
7. **Others** - As needed

---

## Notes for Future Sessions

- Run `tclsh examples/example_usage.tcl` (or equivalent) as smoke test before declaring implementation ready
- The chain test (`scripts/chain_test.py`) is useful for verifying all implementations work together
- Consider adding a `make production-check` target that verifies library/CLI separation for all languages
- The `--fast` flag in chain_test.py skips JVM/CLR languages which have high subprocess overhead
