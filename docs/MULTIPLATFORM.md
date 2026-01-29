# Multi-Platform Support Analysis

This document analyzes cross-platform compatibility for the MPC Designations project and outlines recommendations for broader deployment.

## Target Platforms

### Primary Development
- macOS (Apple Silicon M1)

### Primary Deployment
- RHEL 8.6
- Rocky 9.7
- CentOS (legacy)
- Raspberry Pi 5 (latest RPi OS)

### Community Deployment
- Windows (various versions, old and new)
- Diverse Linux distributions
- Other platforms used by the planetary defense community

---

## Current Legacy System Support

### C Implementation

**Status: Excellent portability**

- Uses only C99 standard library (`string.h`, `stdio.h`, `ctype.h`, `stdlib.h`)
- No external dependencies
- Compiles with any POSIX-compliant compiler (gcc, clang)
- Will work on RHEL 8.6+ (gcc 8.5), Rocky 9 (gcc 11), CentOS 7+ (gcc 4.8.5), RPi OS, and Windows (MSVC, MinGW, Cygwin)
- **CMake support added** for cross-platform builds including Windows/Visual Studio

### Python Implementation

**Status: Good, but version floor may be aggressive**

- Currently requires Python 3.9+
- RHEL 8.6 ships Python 3.6 by default (3.8/3.9 available via AppStream)
- CentOS 7 ships Python 2.7/3.6
- Rocky 9 ships Python 3.9+

**Recommendation:** Consider lowering to Python 3.6 or 3.7 for broader legacy support. The code uses no features requiring 3.9+:
- No `match` statements (3.10+)
- No walrus operator usage that couldn't be refactored
- No type union syntax `X | Y` (3.10+)

### Tcl Implementation

**Status: Excellent**

- Requires only Tcl 8.5+ (released 2007)
- Available on virtually all target platforms
- RHEL/Rocky ship Tcl 8.5 or 8.6

### Swift Implementation

**Status: Good (platform-limited)**

- macOS: Excellent support (native toolchain)
- Linux: Supported via Swift.org toolchain (Ubuntu, CentOS, Amazon Linux)
- Windows: Experimental support as of Swift 5.9
- Uses Foundation framework for regex; could be optimized for pure Swift
- **Package.swift** could be added for Swift Package Manager support

### Perl Implementation

**Status: Excellent**

- Perl 5 is ubiquitous on Unix-like systems
- Available on Windows via Strawberry Perl or ActivePerl
- RHEL/Rocky/CentOS ship Perl 5.x by default
- No external CPAN dependencies required

---

## Recommended Changes for Multi-Platform Support

### 1. Build System Improvements

**CMake added for C implementation:**

```
c/
├── Makefile          # Unix simplicity
├── CMakeLists.txt    # Windows/IDE support (ADDED)
```

CMake provides:
- Native Windows support (generates Visual Studio projects)
- Cross-compilation support
- CTest integration for automated testing
- Static library target for embedding

Build with CMake:
```bash
mkdir build && cd build
cmake ..
cmake --build .
ctest  # Run tests
```

### 2. Python Packaging Enhancements

**Lower version requirement** in `pyproject.toml`:

```toml
requires-python = ">=3.6"
```

**Add wheel building** for easier installation on systems without compilers:

```bash
python -m build --wheel
```

**Consider conda-forge package** for scientific computing environments (common in astronomy).

### 3. CI/CD Expansion

Current CI only tests Ubuntu. Add matrix testing:

```yaml
strategy:
  matrix:
    os: [ubuntu-latest, macos-latest, windows-latest]
    python-version: ['3.6', '3.7', '3.8', '3.9', '3.10', '3.11', '3.12']
```

### 4. Documentation

Platform-specific installation instructions for:
- Linux (apt, yum/dnf, from source)
- macOS (from source)
- Windows (from source with MSVC/MinGW)
- Raspberry Pi

---

## Other Platforms to Consider

| Platform | Priority | Notes |
|----------|----------|-------|
| **FreeBSD/OpenBSD** | Medium | Common in academic/research settings; should work as-is |
| **Solaris/illumos** | Low | Some observatories use Solaris; C code portable |
| **WSL/WSL2** | High | Many Windows astronomers use WSL; test explicitly |
| **Docker** | High | Provide official images for reproducible deployment |
| **WebAssembly** | Medium | Could enable browser-based tools; C compiles to WASM |
| **Android/Termux** | Low | Mobile field use; Python/C work in Termux |

---

## Language Support Across Platforms

### Implemented Languages

| Language | Windows | Linux | macOS | RPi | Legacy RHEL/CentOS | Build Tool | Notes |
|----------|---------|-------|-------|-----|-------------------|------------|-------|
| **C** | Yes | Yes | Yes | Yes | Yes | Make, CMake | Reference implementation |
| **Python** | Yes | Yes | Yes | Yes | Partial (version) | pip | Most accessible |
| **Tcl** | Partial | Yes | Yes | Yes | Yes | None needed | Good for scripting |
| **Swift** | Experimental | Yes | Yes | No | No | Make | macOS primary |
| **Perl** | Yes | Yes | Yes | Yes | Yes | None needed | Ubiquitous on Unix |

### Potential Future Languages

| Language | Windows | Linux | macOS | RPi | Legacy RHEL/CentOS | Build Tool | Notes |
|----------|---------|-------|-------|-----|-------------------|------------|-------|
| **JavaScript/Node** | Yes | Yes | Yes | Partial | Partial | npm | Would enable web tools |
| **Rust** | Yes | Yes | Yes | Partial | Limited | Cargo | Modern alternative to C |
| **Go** | Yes | Yes | Yes | Partial | Partial | go mod | Single binary distribution |
| **Java** | Yes | Yes | Yes | Partial | Yes | Maven/Gradle | Excellent legacy support |
| **Ruby** | Yes | Yes | Yes | Yes | Yes | Gem | Scripting alternative |
| **Lua** | Yes | Yes | Yes | Yes | Yes | None needed | Embedded scripting |

---

## Additional Recommendations

### 1. Package Manager Integration

- **PyPI**: Already possible with current setup
- **Homebrew**: Create formula for macOS/Linux
- **AUR**: Arch Linux user repository
- **COPR/EPEL**: For RHEL/CentOS/Rocky users

### 2. Static Linking Option for C

For maximum portability on Linux:

```makefile
static:
    $(CC) -static -O2 $(CFLAGS) -o mpc_designation_static ...
```

### 3. Endianness

The code is endianness-agnostic (no binary data structures), which is good for ARM, POWER, and other architectures.

### 4. Character Encoding

The code assumes ASCII-compatible input, which is correct for MPC designations. This assumption should be documented explicitly.

### 5. Lite Distribution

The lite modules are ideal for embedding:
- Single-header C distribution (`mpc_designation_lite.h`)
- Single-file Python (`lite.py`)

These should be emphasized in documentation for easy integration into other projects.

---

## Implementation Priority

1. ~~**Add CMake** for Windows build support~~ ✅ Done
2. **Lower Python to 3.6** for RHEL 8/CentOS compatibility
3. **Add Swift Package Manager** (Package.swift) for cross-platform Swift
4. **Expand CI** to test macOS and Windows
5. **Add Docker image** for reproducible deployment

---

## Document History

- 2025-01-29: Initial analysis
