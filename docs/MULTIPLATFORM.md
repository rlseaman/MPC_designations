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

**Potential issue:** The Makefile uses `cc` which is fine for Unix-like systems but Windows needs adjustments (CMake or batch files).

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

---

## Recommended Changes for Multi-Platform Support

### 1. Build System Improvements

**Add CMake as alternative to Make:**

```
c/
├── Makefile          # Keep for Unix simplicity
├── CMakeLists.txt    # Add for Windows/IDE support
```

CMake provides:
- Native Windows support (generates Visual Studio projects)
- Cross-compilation support
- Better dependency detection

**Add Windows batch file or PowerShell script:**

```
c/build.bat           # Simple: cl /O2 src/*.c /Fe:mpc_designation.exe
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

| Language | Windows | Linux | macOS | RPi | Legacy RHEL/CentOS | Notes |
|----------|---------|-------|-------|-----|-------------------|-------|
| **C** | Yes | Yes | Yes | Yes | Yes | Best portability - reference implementation |
| **Python** | Yes | Yes | Yes | Yes | Partial (version) | Most accessible - lower version floor recommended |
| **Tcl** | Partial | Yes | Yes | Yes | Yes | Good for embedded/scripting |
| **JavaScript/Node** | Yes | Yes | Yes | Partial | Partial | Would enable web tools |
| **Rust** | Yes | Yes | Yes | Partial | Limited | Modern alternative to C, good WASM support |
| **Go** | Yes | Yes | Yes | Partial | Partial | Single binary distribution |
| **Java** | Yes | Yes | Yes | Partial | Yes | Excellent legacy support |
| **Perl** | Partial | Yes | Yes | Yes | Yes | Ubiquitous on Unix systems |

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

1. **Lower Python to 3.6** for RHEL 8/CentOS compatibility
2. **Add CMake** for Windows build support
3. **Expand CI** to test macOS and Windows
4. **Add Docker image** for reproducible deployment

---

## Document History

- 2025-01-29: Initial analysis
