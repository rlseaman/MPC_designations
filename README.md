# MPC Designations

Convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

Based on the MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html

## Implementations

| Language | Directory | Status |
|----------|-----------|--------|
| **AWK** | [`awk/`](awk/) | Production |
| **Bash** | [`bash/`](bash/) | Production |
| **C** | [`c/`](c/) | Production |
| **C++** | [`cpp/`](cpp/) | Production |
| **C#** | [`csharp/`](csharp/) | Production |
| **Forth** | [`forth/`](forth/) | Minimal |
| **Fortran** | [`fortran/`](fortran/) | Production |
| **Go** | [`go/`](go/) | Production |
| **Haskell** | [`haskell/`](haskell/) | Production |
| **Java** | [`java/`](java/) | Production |
| **JavaScript** | [`js/`](js/) | Production |
| **Julia** | [`julia/`](julia/) | Production |
| **Kotlin** | [`kotlin/`](kotlin/) | Production |
| **Nim** | [`nim/`](nim/) | Production |
| **Octave/MATLAB** | [`octave/`](octave/) | Production |
| **Perl** | [`perl/`](perl/) | Production |
| **PHP** | [`php/`](php/) | Production |
| **Python** | [`python/`](python/) | Production |
| **R** | [`r/`](r/) | Production |
| **Ruby** | [`ruby/`](ruby/) | Production |
| **Rust** | [`rust/`](rust/) | Production |
| **Swift** | [`swift/`](swift/) | Production |
| **Tcl** | [`tcl/`](tcl/) | Production |
| **TypeScript** | [`typescript/`](typescript/) | Production |

All production implementations pass the same test suite and produce identical results.

**Note:** The Forth implementation is minimal, supporting only numbered asteroids and basic provisional designations. See [forth/README.md](forth/README.md) for details.

## Test Results

All 22 production implementations achieve 100% accuracy on the full test suite (2,021,090 conversions):

| Implementation | Pack (unpacked→packed) | Status |
|----------------|------------------------|--------|
| AWK | 2,021,090 / 2,021,090 | ✅ 100% |
| C | 2,021,090 / 2,021,090 | ✅ 100% |
| C++ | 2,021,090 / 2,021,090 | ✅ 100% |
| C# | 2,021,090 / 2,021,090 | ✅ 100% |
| Fortran | 2,021,090 / 2,021,090 | ✅ 100% |
| Go | 2,021,090 / 2,021,090 | ✅ 100% |
| Haskell | 2,021,090 / 2,021,090 | ✅ 100% |
| Java | 2,021,090 / 2,021,090 | ✅ 100% |
| JavaScript | 2,021,090 / 2,021,090 | ✅ 100% |
| Julia | 2,021,090 / 2,021,090 | ✅ 100% |
| Kotlin | 2,021,090 / 2,021,090 | ✅ 100% |
| Nim | 2,021,090 / 2,021,090 | ✅ 100% |
| Octave | 2,021,090 / 2,021,090 | ✅ 100% |
| Perl | 2,021,090 / 2,021,090 | ✅ 100% |
| PHP | 2,021,090 / 2,021,090 | ✅ 100% |
| Python | 2,021,090 / 2,021,090 | ✅ 100% |
| R | 2,021,090 / 2,021,090 | ✅ 100% |
| Ruby | 2,021,090 / 2,021,090 | ✅ 100% |
| Rust | 2,021,090 / 2,021,090 | ✅ 100% |
| Swift | 2,021,090 / 2,021,090 | ✅ 100% |
| Tcl | 2,021,090 / 2,021,090 | ✅ 100% |
| TypeScript | 2,021,090 / 2,021,090 | ✅ 100% |

**Roundtrip note:** 2,625 old-style designations (e.g., `A873 OA`) normalize to modern format (`1873 OA`) on unpack. This is correct behavior—the packed representation is identical either way.

## Quick Start

### AWK
```bash
cd awk
echo "1995 XA" | awk -f src/mpc_designation.awk -f src/mpc_designation_main.awk
# Output: J95X00A
```

### Bash
```bash
cd bash
./src/mpc_designation.sh '1995 XA'    # Output: J95X00A
```

```bash
source src/mpc_designation.sh
result=$(convert_simple "1995 XA")    # Returns "J95X00A"
```

### C
```bash
cd c && make
./mpc_designation '1995 XA'    # Output: J95X00A
```

### C++
```bash
cd cpp && make
./mpc_designation '1995 XA'    # Output: J95X00A
```

```cpp
#include "mpc_designation.hpp"
std::string result = mpc::MPCDesignation::convertSimple("1995 XA");  // Returns "J95X00A"
```

### C#
```bash
cd csharp && dotnet build
dotnet run -- '1995 XA'    # Output: J95X00A
```

```csharp
using MPC;
string result = MPCDesignation.ConvertSimple("1995 XA");  // Returns "J95X00A"
```

### Forth
```bash
cd forth
gforth src/mpc_designation_cli.fs '1995 XA'    # Output: J95X00A
```

```forth
require src/mpc_designation.fs
s" 1995 XA" convert-simple type  \ Prints: J95X00A
```

### Python
```python
from mpc_designation import convert_simple
convert_simple('1995 XA')  # Returns 'J95X00A'
```

### R
```r
source("mpc_designation.R")
convert_simple("1995 XA")  # Returns "J95X00A"
```

```bash
cd r
Rscript src/mpc_designation_cli.R '1995 XA'    # Output: J95X00A
```

### Tcl
```tcl
source mpc_designation.tcl
MPCDesignation::convertSimple "1995 XA"  ;# Returns "J95X00A"
```

### Swift
```bash
cd swift && make
./mpc_designation '1995 XA'    # Output: J95X00A
```

### Perl
```perl
use MPC::Designation qw(convert_simple);
convert_simple('1995 XA');  # Returns 'J95X00A'
```

### PHP
```php
require_once 'src/MPCDesignation.php';
use MPC\MPCDesignation;
MPCDesignation::convertSimple('1995 XA');  // Returns 'J95X00A'
```

```bash
cd php
php src/mpc_designation_cli.php '1995 XA'    # Output: J95X00A
```

### Fortran
```bash
cd fortran && make
./build/mpc_designation_cli '1995 XA'    # Output: J95X00A
```

```fortran
use mpc_designation
character(len=80) :: result
result = convert_simple('1995 XA')  ! Returns 'J95X00A'
```

### Go
```bash
cd go && make
./mpc_designation '1995 XA'    # Output: J95X00A
```

```go
import "github.com/rlseaman/mpc_designations/go/mpc"
result, _ := mpc.ConvertSimple("1995 XA")  // Returns "J95X00A"
```

### Haskell
```bash
cd haskell && make
./build/mpc_designation '1995 XA'    # Output: J95X00A
```

```haskell
import MPCDesignation
case convertSimple "1995 XA" of
    Right result -> putStrLn result  -- "J95X00A"
    Left err -> print err
```

### Rust
```bash
cd rust && cargo build --release
./target/release/mpc_designation '1995 XA'    # Output: J95X00A
```

```rust
use mpc_designation::convert_simple;
let result = convert_simple("1995 XA").unwrap();  // Returns "J95X00A"
```

### Java
```bash
cd java && make
java -cp classes mpc.MPCDesignationCLI '1995 XA'    # Output: J95X00A
```

```java
import mpc.MPCDesignation;
String result = MPCDesignation.convertSimple("1995 XA");  // Returns "J95X00A"
```

### Julia
```bash
cd julia
julia src/mpc_designation_cli.jl '1995 XA'    # Output: J95X00A
```

```julia
push!(LOAD_PATH, "src")
using MPCDesignation
result = convert_simple("1995 XA")  # Returns "J95X00A"
```

### Kotlin
```bash
cd kotlin && make
kotlin -cp build/mpc_designation.jar mpc.MainKt '1995 XA'    # Output: J95X00A
```

```kotlin
import mpc.MPCDesignation
val result = MPCDesignation.convertSimple("1995 XA")  // Returns "J95X00A"
```

### Nim
```bash
cd nim && make
./mpc_designation '1995 XA'    # Output: J95X00A
```

```nim
import mpc_designation
let result = convertSimple("1995 XA")  # Returns "J95X00A"
```

### Octave/MATLAB
```matlab
source('src/mpc_designation.m');
mpc_convert_simple('1995 XA')  % Returns 'J95X00A'
```

```bash
cd octave
octave --no-gui src/mpc_designation_cli.m '1995 XA'    # Output: J95X00A
```

### Ruby
```ruby
require_relative 'src/mpc_designation'
MPCDesignation.convert_simple('1995 XA')  # Returns 'J95X00A'
```

```bash
cd ruby
ruby src/mpc_designation_cli.rb '1995 XA'    # Output: J95X00A
```

### JavaScript (Node.js)
```javascript
const { convertSimple } = require('./src/mpc_designation');
convertSimple('1995 XA');  // Returns 'J95X00A'
```

```bash
cd js
node src/mpc_designation_cli.js '1995 XA'    # Output: J95X00A
```

### TypeScript
```typescript
import { convertSimple } from 'mpc-designation-ts';
const result: string = convertSimple('1995 XA');  // Returns 'J95X00A'
```

```bash
cd typescript && npm install && npm run build
node dist/src/mpc_designation_cli.js '1995 XA'    # Output: J95X00A
```

## Examples

```bash
# Asteroids - permanent (numbered)
1             -> 00001
100001        -> A0001
620000        -> ~0000

# Asteroids - provisional
1995 XA       -> J95X00A
2024 AB631    -> _OA004S

# Asteroids - survey
2040 P-L      -> PLS2040

# Comets
1P            -> 0001P
C/1995 O1     -> CJ95O010
D/1993 F2-B   -> DJ93F02b

# Natural satellites
S/2019 S 22   -> SK19S220
```

## Project Structure

```
MPC_designations/
├── README.md           # This file
├── VERSION             # Version number (unified across languages)
├── LICENSE             # Public domain
├── CONTRIBUTING.md     # How to add new languages
├── docs/
│   ├── SPECIFICATION.md    # MPC format reference
│   ├── FORMATS.md          # Quick reference tables
│   └── ERROR_CHECKING.md   # Validation documentation
├── test-data/
│   ├── prov_unpack_to_pack.csv.gz  # 2M+ test cases
│   └── error_test_cases.csv         # Error handling tests
├── awk/
│   ├── README.md       # AWK documentation
│   ├── Makefile
│   ├── src/            # Source code
│   └── test/           # Test files
├── bash/
│   ├── README.md       # Bash documentation
│   ├── Makefile
│   ├── src/            # Source code
│   └── test/           # Test files
├── c/
│   ├── README.md       # C documentation
│   ├── Makefile
│   ├── src/            # Source code
│   ├── test/           # Test files
│   └── examples/       # Usage examples
├── cpp/
│   ├── README.md       # C++ documentation
│   ├── Makefile
│   └── src/            # Source code
├── csharp/
│   ├── README.md       # C# documentation
│   ├── Makefile
│   └── src/            # Source code
├── forth/
│   ├── README.md       # Forth documentation
│   ├── Makefile
│   ├── src/            # Source code (gforth)
│   └── test/           # Test files
├── fortran/
│   ├── README.md       # Fortran documentation
│   ├── Makefile
│   ├── src/            # Source code
│   └── test/           # Test files
├── python/
│   ├── README.md       # Python documentation
│   ├── pyproject.toml
│   ├── src/            # Source code
│   ├── test/           # Test files
│   └── examples/       # Usage examples
├── r/
│   ├── README.md       # R documentation
│   ├── Makefile
│   ├── src/            # Source code
│   ├── test/           # Test files
│   └── examples/       # Usage examples
├── tcl/
│   ├── README.md       # Tcl documentation
│   ├── src/            # Source code
│   ├── test/           # Test files
│   └── examples/       # Usage examples
├── swift/
│   ├── Makefile
│   ├── src/            # Source code
│   └── test/           # Test files
├── perl/
│   ├── src/            # Source code (MPC/Designation.pm)
│   └── test/           # Test files
├── php/
│   ├── Makefile
│   └── src/            # Source code
├── go/
│   ├── go.mod          # Go module definition
│   ├── Makefile
│   ├── mpc/            # Library package
│   ├── cmd/            # CLI
│   └── test/           # Test runners
├── haskell/
│   ├── README.md       # Haskell documentation
│   ├── Makefile
│   ├── src/            # Source code (MPCDesignation module)
│   └── test/           # Test files
├── java/
│   ├── Makefile
│   ├── src/            # Source code (mpc package)
│   └── test/           # Test files
├── julia/
│   ├── Makefile
│   ├── src/            # Source code (MPCDesignation module)
│   └── test/           # Test files
├── kotlin/
│   ├── Makefile
│   ├── src/            # Source code (mpc package)
│   └── test/           # Test files
├── octave/
│   ├── README.md       # Octave/MATLAB documentation
│   ├── Makefile
│   ├── src/            # Source code
│   └── test/           # Test files
├── rust/
│   ├── Cargo.toml      # Rust package definition
│   ├── Makefile
│   └── src/            # Source code (lib.rs + binaries)
├── ruby/
│   ├── Makefile
│   └── src/            # Source code
├── js/
│   ├── package.json    # npm package definition
│   ├── Makefile
│   ├── src/            # Source code
│   └── test/           # Test files
└── typescript/
    ├── package.json    # npm package definition
    ├── tsconfig.json   # TypeScript configuration
    ├── Makefile
    ├── src/            # Source code
    └── test/           # Test files
```

## Testing

Each implementation includes tests against 2+ million known-good conversions and 94 error handling tests.

```bash
# Run all tests for all languages
make test-all

# Run tests for a specific language
make test-awk
make test-bash
make test-c
make test-cpp
make test-csharp
make test-forth
make test-fortran
make test-go
make test-haskell
make test-java
make test-julia
make test-js
make test-kotlin
make test-nim
make test-octave
make test-perl
make test-php
make test-python
make test-r
make test-ruby
make test-rust
make test-swift
make test-tcl
make test-typescript

# Quick error tests only (faster)
make test-errors

# Benchmark all implementations
./scripts/benchmark.sh
```

## Sparse Checkout

Download only the language you need:

```bash
git clone --filter=blob:none --sparse https://github.com/rlseaman/MPC_designations.git
cd MPC_designations
git sparse-checkout set python test-data docs
```

## Supported Formats

### Asteroids
| Type | Unpacked | Packed |
|------|----------|--------|
| Permanent (< 100K) | 1 - 99999 | 00001 - 99999 |
| Permanent (100K-620K) | 100001 - 619999 | A0001 - z9999 |
| Permanent (620K+) | 620000+ | ~0000+ |
| Provisional | 1995 XA | J95X00A |
| Survey | 2040 P-L | PLS2040 |

### Comets
| Type | Unpacked | Packed |
|------|----------|--------|
| Numbered | 1P | 0001P |
| Provisional | C/1995 O1 | CJ95O010 |
| Fragment | D/1993 F2-B | DJ93F02b |
| Ancient | C/240 V1 | C240V010 |
| BCE | C/-146 P1 | C.53P010 |

### Natural Satellites
| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |

## Performance

Benchmark results on Apple M1 Max, processing 2,021,090 designation conversions.

### Pack Direction (unpacked → packed)

| Rank | Language | Time (ms) | Rate (entries/sec) | Relative |
|-----:|----------|----------:|-------------------:|---------:|
| 1 | Go | 730 | 2,768,283 | 1.00x |
| 2 | Nim | 1,059 | 1,908,404 | 0.69x |
| 3 | C | 1,065 | 1,898,352 | 0.69x |
| 4 | TypeScript | 1,419 | 1,424,306 | 0.51x |
| 5 | JavaScript | 1,439 | 1,404,510 | 0.51x |
| 6 | Rust | 1,543 | 1,309,845 | 0.47x |
| 7 | Julia | 1,965 | 1,028,389 | 0.37x |
| 8 | C# | 2,672 | 756,396 | 0.27x |
| 9 | Fortran | 3,010 | 671,459 | 0.24x |
| 10 | Kotlin | 3,711 | 544,621 | 0.20x |
| 11 | PHP | 4,318 | 468,025 | 0.17x |
| 12 | Java | 7,559 | 267,375 | 0.10x |
| 13 | Haskell | 7,567 | 267,108 | 0.10x |
| 14 | AWK | 9,561 | 211,389 | 0.08x |
| 15 | Python | 10,949 | 184,592 | 0.07x |
| 16 | Swift | 11,444 | 176,605 | 0.06x |
| 17 | C++ | 15,488 | 130,494 | 0.05x |
| 18 | Perl | 17,178 | 117,654 | 0.04x |
| 19 | Ruby | 23,765 | 85,045 | 0.03x |
| 20 | Tcl | 44,384 | 45,537 | 0.02x |
| 21 | R | 603,586 | 3,348 | <0.01x |
| 22 | Octave | 768,240 | 2,631 | <0.01x |
| 23 | Bash | ~6,000,000 | ~340 | <0.01x |

*Bash time extrapolated from sample (full run would take ~100 minutes)

### Unpack Direction (packed → unpacked)

| Rank | Language | Time (ms) | Rate (entries/sec) | Relative |
|-----:|----------|----------:|-------------------:|---------:|
| 1 | Nim | 261 | 7,743,640 | 1.00x |
| 2 | C | 283 | 7,141,661 | 0.92x |
| 3 | C# | 296 | 6,828,007 | 0.88x |
| 4 | JavaScript | 450 | 4,491,311 | 0.58x |
| 5 | C++ | 455 | 4,441,956 | 0.57x |
| 6 | Go | 465 | 4,340,646 | 0.56x |
| 7 | TypeScript | 577 | 3,502,756 | 0.45x |
| 8 | Rust | 790 | 2,558,342 | 0.33x |
| 9 | Haskell | 1,180 | 1,713,433 | 0.22x |
| 10 | Kotlin | 1,341 | 1,507,151 | 0.19x |
| 11 | Fortran | 1,909 | 1,058,717 | 0.14x |
| 12 | Java | 2,356 | 857,848 | 0.11x |
| 13 | Julia | 2,658 | 760,351 | 0.10x |
| 14 | Swift | 4,182 | 483,283 | 0.06x |
| 15 | PHP | 4,370 | 462,449 | 0.06x |
| 16 | Python | 4,972 | 406,526 | 0.05x |
| 17 | AWK | 9,254 | 218,402 | 0.03x |
| 18 | Ruby | 11,896 | 169,897 | 0.02x |
| 19 | Perl | 12,601 | 160,391 | 0.02x |
| 20 | Tcl | 27,477 | 73,556 | 0.01x |
| 21 | Bash | ~7,200,000 | ~280 | <0.01x |

*Bash time extrapolated from sample (full run would take ~120 minutes)

### Round-trip Verification

All production implementations pass the packed round-trip test: `pack(unpack(y)) = y`.

The 2,625 old-style provisional designations (e.g., `A873 OA` for year 1873) normalize to modern format (`1873 OA`) on unpack. This is expected behavior—both formats pack to the same representation (`I73O00A`).

Run benchmarks with: `./scripts/benchmark.sh --roundtrip`

## Documentation

- [Specification](docs/SPECIFICATION.md) - MPC format specification reference
- [Format Tables](docs/FORMATS.md) - Quick reference for all formats
- [Error Checking](docs/ERROR_CHECKING.md) - Validation documentation
- [Multi-Platform](docs/MULTIPLATFORM.md) - Platform support and compatibility

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on adding new language implementations.

## License

CC0 1.0 Universal - Public Domain Dedication. See [LICENSE](LICENSE).
