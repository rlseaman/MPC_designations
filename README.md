# MPC Designations

Convert between packed and unpacked Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.

Based on the MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html

## Implementations

| Language | Directory | Status |
|----------|-----------|--------|
| **C** | [`c/`](c/) | Production |
| **C++** | [`cpp/`](cpp/) | Production |
| **C#** | [`csharp/`](csharp/) | Production |
| **Fortran** | [`fortran/`](fortran/) | Production |
| **Go** | [`go/`](go/) | Production |
| **Java** | [`java/`](java/) | Production |
| **JavaScript** | [`js/`](js/) | Production |
| **Julia** | [`julia/`](julia/) | Production |
| **Kotlin** | [`kotlin/`](kotlin/) | Production |
| **Perl** | [`perl/`](perl/) | Production |
| **PHP** | [`php/`](php/) | Production |
| **Python** | [`python/`](python/) | Production |
| **Ruby** | [`ruby/`](ruby/) | Production |
| **Rust** | [`rust/`](rust/) | Production |
| **Swift** | [`swift/`](swift/) | Production |
| **Tcl** | [`tcl/`](tcl/) | Production |
| **TypeScript** | [`typescript/`](typescript/) | Production |

All implementations pass the same test suite (2M+ conversions, 94 error cases) and produce identical results.

## Quick Start

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

### Python
```python
from mpc_designation import convert_simple
convert_simple('1995 XA')  # Returns 'J95X00A'
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
2024 AB631    -> _4AMu1B

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
make test-c
make test-cpp
make test-csharp
make test-fortran
make test-go
make test-java
make test-julia
make test-js
make test-kotlin
make test-perl
make test-php
make test-python
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

Benchmark results on Apple M3 Pro, processing 2,021,090 designation conversions.

### Pack Direction (unpacked → packed)

| Language | Time (ms) | Rate (entries/sec) | Relative |
|----------|----------:|-------------------:|---------:|
| Go | 558 | 3,618,747 | 1.00x |
| JavaScript | 849 | 2,380,554 | 0.65x |
| C | 962 | 2,100,925 | 0.58x |
| TypeScript | 1,137 | 1,777,564 | 0.49x |
| Rust | 1,339 | 1,509,402 | 0.41x |
| Fortran | 2,531 | 798,534 | 0.22x |
| Julia | 2,994 | 675,128 | 0.18x |
| C# | 3,257 | 620,537 | 0.17x |
| Kotlin | 5,775 | 349,972 | 0.09x |
| PHP | 6,399 | 315,821 | 0.08x |
| Swift | 8,637 | 234,004 | 0.06x |
| Java | 10,524 | 192,046 | 0.05x |
| Python | 10,612 | 190,449 | 0.05x |
| Perl | 15,718 | 128,584 | 0.03x |
| C++ | 21,907 | 92,258 | 0.02x |
| Ruby | 22,389 | 90,272 | 0.02x |
| Tcl | 41,976 | 48,149 | 0.01x |

### Unpack Direction (packed → unpacked)

| Language | Time (ms) | Rate (entries/sec) | Relative |
|----------|----------:|-------------------:|---------:|
| C | 283 | 7,141,661 | 1.00x |
| C# | 296 | 6,828,007 | 0.95x |
| JavaScript | 450 | 4,491,311 | 0.62x |
| C++ | 455 | 4,441,956 | 0.62x |
| Go | 465 | 4,340,646 | 0.60x |
| TypeScript | 577 | 3,502,756 | 0.49x |
| Rust | 790 | 2,558,342 | 0.35x |
| Kotlin | 1,341 | 1,507,151 | 0.21x |
| Fortran | 1,909 | 1,058,717 | 0.14x |
| Java | 2,356 | 857,848 | 0.12x |
| Julia | 2,658 | 760,351 | 0.10x |
| Swift | 4,182 | 483,283 | 0.06x |
| PHP | 4,370 | 462,449 | 0.06x |
| Python | 4,972 | 406,526 | 0.05x |
| Ruby | 11,896 | 169,897 | 0.02x |
| Perl | 12,601 | 160,391 | 0.02x |
| Tcl | 27,477 | 73,556 | 0.01x |

### Round-trip Verification

All implementations pass the packed round-trip test: `pack(unpack(y)) = y`.

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
