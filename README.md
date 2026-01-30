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

**Note:** Languages not shown require specific compilers/runtimes (e.g., `kotlinc`, `dotnet`, `julia`) that may not be installed. Run `./scripts/benchmark.sh` to benchmark all available implementations.

### Pack Direction (unpacked → packed)

| Language | Time (ms) | Rate (entries/sec) | Relative |
|----------|----------:|-------------------:|---------:|
| Go | 576 | 3,505,992 | 1.00x |
| JavaScript | 843 | 2,397,497 | 0.68x |
| C | 966 | 2,092,226 | 0.59x |
| Rust | 1,416 | 1,427,323 | 0.40x |
| Fortran | 2,509 | 805,536 | 0.22x |
| Julia | 2,997 | 674,288 | 0.19x |
| Kotlin | 5,891 | 343,081 | 0.09x |
| PHP | 6,429 | 314,369 | 0.08x |
| Swift | 8,770 | 230,455 | 0.06x |
| Python | 10,577 | 191,077 | 0.05x |
| Java | 11,241 | 179,796 | 0.05x |
| Perl | 16,189 | 124,843 | 0.03x |
| C++ | 22,234 | 90,901 | 0.02x |
| Ruby | 22,473 | 89,934 | 0.02x |
| Tcl | 42,248 | 47,839 | 0.01x |

### Unpack Direction (packed → unpacked)

| Language | Time (ms) | Rate (entries/sec) | Relative |
|----------|----------:|-------------------:|---------:|
| C | 285 | 7,091,544 | 1.00x |
| JavaScript | 454 | 4,451,740 | 0.62x |
| C++ | 456 | 4,432,215 | 0.62x |
| Go | 476 | 4,240,651 | 0.59x |
| Rust | 797 | 2,535,872 | 0.35x |
| Kotlin | 1,319 | 1,532,290 | 0.21x |
| Fortran | 1,939 | 1,042,336 | 0.14x |
| Java | 2,355 | 858,212 | 0.12x |
| Julia | 2,490 | 811,700 | 0.11x |
| Swift | 4,236 | 477,122 | 0.06x |
| PHP | 4,338 | 465,896 | 0.06x |
| Python | 5,065 | 399,000 | 0.05x |
| Ruby | 11,831 | 170,830 | 0.02x |
| Perl | 12,588 | 160,557 | 0.02x |
| Tcl | 27,846 | 72,581 | 0.01x |

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
