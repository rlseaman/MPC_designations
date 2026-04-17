# Quick Start

Convert MPC packed ↔ unpacked designations from your language of choice. Every
implementation below takes the same input and produces the same output.

Canonical example throughout: `"1995 XA"` (unpacked) ↔ `"J95X00A"` (packed).

## Shell / CLI

```bash
# Python (from python/)
python3 -c "from mpc_designation import convert_simple; print(convert_simple('1995 XA'))"
# J95X00A

# C (from c/ after `make`)
./mpc_designation '1995 XA'              # J95X00A

# Tcl (from tcl/)
echo 'source src/mpc_designation.tcl; puts [MPCDesignation::convertSimple "1995 XA"]' | tclsh

# Go (from go/ after `make`)
./mpc_designation '1995 XA'

# Rust (from rust/)
cargo run --release -- '1995 XA'

# Node.js (from js/)
node src/mpc_designation_cli.js '1995 XA'
```

## Library API — ten common languages

```python
# Python
from mpc_designation import convert_simple, pack, unpack
convert_simple('1995 XA')   # 'J95X00A'
convert_simple('J95X00A')   # '1995 XA'
```

```c
/* C */
#include "mpc_designation.h"
char out[32];
mpc_convert_simple("1995 XA", out, sizeof(out));   /* out == "J95X00A" */
```

```tcl
# Tcl
source mpc_designation.tcl
MPCDesignation::convertSimple "1995 XA"   ;# "J95X00A"
```

```go
// Go
import "github.com/rlseaman/mpc_designations/go/mpc"
s, _ := mpc.ConvertSimple("1995 XA")   // "J95X00A"
```

```rust
// Rust
use mpc_designation::convert_simple;
let s = convert_simple("1995 XA").unwrap();   // "J95X00A"
```

```javascript
// JavaScript
const { convertSimple } = require('./src/mpc_designation');
convertSimple('1995 XA');   // 'J95X00A'
```

```java
// Java
import mpc.MPCDesignation;
String s = MPCDesignation.convertSimple("1995 XA");   // "J95X00A"
```

```ruby
# Ruby
require_relative 'src/mpc_designation'
MPCDesignation.convert_simple('1995 XA')   # 'J95X00A'
```

```perl
# Perl
use MPC::Designation qw(convert_simple);
convert_simple('1995 XA');   # 'J95X00A'
```

```php
// PHP
use MPC\MPCDesignation;
MPCDesignation::convertSimple('1995 XA');   // 'J95X00A'
```

Other implementations (AWK, Bash, C++, C#, Forth, Fortran, Haskell, Julia,
Kotlin, Nim, Octave, R, SPP/IRAF, Swift, TypeScript) are in the top-level
[README](../README.md#quick-start).

## The six helper functions

Present in C, Go, Java, JS, Perl, PHP, Python, Ruby, Rust, Tcl, TypeScript:

```python
to_report_format('0073Pa')              # '0073P      a'  (12-char MPC record format)
from_report_format('0073P      a')      # '0073Pa'
has_fragment('73P-A')                   # True
get_fragment('73P-A')                   # 'A'
get_parent('73P-A')                     # '73P'
designations_equal('1995 XA', 'J95X00A')  # True
```

## Typical designations by class

| Class | Unpacked | Packed |
|---|---|---|
| Numbered asteroid | `1` | `00001` |
| Asteroid 100K–620K | `100001` | `A0001` |
| Asteroid > 620K | `620000` | `~0000` |
| Provisional asteroid | `1995 XA` | `J95X00A` |
| Extended provisional | `2024 AB631` | `_OA004S` |
| Pre-1925 asteroid | `A873 OA` | `I73O00A` |
| Survey | `2040 P-L` | `PLS2040` |
| Numbered comet | `1P` | `0001P` |
| Comet with fragment | `73P-A` | `0073Pa` |
| Provisional comet | `C/1995 O1` | `CJ95O010` |
| Ancient comet | `C/240 V1` | `C240V010` |
| BCE comet | `C/-146 P1` | `C.53P010` |
| Provisional satellite | `S/2019 S 22` | `SK19S220` |

## Where to go next

- Full format rules: [`SPECIFICATION.md`](SPECIFICATION.md)
- Quick reference tables: [`FORMATS.md`](FORMATS.md)
- Error-handling contract: [`ERROR_CHECKING.md`](ERROR_CHECKING.md)
- Why 25 implementations: [`DESIGN.md`](DESIGN.md)
- Per-language docs: each language's `README.md`
