# Input-Validation Gaps (awk, bash, nim, forth, haskell)

Work-queue document. Created when the five bespoke error-test harnesses were
harmonized to read the shared `test-data/error_test_cases.csv` (98 cases).
Harmonization made these implementations' input validation measurable against
the full suite for the first time and surfaced a layer of **pre-existing**
gaps — inputs that the reference implementations (Python, Tcl, and most others)
reject but these five silently accept or mis-handle.

These are **not** regressions from the 2026 packed-provisional detection work
(century `I-L`, half-month/order `I`-skip, comet half-month, cycle-units digit).
That work is complete and passes in all 25 implementations. The 4 detection
collision cases (`C03UYWZ`, `K03UYWZ`, `K95I00A`, `1995 XI`) **pass** in all
five harnesses here — they are not skips.

Each gap is currently recorded as a **documented expected-skip** in the
language's harness (the same convention `c/test/test_errors.c` uses for its two
inherent null-byte cases), so the suites report `Failed: 0` with an explicit
`Skipped: N` and per-case reasons. Closing a gap = implementing the missing
check and removing the skip.

## Reference status

| Implementation | error suite |
|---|---|
| Python, Tcl | 98 / 98, **0 skips** — the validation reference |
| C, C++, C#, Go, Java, Kotlin, Rust, Swift, Julia, PHP, Perl, Ruby, R, Octave, TypeScript, JS | full suite (C skips 2 inherent null-byte cases) |
| **nim** | 87 pass / **11 skip** |
| **haskell** | 81 pass / **17 skip** |
| **bash** | 74 pass / **24 skip** |
| **awk** | 71 pass / **27 skip** |
| **forth** | 67 pass / **31 skip** (most permissive; no error channel) |

## Gaps by theme

### 1. Numeric range / bounds checks (highest impact, most shared)
No validation of numeric magnitudes. Affects: **awk, bash, nim, forth** (and
`comet_order_zero` in **haskell**).
- Asteroid number: `asteroid_zero` (0), `asteroid_max_plus_one` (>15,396,335),
  `asteroid_huge`, `asteroid_overflow`, `packed_above_max` (`~{000`).
- Provisional cycle count: `cycle_huge`, `cycle_overflow`.
- Comet order number: `comet_order_zero`, `comet_order_huge`.
- Survey number: `survey_zero`, `survey_negative`.
- Satellite number: `satellite_zero_number`, `satellite_negative`,
  `satellite_missing_number`.
- Provisional year range: `year_future`, `year_zero` (awk, forth).

### 2. Whitespace strictness
Whitespace runs tolerated/collapsed instead of rejected. Affects: **haskell**
(most — collapses any whitespace run as a separator), **forth**, **bash**.
- `trailing_tab`, `double_space_survey`, `double_space_satellite`,
  `double_space_comet`, `double_space_old_style`, and (haskell) the base
  `double_space`/`triple_space`/`tab_instead`/`leading_tab`/`newline`/
  `carriage_return`/`form_feed`/`vertical_tab`.

### 3. Structural / format-completeness checks
Malformed-but-parseable inputs truncated or silently accepted. Affects: **awk,
bash, forth, haskell** (varies).
- `three_letters` (e.g. `1995 XAB` truncated to two letters).
- `comet_no_year`, `comet_no_provisional`, `comet_double_slash`.
- `comet_invalid_fragment` (numeric), `comet_long_fragment` (>2 letters).
- `old_style_invalid_prefix` (`C908`), `old_style_wrong_length` (`A90`),
  `old_style_lowercase` (`a908`).
- `numbered_comet_zero` (`0P`).
- `very_long_string` (over-long digit run encoded as a tilde asteroid).

### 4. Embedded NUL bytes (inherent, shared with C)
`null_byte`, `null_middle`. **awk, bash** — a NUL terminates/splits the value,
same root cause as the documented C limitation. Likely permanent skips.

### 5. Genuinely missing features
- **bash**: no ancient (`<1000 CE`) or BCE comet support — `ancient_comet`
  (`C/240 V1`) and `bce_comet` (`C/-146 P1`) fail. (Note: **awk** *does* support
  these; **nim** gained BCE/ancient unpack in the 2026 round.) This is a real
  feature port, not just a validation check.

### 6. Injection / unicode lookalikes
- **forth**: `sql_injection` (`1; DROP TABLE--`) parsed rather than rejected.
- **haskell**: `mixed_encoding` (Cyrillic `А`), `nbsp` (non-breaking space).

## Suggested priority

1. **Numeric range checks** (theme 1) — broadest, clearest correctness win,
   touches four languages. Mechanical: add bounds checks mirroring Python/Tcl.
2. **Structural checks** (theme 3) — moderate effort, real correctness value.
3. **Whitespace strictness** (theme 2) — mostly haskell; revisit how its parser
   tokenizes separators.
4. **bash ancient/BCE comet support** (theme 5) — a feature port; do only if
   bash parity matters.
5. **NUL bytes** (theme 4) — likely accept as permanent documented skips
   (string-termination limit), as C already does.
6. **Injection/unicode** (theme 6) — low volume; fold into theme 2/3 work.

## How to work an item

For each language, implement the missing check (reference Python's
`validate_*`/range logic and Tcl's equivalents), then remove the corresponding
entry from that harness's skip-list and confirm the case now PASSES against
`test-data/error_test_cases.csv`. The harness already prints which cases are
skipped, so progress is directly measurable: the goal is `Skipped: 0` (except
awk/bash null-byte, which match the C precedent).
