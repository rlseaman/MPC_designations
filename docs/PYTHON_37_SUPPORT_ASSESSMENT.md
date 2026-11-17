# Python 3.7 Support — Assessment & Recommendation

**Status:** Proposal / decision memo (no work started)
**Audience:** Engineering meeting — Python baseline discussion
**Prepared:** 2026-06-25
**Bottom line:** Backporting `mpc-designation` to Python 3.7.6 is *cheap to build and expensive to keep.* The recommendation is **not** to lower the floor. Instead, upgrade the environment(s) still pinned to 3.7.6 to a supported interpreter (3.9+). It is far better to move the affected systems forward now than to commit the package — and ourselves — to an end-of-life runtime.

> **Annotations (2026-11-17, added when committing this memo to the repo):**
> - **Decision status:** no decision has been recorded. The published floor is
>   unchanged at `requires-python = ">=3.9"`; Option A has not been implemented.
> - **Time has moved:** Python 3.9 reached end-of-life in October 2026, so §5's
>   advice to target 3.10/3.11 (not 3.9) for the pinned environments now
>   applies with more force. Whether to raise the package floor to 3.10 is a
>   separate, open question.
> - **§3.1 fallback string:** the hard-coded `__version__` fallback in
>   `__init__.py` was stale (`"1.0.0"`) on every interpreter where metadata
>   lookup fails, not just 3.7; it is now kept in step with `VERSION` at
>   each release.
> - **Corpus size:** references to the 2,022,404-row corpus describe the
>   test data as of 1.1.0; the corpus is refreshed against the MPC catalog
>   from 1.1.2 onward.
> - Prepared as a meeting memo; kept here as the record of the analysis.

---

## 1. Why this is on the table

`mpc-designation` currently declares `requires-python = ">=3.9"`. At least one downstream environment is pinned to **Python 3.7.6** and cannot install the package as published, and it sounds like more than one team has hit the same 3.7.x wall. The question for the meeting is whether to:

- **(A)** Lower the package floor to support 3.7.6, or
- **(B)** Upgrade the pinned environments to a supported Python and leave the floor where it is.

This memo scopes (A) honestly so the comparison is fair, then recommends (B).

## 2. The surprising part: the code barely cares

A direct audit of the Python implementation (not a guess from the declared floor) shows the runtime and test code are written in a 3.6/3.7-compatible dialect already. The `>=3.9` floor is a **policy choice, not a technical constraint.**

What the code does **not** use (all checked, all absent):

- PEP 604 `X | Y` unions and PEP 585 builtin generics (`list[str]`) — it uses classic `typing.List`, `Optional`, `Dict`, etc.
- Walrus `:=`, `match`/`case`, f-string `=` debugging
- `str.removeprefix`/`removesuffix`, `math.prod`, `functools.cache`, `dataclasses`, `zoneinfo`, `tomllib`

So the conversion logic that processes the full 2,022,404-row corpus would run unmodified on 3.7.6.

## 3. What a 3.7 support project would actually entail

Three buckets. Only the first contains code, and it is tiny.

### 3.1 Code (one real change)

- **`__init__.py` version lookup.** `from importlib.metadata import version` — `importlib.metadata` is 3.8+. On 3.7 the import fails, is swallowed by the existing `except Exception`, and `__version__` silently falls back to the hard-coded `"1.0.0"`. An installed 1.1.0 would misreport itself.
  - *Fix:* add a conditional backport dependency, `importlib-metadata; python_version < "3.8"`, and import from it on the fallback path — **or** accept a wrong `__version__` string on 3.7.

That is the entire code surface. Everything else in `mpc_designation.py` and `batch.py` is already compatible.

### 3.2 Tests (no code changes)

- All six files under `python/test/` import stdlib only (`os`, `sys`, `time`, `re`, `codecs`) plus the package. No `pytest`, no third-party deps, no version-gated syntax. They run as-is on 3.7.6.
- **Caveat:** they are hand-rolled scripts (`def test_…` + `if __name__ == "__main__":`), not a `pytest`/`unittest` discovery suite. "Passes on 3.7" is therefore not free — each script must be wired into a 3.7 CI matrix job by hand.

### 3.3 Packaging (the actual work)

- `requires-python` → `>=3.7`; add `3.7` and `3.8` classifiers.
- **SPDX license declaration.** `license = "CC0-1.0"` (PEP 639 string form) requires setuptools ≥77 and a modern pip. The pip/setuptools that ship with 3.7 do not understand it. We would revert to the legacy `license = {text = "CC0-1.0"}` table form or a `License ::` classifier — losing the modern declaration.
- **Build backend pin.** setuptools dropped Python 3.7 in v68 (2023). Building *on* 3.7 pins us to setuptools 61–67.

**Rough one-time cost: half a day** — one dependency line, a license-metadata downgrade, classifier/floor edits, and a new CI matrix entry.

## 4. Why the one-time cost is the wrong number to look at

The build cost is small; the **carrying cost** is the problem, and it is permanent:

1. **3.7 is end-of-life (June 2023).** Lowering the floor advertises support for an interpreter that receives no security patches. For an astronomy utility this is low-stakes, but it is still a promise we would be making in metadata.
2. **Dialect lock-in.** Today we use no 3.8+ feature, so the floor "costs nothing." The moment anyone wants a walrus operator, `match`, builtin generics, `importlib.metadata`, or a modern typing idiom, they hit a wall they didn't know was there — and reverting a floor after publication is painful.
3. **Expanded test matrix forever.** Every release would need a 3.7 (and realistically 3.8) job, against toolchain versions frozen in 2023, with the hand-wired script runner noted in §3.2.
4. **Toolchain drift.** Pinned-old setuptools/pip and the downgraded license metadata are friction every contributor inherits.

We would take on all of that to serve environments that are themselves overdue for an upgrade.

## 5. The alternative: move the pinned environments forward

The systems stuck on 3.7.6 are the real issue, and they will keep generating problems like this one (not just for our package) until they move. Reasons to upgrade them now rather than backport around them:

- **3.7.6 specifically** dates to December 2019 and has had no security fixes since June 2023. Other dependencies in those environments are accumulating the same EOL risk.
- **The minimum useful target, 3.9, is itself near EOL (October 2026).** If we are going to ask anyone to move, the floor should be 3.10 or 3.11, not a hop to 3.9 that needs repeating in months.
- Once the environment is on 3.9+, `mpc-designation` installs today with zero changes, and so does anything else those teams have been working around.

If a single environment genuinely cannot move before the meeting, there is a non-committal stopgap that does **not** change what we publish: that team installs from a local checkout carrying the two-line patch from §3.1 + §3.3 (backport pin and floor), while PyPI stays at `>=3.9`. That unblocks them without making 3.7 a project.

### 5.1 Sidebar — what newer Python actually buys us

A key point for the floor debate: for *this* package, most of the value in newer Python comes from **running on it**, not from **requiring it**. Those have very different costs.

**Free wins (benefit any user on a newer interpreter — no floor change, no code change):**

- **Interpreter speedup** — 3.11 is ~1.25× faster than 3.10 on average, with further gains in 3.12 and the 3.14 tail-call interpreter. The 2M-row corpus benchmark is exactly the string/regex/integer workload this accelerates. 3.9 leaves it on the table; capturing it costs nothing but testing on a newer interpreter (which our classifiers already do through 3.14).
- **Fine-grained tracebacks, "did you mean" hints, colored output** (3.11 → 3.14) and the **new PyREPL** (3.13+) — quality-of-life for anyone debugging the 25-language interop harness.

None of these require raising the floor. They are reasons to keep testing on 3.14, not reasons to drop 3.9–3.13 users.

**Gated wins (only if we raise the floor, i.e. give up older users):**

- **Free-threading (no-GIL)** — experimental in 3.13, officially supported in 3.14 (still a separate build). The one genuinely code-relevant win: `batch.py` uses `ProcessPoolExecutor` specifically to dodge the GIL, and its `convert_batch_threaded` path is documented as slower for CPU-bound work. On a free-threaded build that inverts — threads could match or beat processes without pickle/spawn overhead. Our functions are pure, so thread-safety is likely already fine. This is a *later, opt-in* story (needs a 3.14t build + a thread-safety pass), not a reason to bump the published floor now.
- **`match`/`case` (3.10)** — the defensible ergonomic win; `detect_format()`/`convert()` are dispatch chains that read naturally as pattern matches. Costs a ≥3.10 floor.
- **`X | Y` unions, builtin generics, PEP 695 type syntax, deferred annotations** — cosmetic here; lets us drop `typing` imports. Low payoff for a module this small.

**Net for the floor decision:** the wins that matter for a string-conversion library and its benchmark (speed, debuggability) are *free* — captured by running on 3.11+, not by requiring it. The only floor-gated payoff in the code is free-threading in `batch.py`, and that is opt-in and not yet used. Jumping the floor to 3.14 would trade the entire 3.9–3.13 user base for `match`-statement aesthetics and a free-threading path we are not exercising yet. Recommended posture: target a *running* interpreter of 3.11+ to bank the free wins; keep the *required* floor modest.

## 6. Recommendation

- **Do not lower the published floor to 3.7.** (Option A)
- **Use this finding to argue for upgrading the pinned environments past 3.9 — ideally to 3.10/3.11.** (Option B)
- If pressed for an immediate unblock, offer the local-patch stopgap in §5, explicitly scoped as a bridge, not support.

The engineering meeting framing in one line: *it is cheaper and safer to move a handful of environments forward once than to drag the package — and every future contributor — backward to meet them.*

---

## Appendix A — Audit evidence

Files inspected: `python/src/mpc_designation/{mpc_designation.py, batch.py, __init__.py}`, `python/test/*.py`, `python/pyproject.toml`.

| Feature gate | Min version | Used? |
|---|---|---|
| classic `typing` generics (`List`, `Optional`, `Dict`, `Tuple`, `Any`, `Union`, `Callable`) | 3.5 | **Yes** (so: 3.7-safe) |
| f-strings | 3.6 | Yes |
| `importlib.metadata` | **3.8** | **Yes — in `__init__.py` only** |
| walrus `:=` | 3.8 | No |
| f-string `=` debug | 3.8 | No |
| PEP 585 builtin generics `list[...]` | 3.9 | No |
| PEP 604 `X | Y` unions | 3.10 | No |
| `str.removeprefix/removesuffix` | 3.9 | No |
| `match`/`case` | 3.10 | No |
| `math.prod` / `functools.cache` / `dataclasses` / `zoneinfo` / `tomllib` | 3.7–3.11 | No |

Test suite (`python/test/`): stdlib-only imports (`os`, `sys`, `time`, `re`, `codecs`), no third-party test framework, no version-gated syntax. Compatible with 3.7.6 as written; not auto-discovered (manual CI wiring required).

## Appendix B — Concrete change list if Option A is ever chosen

1. `pyproject.toml`: `requires-python = ">=3.7"`; add `Programming Language :: Python :: 3.7` and `:: 3.8` classifiers.
2. `pyproject.toml`: replace `license = "CC0-1.0"` with `license = {text = "CC0-1.0"}` (or rely on a `License ::` classifier); confirm install on the 3.7-era pip/setuptools.
3. `pyproject.toml`: add dependency `importlib-metadata; python_version < "3.8"`.
4. `__init__.py`: fall back to `importlib_metadata` (backport) when `importlib.metadata` is unavailable, instead of returning the stale `"1.0.0"`.
5. CI: add a 3.7 (and 3.8) matrix job that explicitly runs each `python/test/*.py` script.
