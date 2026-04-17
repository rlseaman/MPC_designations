# Satellite and Naming Support: Analysis

**Date:** 2026-04-17
**Status:** Investigation notes — no code changes yet. Revisit before scoping implementation work.

## Context

The MPC has continued to designate new irregular satellites of Jupiter and
Saturn in recent years. The library currently supports **provisional** natural
satellite designations (`S/YYYY P n` ↔ `S[cc][yy][P][nn]0`) but not
**permanent** satellite designations (Roman-numeral form, e.g., Jupiter XIII).
Before implementing anything, we investigated the full scope of the gap and the
authoritative sources that would underpin a proper fix.

## Current coverage in the library

| Class | Supported | Notes |
|---|---|---|
| Asteroids — numbered (1 to 15,396,335) | Yes | Three encoding tiers, incl. base-62 `~` prefix |
| Asteroids — provisional | Yes | Including extended (cycle ≥ 620) and pre-1925 A/B-prefix |
| Asteroids — survey (PLS, T-1/2/3) | Yes | |
| Comets — numbered | Yes | Incl. 1- and 2-letter fragments |
| Comets — provisional | Yes | Incl. 2-letter fragments, ancient (<1000 CE), BCE |
| Satellites — provisional (`S/YYYY P n`) | Yes | Planet codes J, S, U, N |
| **Satellites — permanent (Roman numeral)** | **No** | MPC spec documents `[Planet][NNN]S` format (e.g., `J013S` = Jupiter XIII) |
| **Named bodies → designation lookup** | **No** | Entirely out of scope today |

## Planet-code gaps in the MPC spec

The official [MPC Packed Designations](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
page lists exactly **four** planet codes for natural satellites:

| Code | Planet |
|---|---|
| J | Jupiter |
| S | Saturn |
| U | Uranus |
| N | Neptune |

Conspicuously absent:

- **Earth** (the Moon). Earth's Moon has no MPC packed designation. The Moon predates
  the designation system and sits outside MPC's remit. The IAU refers to it
  simply as "Moon" (or "Luna"). A hypothetical `E001S` would be an invention,
  not a standard — do not implement.
- **Mars** (Phobos, Deimos). The spec does not document Mars. In practice,
  however, astrometric data in the wild uses `M001S` for Mars I (Phobos) —
  see Project Pluto's pseudo-MPEC for
  [Phobos](https://mail.projectpluto.com/natsats/phobos.htm). Deimos is
  presumably `M002S` by analogy. This is a **de facto convention not in the
  written spec.**
- **Pluto** (5 satellites in JPL's table). MPC spec assigns no letter; `P`
  would collide with the comet prefix. Unresolved.

**Action:** Before coding permanent satellite support, submit a help-desk
question asking MPC to clarify the official planet codes for Mars, Pluto, and
(for completeness) whether Earth is ever in scope.

## Permanent satellite format (confirmed by MPC help desk)

Previously resolved with the MPC (see
`docs/COMPLETE_DESIGNATION_REFERENCE.md` Part 11.4):

- Form: `[Planet][NNN]S` — one planet letter, three-digit zero-padded decimal
  Roman-numeral value, final `S`.
- Example: Jupiter XIII → `J013S`, Saturn X → `S010S`.
- Maximum: 999.

## Authoritative tables for names

There is **no single IAU table** joining packed form ↔ Roman numeral ↔ IAU
name. The useful sources are scattered:

| Source | Coverage | URL |
|---|---|---|
| JPL SSD Planetary Satellite Discovery Circumstances | ~459 satellites; joins Roman numeral, IAU name, provisional designation, year, discoverer | [ssd.jpl.nasa.gov/sats/discovery.html](https://ssd.jpl.nasa.gov/sats/discovery.html) |
| IAU WGPSN / USGS Gazetteer of Planetary Nomenclature | Official approved names (features and bodies) | [planetarynames.wr.usgs.gov](https://planetarynames.wr.usgs.gov/Page/Approved) |
| MPC MPCORB / `mpcorb_extended.json` | Numbered asteroids → names | [minorplanetcenter.net/iau/MPCORB.html](https://www.minorplanetcenter.net/iau/MPCORB.html) |
| MPC `CometEls.txt` / `AllCometEls.txt` | Comet orbital elements including names | [minorplanetcenter.net/data](https://www.minorplanetcenter.net/data) |
| Planets themselves | No MPC designation; IAU-named only | n/a |

For **satellites specifically**, JPL's table is the cleanest pre-joined source
(~459 rows, stable, text-scrapable, includes permanent-to-provisional cross-ref).

Explicit exclusions to note:

- JPL's table says "459 planetary satellites (including those of Pluto but not
  Earth)" — confirms Earth's Moon is excluded from the standard corpus.
- The MPC data portal does not currently list a downloadable natural-satellite
  names file (only a "Natural Satellite Ephemerides" service).

## The coupling question

The user correctly observed: adding *named satellites* to a library that
already knows about *named asteroids* and *named comets* crosses a design line.
Today the project is a **pure grammar converter** — deterministic, data-free,
identical across 25 languages. Introducing name lookups means bringing in a
**reference database** that:

1. Requires periodic refresh (MPC and JPL both update monthly).
2. Scales asymmetrically: ~620,000 named-asteroid rows, a few hundred named
   comets, ~459 named satellites.
3. Must live somewhere that works for all 25 language targets (shared CSV/JSON?
   generated per-language modules? Git-LFS-backed blob?).
4. Invites scope creep — once names are in, users will ask for orbital
   elements, ephemerides, cross-references to other catalogs.

## Candidate scopes (smallest to largest)

1. **Grammar only.** Add permanent-satellite format (`J013S`) for J/S/U/N as
   documented, plus M (Mars) as a tolerated non-spec extension with a
   `strict=True` option to reject. No names table. In keeping with the current
   library's character — stays data-free, 25-way parity preserved. **Est.
   effort:** ~1–2 days of parse/pack/round-trip work plus test updates.

2. **Grammar + satellite names (separate optional module).** Ship scope #1 in
   the core library, and add a small *optional* name-lookup module
   (`mpc_satellite_names` or similar) that bundles a static CSV derived from
   JPL's discovery table (~459 rows, stable, refreshable). Does not break the
   "pure grammar" design of the core — names live in a sibling module that
   depends on core for designation parsing. **Est. effort:** +1 day for the
   CSV + Python/C/Tcl wrapper, more if we parity-ship to all 25 languages.

3. **Grammar + all name lookups (asteroids, comets, satellites).** A
   fundamentally different project — an MPC *reference-data* library, not a
   designation-grammar library. Different scaling, different refresh cadence,
   different API surface. Probably belongs in a separate repository.

## Recommended next steps (in order)

1. **Ask MPC help desk** two questions:
   - What is the official planet code for Mars, Pluto, and (ever) Earth in the
     permanent-satellite packed format? Is `M001S` for Phobos a sanctioned
     convention or unofficial community use?
   - Does MPC plan to publish a machine-readable satellite names table, or
     should downstream tools treat JPL's discovery table as the reference?
2. **Decide on scope** (pick 1, 2, or 3 above) with answers from MPC in hand.
3. **Update `docs/SPECIFICATION.md`** and `docs/FORMATS.md` to explicitly
   document the permanent-satellite format before implementing.
4. **Implement scope #1 first** in Python (reference implementation), then
   propagate across the 25 languages using the existing per-language checklist
   pattern from `CLAUDE_NOTES.md`.
5. **If pursuing #2,** design the names module as an optional sibling, not a
   core dependency.

## Sources consulted

- [MPC Packed Designations (PackedDes.html)](https://www.minorplanetcenter.net/iau/info/PackedDes.html)
- [JPL Planetary Satellite Discovery Circumstances](https://ssd.jpl.nasa.gov/sats/discovery.html)
- [Project Pluto pseudo-MPEC for Mars I (Phobos)](https://mail.projectpluto.com/natsats/phobos.htm)
- [USGS/IAU Gazetteer of Planetary Nomenclature](https://planetarynames.wr.usgs.gov/Page/Approved)
- [IAU WGPSN](https://www.iau.org/WG98/WG98/Home.aspx)
- [Naming of natural satellites (Wikipedia)](https://en.wikipedia.org/wiki/Naming_of_natural_satellites)
- [MPCORB Database](https://www.minorplanetcenter.net/iau/MPCORB.html)
- [MPC Data page](https://www.minorplanetcenter.net/data)
- Internal: `docs/COMPLETE_DESIGNATION_REFERENCE.md` (Parts 5.3, 10.4, 11.4)
- Internal: `CLAUDE_NOTES.md` (MPC help-desk clarification #4)
