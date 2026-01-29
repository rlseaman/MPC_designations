# MPC Designation Format Specification

This document describes the Minor Planet Center (MPC) designation formats for asteroids, comets, and natural satellites.

## Reference

Official MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html

## Overview

The MPC uses two designation formats:

1. **Unpacked** (human-readable): e.g., `1995 XA`, `C/1995 O1`, `S/2019 S 22`
2. **Packed** (compact): e.g., `J95X00A`, `CJ95O010`, `SK19S220`

This library converts between these formats in both directions.

## Asteroids

### Permanent (Numbered) Asteroids

Asteroids with confirmed orbits receive a permanent number.

| Number Range | Packed Format | Example Unpacked | Example Packed |
|-------------|---------------|------------------|----------------|
| 1 - 99,999 | 5 digits, zero-padded | 1 | 00001 |
| 100,000 - 619,999 | Letter + 4 digits | 100001 | A0001 |
| 620,000+ | Tilde + base-62 | 620000 | ~0000 |

The letter encoding for 100,000+:
- A = 10, B = 11, ... Z = 35
- a = 36, b = 37, ... z = 61

Maximum encodable number: 15,396,335 (= 620,000 + 62^4 - 1)

### Provisional Asteroids

Newly discovered asteroids receive a provisional designation based on discovery date.

Format: `YYYY LLn` where:
- YYYY = Year
- LL = Half-month code (A-Y, skipping I) + second letter (A-Z, skipping I)
- n = Cycle number (optional, for 25+ discoveries in that half-month)

| Format | Unpacked | Packed | Notes |
|--------|----------|--------|-------|
| Standard | 1995 XA | J95X00A | First discovery in second half of December 1995 |
| With cycle | 1995 XB1 | J95X01B | 26th discovery |
| Extended | 2024 AA631 | _4AMu1A | Cycle >= 620 uses extended format |

Century codes:
- I = 1800s, J = 1900s, K = 2000s, L = 2100s

### Survey Asteroids

Special designations for systematic surveys:

| Survey | Unpacked | Packed |
|--------|----------|--------|
| Palomar-Leiden | 2040 P-L | PLS2040 |
| Trojan Survey 1 | 3138 T-1 | T1S3138 |
| Trojan Survey 2 | 1234 T-2 | T2S1234 |
| Trojan Survey 3 | 5678 T-3 | T3S5678 |

## Comets

### Numbered Periodic Comets

Comets with confirmed periodic orbits:

| Unpacked | Packed | Notes |
|----------|--------|-------|
| 1P | 0001P | Halley's Comet |
| 354P | 0354P | |

### Provisional Comets

Format: `T/YYYY Ln` where T is the comet type:
- P = Periodic
- C = Non-periodic (long period)
- D = Defunct/disappeared
- X = Uncertain orbit
- A = Asteroid-like comet
- I = Interstellar

| Unpacked | Packed | Notes |
|----------|--------|-------|
| C/1995 O1 | CJ95O010 | Comet Hale-Bopp |
| P/2019 A4 | PK19A040 | |

### Comet Fragments

Fragmented comets append a fragment letter:

| Unpacked | Packed | Notes |
|----------|--------|-------|
| D/1993 F2-B | DJ93F02b | Shoemaker-Levy 9 fragment B |
| P/1930 J1-AA | PJ30J01aa | Two-letter fragment |

### Ancient Comets

Historical observations use special encoding:

| Year Range | Unpacked | Packed | Notes |
|------------|----------|--------|-------|
| 1-999 CE | C/240 V1 | C240V010 | Year without century code |
| BCE | C/-146 P1 | C.53P010 | Dot prefix for BCE |

BCE year encoding: -146 BCE = 200 - 146 - 1 = 53

## Natural Satellites

Provisional satellite discoveries:

Format: `S/YYYY P n` where:
- YYYY = Year
- P = Planet code (J=Jupiter, S=Saturn, U=Uranus, N=Neptune)
- n = Discovery number

| Unpacked | Packed |
|----------|--------|
| S/2019 S 22 | SK19S220 |
| S/2003 J 2 | SJ03J020 |

## Base-62 Encoding

Used for large numbers in packed format:

```
0-9:  0-9
A-Z: 10-35
a-z: 36-61
```

4-character base-62 encodes 0 to 14,776,335 (62^4 - 1).

## Half-Month Codes

| Letter | Dates |
|--------|-------|
| A | Jan 1-15 |
| B | Jan 16-31 |
| C | Feb 1-15 |
| D | Feb 16-29 |
| ... | ... |
| Y | Dec 16-31 |

Note: Letter I is not used (to avoid confusion with numeral 1).
