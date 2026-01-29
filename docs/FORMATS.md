# MPC Designation Format Reference

Quick reference tables for all supported formats.

## Asteroids - Permanent (Numbered)

| Number Range | Unpacked | Packed | Encoding |
|-------------|----------|--------|----------|
| 1 | 1 | 00001 | 5-digit zero-padded |
| 99999 | 99999 | 99999 | 5-digit zero-padded |
| 100001 | 100001 | A0001 | Letter=10, digits=0001 |
| 360001 | 360001 | a0001 | Letter=36, digits=0001 |
| 619999 | 619999 | z9999 | Maximum standard format |
| 620000 | 620000 | ~0000 | Tilde + base-62 |
| 15396335 | 15396335 | ~zzzz | Maximum encodable |

### Letter to Number Mapping

| Letter | Value | Number Start |
|--------|-------|--------------|
| A | 10 | 100,000 |
| B | 11 | 110,000 |
| ... | ... | ... |
| Z | 35 | 350,000 |
| a | 36 | 360,000 |
| b | 37 | 370,000 |
| ... | ... | ... |
| z | 61 | 610,000 |

## Asteroids - Provisional

### Century Codes

| Code | Century | Years |
|------|---------|-------|
| A | 10 | 1000-1099 |
| B | 11 | 1100-1199 |
| C | 12 | 1200-1299 |
| D | 13 | 1300-1399 |
| E | 14 | 1400-1499 |
| F | 15 | 1500-1599 |
| G | 16 | 1600-1699 |
| H | 17 | 1700-1799 |
| I | 18 | 1800-1899 |
| J | 19 | 1900-1999 |
| K | 20 | 2000-2099 |
| L | 21 | 2100-2199 |

### Half-Month Codes

| Code | Dates | Code | Dates |
|------|-------|------|-------|
| A | Jan 1-15 | N | Jul 1-15 |
| B | Jan 16-31 | O | Jul 16-31 |
| C | Feb 1-15 | P | Aug 1-15 |
| D | Feb 16-28/29 | Q | Aug 16-31 |
| E | Mar 1-15 | R | Sep 1-15 |
| F | Mar 16-31 | S | Sep 16-30 |
| G | Apr 1-15 | T | Oct 1-15 |
| H | Apr 16-30 | U | Oct 16-31 |
| J | May 1-15 | V | Nov 1-15 |
| K | May 16-31 | W | Nov 16-30 |
| L | Jun 1-15 | X | Dec 1-15 |
| M | Jun 16-30 | Y | Dec 16-31 |

Note: Letter I is skipped.

### Cycle Count Encoding

| Cycle | Encoded | Notes |
|-------|---------|-------|
| 0 | 00 | First 25 discoveries (A-Z) |
| 1-9 | 01-09 | |
| 10-35 | 0A-0Z | |
| 36-61 | 0a-0z | |
| 62-99 | 10-1z | |
| ... | ... | |
| 619 | 9z | Maximum standard |
| 620+ | Extended | Uses base-62 string |

### Examples

| Unpacked | Packed | Breakdown |
|----------|--------|-----------|
| 1995 XA | J95X00A | J=1900s, 95=year, X=half-month, 00=cycle 0, A=letter |
| 2024 AB1 | K24A01B | K=2000s, 24=year, A=half-month, 01=cycle 1, B=letter |
| 2024 AA631 | _4AMu1A | Extended: _=marker, 4A=base62(2024%100), Mu1=cycle, A=letter |

## Asteroids - Survey

| Survey | Prefix | Unpacked | Packed |
|--------|--------|----------|--------|
| Palomar-Leiden | P-L | 2040 P-L | PLS2040 |
| Trojan 1 | T-1 | 3138 T-1 | T1S3138 |
| Trojan 2 | T-2 | 1234 T-2 | T2S1234 |
| Trojan 3 | T-3 | 5678 T-3 | T3S5678 |

## Comets

### Type Prefixes

| Type | Meaning | Example |
|------|---------|---------|
| P | Periodic | P/2019 A4 |
| C | Non-periodic | C/1995 O1 |
| D | Defunct | D/1993 F2 |
| X | Uncertain orbit | X/1872 X1 |
| A | Asteroid-like | A/2019 G1 |
| I | Interstellar | I/2019 Q4 |

### Provisional Format

| Unpacked | Packed | Notes |
|----------|--------|-------|
| C/1995 O1 | CJ95O010 | Century J, order 1 |
| P/2019 A4 | PK19A040 | Century K, order 4 |
| D/1993 F2 | DJ93F020 | Century J, order 2 |

### Fragments

| Unpacked | Packed | Notes |
|----------|--------|-------|
| D/1993 F2-A | DJ93F02a | Single letter fragment |
| D/1993 F2-B | DJ93F02b | |
| P/1930 J1-AA | PJ30J01aa | Two-letter fragment |
| P/1930 J1-AZ | PJ30J01az | |

### Ancient (Year < 1000)

| Unpacked | Packed | Year Encoding |
|----------|--------|---------------|
| C/240 V1 | C240V010 | 3-digit year, no century code |
| C/837 F1 | C837F010 | |

### BCE Years

| Unpacked | Packed | Calculation |
|----------|--------|-------------|
| C/-43 K1 | C.56K010 | 200 - 43 - 1 = 156, last 2 digits = 56 |
| C/-146 P1 | C.53P010 | 200 - 146 - 1 = 53 |

## Natural Satellites

### Planet Codes

| Code | Planet |
|------|--------|
| J | Jupiter |
| S | Saturn |
| U | Uranus |
| N | Neptune |

### Format

| Unpacked | Packed | Notes |
|----------|--------|-------|
| S/2019 S 22 | SK19S220 | S=satellite, K=century, S=Saturn, 22=number |
| S/2003 J 2 | SJ03J020 | J=century (1900s), J=Jupiter |
| S/2018 U 1 | SK18U010 | U=Uranus |

## Base-62 Character Set

```
Index: 0123456789...
Char:  0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
Value: 0         ...9  10                  35 36                  61
```
