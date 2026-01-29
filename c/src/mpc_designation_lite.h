/*
 * mpc_designation_lite.h - Simplified MPC designation converter (header-only)
 *
 * This is a lightweight, header-only implementation that handles the most
 * common cases:
 *   - Numbered asteroids: "1" <-> "00001"
 *   - Provisional asteroids: "1995 XA" <-> "J95X00A"
 *
 * For comets, satellites, and extended formats, use the full mpc_designation.h
 *
 * Usage:
 *   #define MPC_LITE_IMPLEMENTATION  // in ONE .c file only
 *   #include "mpc_designation_lite.h"
 *
 * Example:
 *   char output[16];
 *   mpc_lite_pack("1995 XA", output, sizeof(output));    // -> "J95X00A"
 *   mpc_lite_unpack("J95X00A", output, sizeof(output));  // -> "1995 XA"
 */

#ifndef MPC_DESIGNATION_LITE_H
#define MPC_DESIGNATION_LITE_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Error codes */
#define MPC_LITE_OK           0
#define MPC_LITE_ERR_FORMAT  -1
#define MPC_LITE_ERR_RANGE   -2
#define MPC_LITE_ERR_BUFFER  -3

/*
 * Pack a designation (ensure packed format).
 * If already packed, copies as-is.
 */
int mpc_lite_pack(const char *input, char *output, size_t outlen);

/*
 * Unpack a designation (ensure unpacked format).
 * If already unpacked, copies as-is.
 */
int mpc_lite_unpack(const char *input, char *output, size_t outlen);

/*
 * Convert between formats (auto-detect and flip).
 */
int mpc_lite_convert(const char *input, char *output, size_t outlen);

/*
 * Check if input is in packed format.
 * Returns 1 if packed, 0 otherwise.
 */
int mpc_lite_is_packed(const char *input);

/*
 * Check if input is in unpacked format.
 * Returns 1 if unpacked, 0 otherwise.
 */
int mpc_lite_is_unpacked(const char *input);

#ifdef __cplusplus
}
#endif

/* ========================================================================= */
/* IMPLEMENTATION                                                            */
/* ========================================================================= */

#ifdef MPC_LITE_IMPLEMENTATION

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

static void mpc_lite_trim(char *s) {
    char *start = s;
    while (*start && isspace(*start)) start++;
    if (start != s) memmove(s, start, strlen(start) + 1);
    size_t len = strlen(s);
    while (len > 0 && isspace(s[len - 1])) s[--len] = '\0';
}

static int mpc_lite_decode_cycle(const char *enc) {
    int tens, ones;
    if (isdigit(enc[0])) {
        tens = enc[0] - '0';
    } else if (isupper(enc[0])) {
        tens = enc[0] - 'A' + 10;
    } else {
        tens = enc[0] - 'a' + 36;
    }
    ones = enc[1] - '0';
    return tens * 10 + ones;
}

static void mpc_lite_encode_cycle(int count, char *out) {
    int tens = count / 10;
    int ones = count % 10;
    if (tens < 10) {
        out[0] = '0' + tens;
    } else if (tens < 36) {
        out[0] = 'A' + tens - 10;
    } else {
        out[0] = 'a' + tens - 36;
    }
    out[1] = '0' + ones;
    out[2] = '\0';
}

static int mpc_lite_century_to_letter(int century) {
    switch (century) {
        case 18: return 'I';
        case 19: return 'J';
        case 20: return 'K';
        case 21: return 'L';
        default: return -1;
    }
}

static int mpc_lite_letter_to_century(char letter) {
    switch (letter) {
        case 'I': return 18;
        case 'J': return 19;
        case 'K': return 20;
        case 'L': return 21;
        default: return -1;
    }
}

int mpc_lite_is_packed(const char *input) {
    char buf[32];
    strncpy(buf, input, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    mpc_lite_trim(buf);
    size_t len = strlen(buf);

    /* Packed permanent: 5 digits or letter + 4 digits */
    if (len == 5) {
        int all_digit = 1;
        for (int i = 0; i < 5; i++) {
            if (!isdigit(buf[i])) { all_digit = 0; break; }
        }
        if (all_digit) return 1;
        if (isalpha(buf[0]) && isdigit(buf[1]) && isdigit(buf[2]) &&
            isdigit(buf[3]) && isdigit(buf[4])) return 1;
    }

    /* Packed provisional: 7 chars starting with I/J/K/L */
    if (len == 7 && mpc_lite_letter_to_century(buf[0]) >= 0) {
        return 1;
    }

    return 0;
}

int mpc_lite_is_unpacked(const char *input) {
    char buf[32];
    strncpy(buf, input, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    mpc_lite_trim(buf);
    size_t len = strlen(buf);

    /* All digits = unpacked permanent */
    int all_digit = 1;
    for (size_t i = 0; i < len; i++) {
        if (!isdigit(buf[i])) { all_digit = 0; break; }
    }
    if (all_digit && len > 0) return 1;

    /* YYYY LL or YYYY LLnnn */
    if (len >= 7 && isdigit(buf[0]) && isdigit(buf[1]) &&
        isdigit(buf[2]) && isdigit(buf[3]) && buf[4] == ' ' &&
        isupper(buf[5]) && isupper(buf[6])) {
        /* Check rest are digits */
        for (size_t i = 7; i < len; i++) {
            if (!isdigit(buf[i])) return 0;
        }
        return 1;
    }

    return 0;
}

int mpc_lite_pack(const char *input, char *output, size_t outlen) {
    char buf[32];
    strncpy(buf, input, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    mpc_lite_trim(buf);

    if (outlen < 8) return MPC_LITE_ERR_BUFFER;

    /* Already packed? */
    if (mpc_lite_is_packed(buf)) {
        strcpy(output, buf);
        return MPC_LITE_OK;
    }

    /* Unpacked permanent (all digits) */
    int all_digit = 1;
    size_t len = strlen(buf);
    for (size_t i = 0; i < len; i++) {
        if (!isdigit(buf[i])) { all_digit = 0; break; }
    }
    if (all_digit && len > 0) {
        long num = atol(buf);
        if (num < 1 || num > 619999) return MPC_LITE_ERR_RANGE;
        if (num < 100000) {
            snprintf(output, outlen, "%05ld", num);
        } else {
            int div = num / 10000;
            int mod = num % 10000;
            char letter;
            if (div < 36) {
                letter = 'A' + div - 10;
            } else {
                letter = 'a' + div - 36;
            }
            snprintf(output, outlen, "%c%04d", letter, mod);
        }
        return MPC_LITE_OK;
    }

    /* Unpacked provisional: YYYY LL or YYYY LLnnn */
    int year;
    char half_month, second_letter;
    int order = 0;
    char rest[16] = {0};

    if (sscanf(buf, "%4d %c%c%15s", &year, &half_month, &second_letter, rest) >= 3) {
        if (rest[0]) order = atoi(rest);
        if (order >= 620) return MPC_LITE_ERR_RANGE;

        int century = year / 100;
        int letter_code = mpc_lite_century_to_letter(century);
        if (letter_code < 0) return MPC_LITE_ERR_RANGE;

        char cycle[3];
        mpc_lite_encode_cycle(order, cycle);

        snprintf(output, outlen, "%c%02d%c%s%c",
                 letter_code, year % 100, half_month, cycle, second_letter);
        return MPC_LITE_OK;
    }

    return MPC_LITE_ERR_FORMAT;
}

int mpc_lite_unpack(const char *input, char *output, size_t outlen) {
    char buf[32];
    strncpy(buf, input, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    mpc_lite_trim(buf);

    if (outlen < 16) return MPC_LITE_ERR_BUFFER;

    /* Already unpacked? */
    if (mpc_lite_is_unpacked(buf)) {
        /* Remove leading zeros for permanent */
        int all_digit = 1;
        for (size_t i = 0; buf[i]; i++) {
            if (!isdigit(buf[i])) { all_digit = 0; break; }
        }
        if (all_digit) {
            snprintf(output, outlen, "%ld", atol(buf));
        } else {
            strcpy(output, buf);
        }
        return MPC_LITE_OK;
    }

    size_t len = strlen(buf);

    /* Packed permanent (5 chars) */
    if (len == 5) {
        int all_digit = 1;
        for (int i = 0; i < 5; i++) {
            if (!isdigit(buf[i])) { all_digit = 0; break; }
        }
        if (all_digit) {
            snprintf(output, outlen, "%ld", atol(buf));
            return MPC_LITE_OK;
        }
        if (isalpha(buf[0]) && isdigit(buf[1])) {
            int val;
            if (isupper(buf[0])) {
                val = buf[0] - 'A' + 10;
            } else {
                val = buf[0] - 'a' + 36;
            }
            long num = val * 10000L + atoi(buf + 1);
            snprintf(output, outlen, "%ld", num);
            return MPC_LITE_OK;
        }
    }

    /* Packed provisional (7 chars) */
    if (len == 7) {
        int century = mpc_lite_letter_to_century(buf[0]);
        if (century >= 0) {
            int year_short = (buf[1] - '0') * 10 + (buf[2] - '0');
            int year = century * 100 + year_short;
            char half_month = buf[3];
            int order = mpc_lite_decode_cycle(buf + 4);
            char second_letter = buf[6];

            if (order == 0) {
                snprintf(output, outlen, "%d %c%c", year, half_month, second_letter);
            } else {
                snprintf(output, outlen, "%d %c%c%d", year, half_month, second_letter, order);
            }
            return MPC_LITE_OK;
        }
    }

    return MPC_LITE_ERR_FORMAT;
}

int mpc_lite_convert(const char *input, char *output, size_t outlen) {
    if (mpc_lite_is_packed(input)) {
        return mpc_lite_unpack(input, output, outlen);
    }
    if (mpc_lite_is_unpacked(input)) {
        return mpc_lite_pack(input, output, outlen);
    }
    return MPC_LITE_ERR_FORMAT;
}

#endif /* MPC_LITE_IMPLEMENTATION */

#endif /* MPC_DESIGNATION_LITE_H */
