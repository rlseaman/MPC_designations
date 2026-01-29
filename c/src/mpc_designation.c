/*
 * mpc_designation.c - Convert between packed and unpacked MPC designations
 *
 * Based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 */

#include "mpc_designation.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

/* Base-62 character set: 0-9, A-Z, a-z */
static const char BASE62_CHARS[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/* Century codes: index = century (10-21), value = letter code */
static const char CENTURY_TO_LETTER[] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L'
};

/* Comet types */
static const char COMET_TYPES[] = "PCDXAI";

/* Comet type descriptions */
static const char *COMET_TYPE_NAMES[] = {
    "periodic",      /* P */
    "non-periodic",  /* C */
    "defunct",       /* D */
    "uncertain",     /* X */
    "minor-planet",  /* A */
    "interstellar"   /* I */
};

/* Planet codes for satellites */
static const char SATELLITE_PLANETS[] = "JSUN";

/* Planet names for satellites */
static const char *SATELLITE_PLANET_NAMES[] = {
    "Jupiter",  /* J */
    "Saturn",   /* S */
    "Uranus",   /* U */
    "Neptune"   /* N */
};

/* Get comet type description */
static const char *get_comet_type_name(char type) {
    const char *p = strchr(COMET_TYPES, type);
    if (p) return COMET_TYPE_NAMES[p - COMET_TYPES];
    return "unknown";
}

/* Get planet name for satellite */
static const char *get_planet_name(char code) {
    const char *p = strchr(SATELLITE_PLANETS, code);
    if (p) return SATELLITE_PLANET_NAMES[p - SATELLITE_PLANETS];
    return "unknown";
}

/* ========================================================================= */
/* Utility functions                                                         */
/* ========================================================================= */

static int base62_to_num(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    if (c >= 'a' && c <= 'z') return c - 'a' + 36;
    return -1;
}

static char num_to_base62(int n) {
    if (n < 0 || n > 61) return '?';
    return BASE62_CHARS[n];
}

static long base62_string_to_num(const char *s, int len) {
    long result = 0;
    for (int i = 0; i < len; i++) {
        int val = base62_to_num(s[i]);
        if (val < 0) return -1;
        result = result * 62 + val;
    }
    return result;
}

static void num_to_base62_string(long num, char *out, int width) {
    for (int i = width - 1; i >= 0; i--) {
        out[i] = num_to_base62(num % 62);
        num /= 62;
    }
    out[width] = '\0';
}

static int century_to_code(int century) {
    if (century < 10 || century > 21) return -1;
    return CENTURY_TO_LETTER[century - 10];
}

static int code_to_century(char code) {
    if (code >= 'A' && code <= 'L') return code - 'A' + 10;
    return -1;
}

static int letter_to_position(char letter) {
    /* A=1, B=2, ..., H=8, J=9 (skip I), K=10, ... */
    if (letter < 'A' || letter > 'Z') return -1;
    int pos = letter - 'A' + 1;
    if (letter > 'I') pos--;
    return pos;
}

static char position_to_letter(int pos) {
    if (pos < 1 || pos > 24) return '?';
    if (pos >= 9) pos++;  /* Skip I */
    return 'A' + pos - 1;
}

static int decode_cycle_count(const char *encoded) {
    int tens, ones;
    char first = encoded[0];
    char second = encoded[1];

    if (first >= '0' && first <= '9') {
        tens = first - '0';
    } else if (first >= 'A' && first <= 'Z') {
        tens = first - 'A' + 10;
    } else if (first >= 'a' && first <= 'z') {
        tens = first - 'a' + 36;
    } else {
        return -1;
    }

    if (second < '0' || second > '9') return -1;
    ones = second - '0';

    return tens * 10 + ones;
}

static int encode_cycle_count(int count, char *out) {
    if (count < 0 || count >= 620) return MPC_ERR_RANGE;

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

    return MPC_OK;
}

static int is_comet_type(char c) {
    return strchr(COMET_TYPES, c) != NULL;
}

static int is_satellite_planet(char c) {
    return strchr(SATELLITE_PLANETS, c) != NULL;
}

static void trim(char *s) {
    char *start = s;
    while (*start && *start == ' ') start++;  /* Only trim spaces, not other whitespace */
    if (start != s) memmove(s, start, strlen(start) + 1);

    char *end = s + strlen(s) - 1;
    while (end >= s && *end == ' ') *end-- = '\0';
}

/*
 * Validate raw input string before any processing:
 * - Check for null bytes, tabs, and non-ASCII characters
 * - Must be called on the original input, not trimmed
 * Returns MPC_OK if valid, MPC_ERR_FORMAT if invalid
 */
static int validate_raw_input(const char *s) {
    for (const char *p = s; *p; p++) {
        unsigned char c = (unsigned char)*p;
        /* Reject non-printing characters except space */
        if (c < 32 || c > 126) {
            return MPC_ERR_FORMAT;
        }
    }
    return MPC_OK;
}

/*
 * Validate whitespace in a string:
 * - Reject non-printing characters (except space)
 * - Reject tabs
 * - Reject consecutive spaces
 * Returns MPC_OK if valid, MPC_ERR_FORMAT if invalid
 */
static int validate_whitespace(const char *s) {
    int prev_space = 0;
    for (const char *p = s; *p; p++) {
        unsigned char c = (unsigned char)*p;
        /* Reject non-printing characters except space */
        if (c < 32 || c > 126) {
            return MPC_ERR_FORMAT;
        }
        /* Reject consecutive spaces */
        if (c == ' ') {
            if (prev_space) return MPC_ERR_FORMAT;
            prev_space = 1;
        } else {
            prev_space = 0;
        }
    }
    return MPC_OK;
}

/*
 * Check if a character is a valid half-month letter (A-Y, excluding I)
 */
static int is_valid_half_month(char c) {
    return (c >= 'A' && c <= 'Y' && c != 'I');
}

/* ========================================================================= */
/* Permanent (numbered) asteroid designations                                */
/* ========================================================================= */

/* Maximum asteroid number: 620000 + 62^4 - 1 = 15396335 */
#define MPC_MAX_ASTEROID_NUMBER 15396335L

int mpc_pack_permanent(long number, char *output, size_t outlen) {
    if (number < 1 || number > MPC_MAX_ASTEROID_NUMBER) return MPC_ERR_RANGE;
    if (outlen < 6) return MPC_ERR_BUFFER;

    if (number < 100000) {
        snprintf(output, outlen, "%05ld", number);
    } else if (number < 620000) {
        int div = number / 10000;
        int mod = number % 10000;
        char letter;
        if (div < 36) {
            letter = 'A' + div - 10;
        } else {
            letter = 'a' + div - 36;
        }
        snprintf(output, outlen, "%c%04d", letter, mod);
    } else {
        long offset = number - 620000;
        output[0] = '~';
        num_to_base62_string(offset, output + 1, 4);
    }

    return MPC_OK;
}

int mpc_unpack_permanent(const char *packed, long *number) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    size_t len = strlen(buf);
    char first = buf[0];

    /* Tilde format (>= 620,000) */
    if (first == '~' && len == 5) {
        long offset = base62_string_to_num(buf + 1, 4);
        if (offset < 0) return MPC_ERR_FORMAT;
        *number = 620000 + offset;
        return MPC_OK;
    }

    if (len != 5) return MPC_ERR_FORMAT;

    if (first >= '0' && first <= '9') {
        /* Simple numeric format (< 100,000) */
        *number = atol(buf);
        return MPC_OK;
    } else if (first >= 'A' && first <= 'Z') {
        /* Extended format with uppercase (100,000 - 359,999) */
        int val = first - 'A' + 10;
        *number = val * 10000 + atol(buf + 1);
        return MPC_OK;
    } else if (first >= 'a' && first <= 'z') {
        /* Extended format with lowercase (360,000 - 619,999) */
        int val = first - 'a' + 36;
        *number = val * 10000 + atol(buf + 1);
        return MPC_OK;
    }

    return MPC_ERR_FORMAT;
}

/* ========================================================================= */
/* Provisional asteroid designations                                         */
/* ========================================================================= */

int mpc_pack_provisional(const char *unpacked, char *output, size_t outlen) {
    char buf[64];
    strncpy(buf, unpacked, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (outlen < 8) return MPC_ERR_BUFFER;

    /* Check for survey designations: "2040 P-L", "3138 T-1", etc. */
    int num;
    char survey[8];
    if (sscanf(buf, "%d %7s", &num, survey) == 2) {
        /* Survey number must be positive */
        if (num < 1) {
            return MPC_ERR_FORMAT;
        }
        if (strcmp(survey, "P-L") == 0) {
            snprintf(output, outlen, "PLS%04d", num);
            return MPC_OK;
        } else if (strcmp(survey, "T-1") == 0) {
            snprintf(output, outlen, "T1S%04d", num);
            return MPC_OK;
        } else if (strcmp(survey, "T-2") == 0) {
            snprintf(output, outlen, "T2S%04d", num);
            return MPC_OK;
        } else if (strcmp(survey, "T-3") == 0) {
            snprintf(output, outlen, "T3S%04d", num);
            return MPC_OK;
        }
    }

    /* Check for old-style designation: "A908 CJ" */
    char prefix;
    int century_digit, year_short;
    char half_month, second_letter;
    if (sscanf(buf, "%c%1d%2d %c%c", &prefix, &century_digit, &year_short, &half_month, &second_letter) == 5) {
        if ((prefix == 'A' || prefix == 'B') &&
            is_valid_half_month(half_month) && isupper(second_letter)) {
            char century_code;
            if (century_digit == 8) century_code = 'I';
            else if (century_digit == 9) century_code = 'J';
            else if (century_digit == 0) century_code = 'K';
            else return MPC_ERR_FORMAT;

            snprintf(output, outlen, "%c%02d%c00%c", century_code, year_short, half_month, second_letter);
            return MPC_OK;
        }
    }

    /* Standard provisional: "1995 XA" or "1995 XA12" */
    int year;
    char letters[32] = {0};
    if (sscanf(buf, "%4d %31s", &year, letters) != 2) {
        return MPC_ERR_FORMAT;
    }

    if (strlen(letters) < 2 || !isupper(letters[0]) || !isupper(letters[1])) {
        return MPC_ERR_FORMAT;
    }

    half_month = letters[0];
    second_letter = letters[1];

    /* Validate half-month letter (I is not used) */
    if (!is_valid_half_month(half_month)) {
        return MPC_ERR_FORMAT;
    }
    long order_num = 0;
    if (strlen(letters) > 2) {
        /* Validate that remaining chars are all digits */
        for (size_t i = 2; letters[i]; i++) {
            if (!isdigit(letters[i])) {
                return MPC_ERR_FORMAT;
            }
        }
        /* Check for cycle count overflow */
        char *endptr;
        order_num = strtol(letters + 2, &endptr, 10);
        if (*endptr != '\0' || order_num < 0 || order_num > 999999) {
            return MPC_ERR_RANGE;
        }
    }

    int century = year / 100;
    year_short = year % 100;

    int century_code_int = century_to_code(century);
    if (century_code_int < 0) return MPC_ERR_FORMAT;
    char century_code = (char)century_code_int;

    /* Check if we need extended format (cycle >= 620) */
    if (order_num >= 620) {
        int year_digit = year % 100;  /* Two-digit year encoded as base-62 */
        long base_sequence = (order_num - 620) * 25 + letter_to_position(second_letter) - 1;
        /* Maximum 4-digit base-62 value is 62^4 - 1 = 14776335 */
        if (base_sequence > 14776335L) {
            return MPC_ERR_RANGE;
        }
        char seq_encoded[5];
        num_to_base62_string((int)base_sequence, seq_encoded, 4);
        snprintf(output, outlen, "_%c%c%s", num_to_base62(year_digit), half_month, seq_encoded);
        return MPC_OK;
    }

    char order_encoded[4];
    if (encode_cycle_count((int)order_num, order_encoded) != MPC_OK) {
        return MPC_ERR_RANGE;
    }

    snprintf(output, outlen, "%c%02d%c%s%c", century_code, year_short, half_month, order_encoded, second_letter);
    return MPC_OK;
}

int mpc_unpack_provisional(const char *packed, char *output, size_t outlen) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    size_t len = strlen(buf);
    if (outlen < 16) return MPC_ERR_BUFFER;

    /* Check for survey designations first */
    if (len == 7) {
        if (strncmp(buf, "PLS", 3) == 0) {
            int num = atoi(buf + 3);
            snprintf(output, outlen, "%d P-L", num);
            return MPC_OK;
        } else if (strncmp(buf, "T1S", 3) == 0) {
            int num = atoi(buf + 3);
            snprintf(output, outlen, "%d T-1", num);
            return MPC_OK;
        } else if (strncmp(buf, "T2S", 3) == 0) {
            int num = atoi(buf + 3);
            snprintf(output, outlen, "%d T-2", num);
            return MPC_OK;
        } else if (strncmp(buf, "T3S", 3) == 0) {
            int num = atoi(buf + 3);
            snprintf(output, outlen, "%d T-3", num);
            return MPC_OK;
        }
    }

    /* Extended format with underscore */
    if (len == 7 && buf[0] == '_') {
        int year_digit = base62_to_num(buf[1]);
        char half_month = buf[2];
        long base_sequence = base62_string_to_num(buf + 3, 4);

        int cycle = 620 + base_sequence / 25;
        int letter_pos = (base_sequence % 25) + 1;
        char second_letter = position_to_letter(letter_pos);

        /* Assume 2020s decade */
        int year = 2020 + year_digit;
        if (year > 2029) year -= 10;

        snprintf(output, outlen, "%d %c%c%d", year, half_month, second_letter, cycle);
        return MPC_OK;
    }

    if (len != 7) return MPC_ERR_FORMAT;

    char century_code = buf[0];
    int century = code_to_century(century_code);
    if (century < 0) return MPC_ERR_FORMAT;

    char year_str[3] = {buf[1], buf[2], '\0'};
    int year_short = atoi(year_str);
    char half_month = buf[3];

    char order_str[3] = {buf[4], buf[5], '\0'};
    int order_num = decode_cycle_count(order_str);
    if (order_num < 0) return MPC_ERR_FORMAT;

    char second_letter = buf[6];

    int full_year = century * 100 + year_short;

    if (order_num == 0) {
        snprintf(output, outlen, "%d %c%c", full_year, half_month, second_letter);
    } else {
        snprintf(output, outlen, "%d %c%c%d", full_year, half_month, second_letter, order_num);
    }

    return MPC_OK;
}

/* ========================================================================= */
/* Comet provisional designations                                            */
/* ========================================================================= */

static int pack_comet_provisional(const char *unpacked, char *output, size_t outlen) {
    char buf[64];
    strncpy(buf, unpacked, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (outlen < 9) return MPC_ERR_BUFFER;

    int year;
    char half_month;
    int order_num;
    char fragment[4] = {0};

    /* Try parsing with fragment: "1995 O1-B" or "1930 J1-AA" */
    if (sscanf(buf, "%4d %c%d-%3s", &year, &half_month, &order_num, fragment) < 3) {
        /* Try without fragment: "1995 O1" */
        if (sscanf(buf, "%4d %c%d", &year, &half_month, &order_num) != 3) {
            return MPC_ERR_FORMAT;
        }
    }

    /* Comet order number must be positive */
    if (order_num < 1) {
        return MPC_ERR_FORMAT;
    }

    int century = year / 100;
    int year_short = year % 100;

    int century_code_int = century_to_code(century);
    if (century_code_int < 0) return MPC_ERR_FORMAT;
    char century_code = (char)century_code_int;

    char order_encoded[4];
    if (encode_cycle_count(order_num, order_encoded) != MPC_OK) {
        return MPC_ERR_RANGE;
    }

    /* Fragment encoding: "0" for none, lowercase letter(s) for fragment */
    /* Fragment must be 1-2 uppercase letters only */
    char fragment_code[4];
    if (fragment[0] == '\0') {
        strcpy(fragment_code, "0");
    } else {
        /* Validate fragment: must be 1-2 letters only */
        if (!isupper(fragment[0])) {
            return MPC_ERR_FORMAT;
        }
        if (fragment[1] && !isupper(fragment[1])) {
            return MPC_ERR_FORMAT;
        }
        if (fragment[2]) {
            /* Fragment too long (> 2 chars) */
            return MPC_ERR_FORMAT;
        }
        fragment_code[0] = tolower(fragment[0]);
        if (fragment[1]) {
            fragment_code[1] = tolower(fragment[1]);
            fragment_code[2] = '\0';
        } else {
            fragment_code[1] = '\0';
        }
    }

    snprintf(output, outlen, "%c%02d%c%s%s", century_code, year_short, half_month, order_encoded, fragment_code);
    return MPC_OK;
}

static int unpack_comet_provisional(const char *packed, char *output, size_t outlen) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    size_t len = strlen(buf);
    if (len != 7 && len != 8) return MPC_ERR_FORMAT;
    if (outlen < 20) return MPC_ERR_BUFFER;

    char century_code = buf[0];
    int century = code_to_century(century_code);
    if (century < 0) return MPC_ERR_FORMAT;

    char year_str[3] = {buf[1], buf[2], '\0'};
    int year_short = atoi(year_str);
    char half_month = buf[3];

    char order_str[3] = {buf[4], buf[5], '\0'};
    int order_num = decode_cycle_count(order_str);
    if (order_num < 0) return MPC_ERR_FORMAT;

    int full_year = century * 100 + year_short;

    /* Fragment: 1 char for 7-char format, 2 chars for 8-char format */
    char fragment[4] = {0};
    if (len == 7) {
        fragment[0] = buf[6];
    } else {
        fragment[0] = buf[6];
        fragment[1] = buf[7];
    }

    if (fragment[0] != '0') {
        char frag_upper[4];
        frag_upper[0] = toupper(fragment[0]);
        frag_upper[1] = fragment[1] ? toupper(fragment[1]) : '\0';
        frag_upper[2] = '\0';
        snprintf(output, outlen, "%d %c%d-%s", full_year, half_month, order_num, frag_upper);
    } else {
        snprintf(output, outlen, "%d %c%d", full_year, half_month, order_num);
    }

    return MPC_OK;
}

/* ========================================================================= */
/* Numbered comet designations                                               */
/* ========================================================================= */

static int pack_comet_numbered(const char *unpacked, char *output, size_t outlen) {
    char buf[32];
    strncpy(buf, unpacked, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (outlen < 6) return MPC_ERR_BUFFER;

    int number;
    char comet_type;

    if (sscanf(buf, "%d%c", &number, &comet_type) != 2) {
        return MPC_ERR_FORMAT;
    }

    if (comet_type != 'P' && comet_type != 'D') {
        return MPC_ERR_FORMAT;
    }

    if (number < 1 || number > 9999) {
        return MPC_ERR_RANGE;
    }

    snprintf(output, outlen, "%04d%c", number, comet_type);
    return MPC_OK;
}

static int unpack_comet_numbered(const char *packed, char *output, size_t outlen) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (strlen(buf) != 5) return MPC_ERR_FORMAT;
    if (outlen < 8) return MPC_ERR_BUFFER;

    char num_str[5] = {buf[0], buf[1], buf[2], buf[3], '\0'};
    int number = atoi(num_str);
    char comet_type = buf[4];

    if (comet_type != 'P' && comet_type != 'D') {
        return MPC_ERR_FORMAT;
    }

    snprintf(output, outlen, "%d%c", number, comet_type);
    return MPC_OK;
}

/* ========================================================================= */
/* Satellite designations                                                    */
/* ========================================================================= */

static int pack_satellite(const char *unpacked, char *output, size_t outlen) {
    char buf[32];
    strncpy(buf, unpacked, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (outlen < 9) return MPC_ERR_BUFFER;

    int year;
    char planet;
    int number;

    if (sscanf(buf, "S/%d %c %d", &year, &planet, &number) != 3) {
        return MPC_ERR_FORMAT;
    }

    if (!is_satellite_planet(planet)) {
        return MPC_ERR_FORMAT;
    }

    /* Satellite number must be positive */
    if (number < 1) {
        return MPC_ERR_FORMAT;
    }

    int century = year / 100;
    int year_short = year % 100;

    int century_code_int = century_to_code(century);
    if (century_code_int < 0) return MPC_ERR_FORMAT;
    char century_code = (char)century_code_int;

    char number_encoded[4];
    if (encode_cycle_count(number, number_encoded) != MPC_OK) {
        return MPC_ERR_RANGE;
    }

    snprintf(output, outlen, "S%c%02d%c%s0", century_code, year_short, planet, number_encoded);
    return MPC_OK;
}

static int unpack_satellite(const char *packed, char *output, size_t outlen) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (strlen(buf) != 8 || buf[0] != 'S') return MPC_ERR_FORMAT;
    if (outlen < 16) return MPC_ERR_BUFFER;

    char century_code = buf[1];
    int century = code_to_century(century_code);
    if (century < 0) return MPC_ERR_FORMAT;

    char year_str[3] = {buf[2], buf[3], '\0'};
    int year_short = atoi(year_str);
    char planet = buf[4];

    if (!is_satellite_planet(planet)) return MPC_ERR_FORMAT;

    char num_str[3] = {buf[5], buf[6], '\0'};
    int number = decode_cycle_count(num_str);
    if (number < 0) return MPC_ERR_FORMAT;

    int full_year = century * 100 + year_short;

    snprintf(output, outlen, "S/%d %c %d", full_year, planet, number);
    return MPC_OK;
}

/* ========================================================================= */
/* Ancient and BCE comet designations                                        */
/* ========================================================================= */

static int encode_bce_year(int year, char *prefix, char *code) {
    if (year >= 0) return MPC_ERR_RANGE;

    int abs_year = -year;
    int code_num = 99 - (abs_year % 100);

    if (abs_year < 100) {
        *prefix = '/';
    } else if (abs_year < 200) {
        *prefix = '.';
    } else if (abs_year < 300) {
        *prefix = '-';
    } else {
        return MPC_ERR_RANGE;
    }

    snprintf(code, 3, "%02d", code_num);
    return MPC_OK;
}

static int decode_bce_year(char prefix, const char *code) {
    int code_num = atoi(code);
    int year_part = 99 - code_num;

    if (prefix == '/') {
        return -year_part;
    } else if (prefix == '.') {
        return -(year_part + 100);
    } else if (prefix == '-') {
        return -(year_part + 200);
    }

    return 0;  /* Invalid */
}

static int pack_ancient_comet(const char *unpacked, char *output, size_t outlen) {
    char buf[64];
    strncpy(buf, unpacked, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (outlen < 9) return MPC_ERR_BUFFER;

    char comet_type;
    int year;
    char half_month;
    int order_num;
    char fragment[4] = {0};

    /* Parse: "C/240 V1" or "C/-146 P1" or "C/-146 P1-A" */
    if (sscanf(buf, "%c/%d %c%d-%3s", &comet_type, &year, &half_month, &order_num, fragment) < 4) {
        if (sscanf(buf, "%c/%d %c%d", &comet_type, &year, &half_month, &order_num) != 4) {
            return MPC_ERR_FORMAT;
        }
    }

    if (!is_comet_type(comet_type)) return MPC_ERR_FORMAT;

    /* Comet order number must be positive */
    if (order_num < 1) {
        return MPC_ERR_FORMAT;
    }

    char order_encoded[4];
    if (encode_cycle_count(order_num, order_encoded) != MPC_OK) {
        return MPC_ERR_RANGE;
    }

    char fragment_code = (fragment[0] == '\0') ? '0' : tolower(fragment[0]);

    if (year < 0) {
        /* BCE year */
        char prefix;
        char year_code[4];
        if (encode_bce_year(year, &prefix, year_code) != MPC_OK) {
            return MPC_ERR_RANGE;
        }
        snprintf(output, outlen, "%c%c%s%c%s%c", comet_type, prefix, year_code, half_month, order_encoded, fragment_code);
    } else {
        /* Ancient (1-999) */
        snprintf(output, outlen, "%c%03d%c%s%c", comet_type, year, half_month, order_encoded, fragment_code);
    }

    return MPC_OK;
}

static int unpack_ancient_comet(const char *packed, char *output, size_t outlen) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (strlen(buf) != 8) return MPC_ERR_FORMAT;
    if (outlen < 20) return MPC_ERR_BUFFER;

    char comet_type = buf[0];
    if (!is_comet_type(comet_type)) return MPC_ERR_FORMAT;

    int year;
    char half_month;
    int order_num;
    char fragment;

    /* Check for BCE prefix */
    if (buf[1] == '/' || buf[1] == '.' || buf[1] == '-') {
        char prefix = buf[1];
        char year_code[3] = {buf[2], buf[3], '\0'};
        year = decode_bce_year(prefix, year_code);
        half_month = buf[4];
        char order_str[3] = {buf[5], buf[6], '\0'};
        order_num = decode_cycle_count(order_str);
        fragment = buf[7];
    } else {
        /* Ancient year (3 digits) */
        char year_str[4] = {buf[1], buf[2], buf[3], '\0'};
        year = atoi(year_str);
        half_month = buf[4];
        char order_str[3] = {buf[5], buf[6], '\0'};
        order_num = decode_cycle_count(order_str);
        fragment = buf[7];
    }

    if (order_num < 0) return MPC_ERR_FORMAT;

    if (fragment != '0') {
        snprintf(output, outlen, "%c/%d %c%d-%c", comet_type, year, half_month, order_num, toupper(fragment));
    } else {
        snprintf(output, outlen, "%c/%d %c%d", comet_type, year, half_month, order_num);
    }

    return MPC_OK;
}

/* ========================================================================= */
/* Full comet designations                                                   */
/* ========================================================================= */

static int is_asteroid_style_packed(const char *provisional) {
    size_t len = strlen(provisional);
    if (len != 7) return 0;
    return isupper(provisional[6]);
}

static int is_asteroid_style_unpacked(const char *provisional) {
    /* Comet-style: "1995 O1" - half-month followed by digit */
    /* Asteroid-style: "2006 AH2" - half-month followed by letter */
    size_t len = strlen(provisional);
    if (len < 7) return 0;

    /* Find the space */
    const char *space = strchr(provisional, ' ');
    if (!space || strlen(space) < 3) return 0;

    /* Check character after half-month letter */
    char second_char = space[2];
    return isalpha(second_char);
}

static int pack_comet_full(const char *unpacked, char *output, size_t outlen) {
    char buf[64];
    strncpy(buf, unpacked, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    trim(buf);

    if (outlen < 9) return MPC_ERR_BUFFER;  /* Minimum for ancient comets */

    /* Parse: optional number, type, slash, year, provisional */
    int number = 0;
    char comet_type;
    int year;
    char prov_part[32];

    /* Try with number first: "1P/1982 U1" */
    int parsed = sscanf(buf, "%d%c/%d %31s", &number, &comet_type, &year, prov_part);
    if (parsed != 4) {
        /* Try without number: "C/1995 O1" */
        number = 0;
        parsed = sscanf(buf, "%c/%d %31s", &comet_type, &year, prov_part);
        if (parsed != 3) {
            return MPC_ERR_FORMAT;
        }
    }

    if (!is_comet_type(comet_type)) return MPC_ERR_FORMAT;

    /* Check for ancient or BCE year */
    if (year < 1000) {
        return pack_ancient_comet(buf, output, outlen);
    }

    /* Reconstruct provisional with year */
    char provisional[48];
    snprintf(provisional, sizeof(provisional), "%d %s", year, prov_part);

    /* Determine if asteroid-style or comet-style */
    char provisional_packed[16];
    int err;
    if (is_asteroid_style_unpacked(provisional)) {
        err = mpc_pack_provisional(provisional, provisional_packed, sizeof(provisional_packed));
    } else {
        err = pack_comet_provisional(provisional, provisional_packed, sizeof(provisional_packed));
    }
    if (err != MPC_OK) return err;

    if (number == 0) {
        /* No periodic number - compact format */
        snprintf(output, outlen, "%c%s", comet_type, provisional_packed);
    } else {
        /* Has periodic number - full 12-char format */
        if (number < 1 || number > 9999) return MPC_ERR_RANGE;
        snprintf(output, outlen, "%04d%c%s", number, comet_type, provisional_packed);
    }

    return MPC_OK;
}

static int unpack_comet_full(const char *packed, char *output, size_t outlen) {
    char buf[16];
    strncpy(buf, packed, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    /* Don't trim - we need leading spaces for 12-char format */

    size_t len = strlen(buf);
    if (outlen < 24) return MPC_ERR_BUFFER;

    char comet_type;
    char provisional_part[12];
    int number = 0;

    if (len == 8) {
        /* Compact 8-char format */
        comet_type = buf[0];
        strncpy(provisional_part, buf + 1, 7);
        provisional_part[7] = '\0';
    } else if (len == 9) {
        /* Compact 9-char format with 2-letter fragment */
        comet_type = buf[0];
        strncpy(provisional_part, buf + 1, 8);
        provisional_part[8] = '\0';
    } else if (len == 12 || (len < 12 && buf[0] == ' ')) {
        /* Full 12-char format - pad with leading spaces if needed */
        char padded[16];
        int pad = 12 - len;
        memset(padded, ' ', pad);
        strcpy(padded + pad, buf);

        char num_part[5] = {padded[0], padded[1], padded[2], padded[3], '\0'};
        comet_type = padded[4];
        strncpy(provisional_part, padded + 5, 7);
        provisional_part[7] = '\0';

        /* Parse number, trimming spaces */
        char *p = num_part;
        while (*p == ' ') p++;
        if (*p) number = atoi(p);
    } else {
        return MPC_ERR_FORMAT;
    }

    if (!is_comet_type(comet_type)) return MPC_ERR_FORMAT;

    /* Unpack provisional */
    char provisional[32];
    int err;
    if (is_asteroid_style_packed(provisional_part)) {
        err = mpc_unpack_provisional(provisional_part, provisional, sizeof(provisional));
    } else {
        err = unpack_comet_provisional(provisional_part, provisional, sizeof(provisional));
    }
    if (err != MPC_OK) return err;

    if (number == 0) {
        snprintf(output, outlen, "%c/%s", comet_type, provisional);
    } else {
        snprintf(output, outlen, "%d%c/%s", number, comet_type, provisional);
    }

    return MPC_OK;
}

/* ========================================================================= */
/* Format detection                                                          */
/* ========================================================================= */

int mpc_detect_format(const char *input, mpc_info_t *info) {
    char buf[64];
    strncpy(buf, input, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';

    info->format = MPC_FORMAT_UNKNOWN;
    info->type = MPC_TYPE_UNKNOWN;
    info->subtype[0] = '\0';

    /* Validate raw input for invalid characters (tabs, non-ASCII, etc.) */
    if (validate_raw_input(input) != MPC_OK) {
        return MPC_ERR_FORMAT;
    }

    size_t orig_len = strlen(input);

    /* Check for packed 12-char comet before trimming */
    if (orig_len == 12) {
        char c4 = input[4];
        if (is_comet_type(c4)) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_COMET_FULL;
            strcpy(info->subtype, "comet with provisional (12-char)");
            return MPC_OK;
        }
    }

    /* Check for packed 8-char comet */
    if (orig_len == 8 && is_comet_type(input[0]) &&
        input[1] >= 'A' && input[1] <= 'L') {
        info->format = MPC_FORMAT_PACKED;
        info->type = MPC_TYPE_COMET_FULL;
        strcpy(info->subtype, "comet with provisional (8-char)");
        return MPC_OK;
    }

    /* Check for packed 9-char comet with 2-letter fragment */
    if (orig_len == 9 && is_comet_type(input[0]) &&
        input[1] >= 'A' && input[1] <= 'L' &&
        islower(input[7]) && islower(input[8])) {
        info->format = MPC_FORMAT_PACKED;
        info->type = MPC_TYPE_COMET_FULL;
        strcpy(info->subtype, "comet with provisional (9-char, 2-letter fragment)");
        return MPC_OK;
    }

    /* Check for packed ancient comet */
    if (orig_len == 8 && is_comet_type(input[0]) &&
        isdigit(input[1]) && isdigit(input[2]) && isdigit(input[3])) {
        info->format = MPC_FORMAT_PACKED;
        info->type = MPC_TYPE_COMET_ANCIENT;
        strcpy(info->subtype, "comet ancient provisional");
        return MPC_OK;
    }

    /* Check for packed BCE comet: T + prefix + 2-digit code + half-month + 2-char order + fragment */
    /* e.g., "C.53P010" - must NOT contain space (which would indicate unpacked) */
    if (orig_len == 8 && is_comet_type(input[0]) &&
        (input[1] == '/' || input[1] == '.' || input[1] == '-') &&
        isdigit(input[2]) && isdigit(input[3]) &&  /* 2-digit year code */
        isupper(input[4]) &&                        /* half-month letter */
        strchr(input, ' ') == NULL) {               /* no space = packed format */
        info->format = MPC_FORMAT_PACKED;
        info->type = MPC_TYPE_COMET_BCE;
        strcpy(info->subtype, "comet BCE provisional");
        return MPC_OK;
    }

    trim(buf);
    size_t len = strlen(buf);

    /* Validate whitespace (no tabs, no consecutive spaces, printable only) */
    if (validate_whitespace(buf) != MPC_OK) {
        return MPC_ERR_FORMAT;
    }

    /* Check for packed satellite */
    if (len == 8 && buf[0] == 'S' && buf[1] >= 'A' && buf[1] <= 'L') {
        info->format = MPC_FORMAT_PACKED;
        info->type = MPC_TYPE_SATELLITE;
        char planet = buf[4];
        snprintf(info->subtype, sizeof(info->subtype), "natural satellite (%s)", get_planet_name(planet));
        return MPC_OK;
    }

    /* Check for packed permanent */
    if (len == 5) {
        if (buf[0] == '~') {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_PERMANENT;
            strcpy(info->subtype, "permanent numbered (tilde/base-62, >= 620000)");
            return MPC_OK;
        }
        int all_digits = 1;
        for (int i = 0; i < 5; i++) {
            if (!isdigit(buf[i])) { all_digits = 0; break; }
        }
        if (all_digits) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_PERMANENT;
            strcpy(info->subtype, "permanent numbered (5-digit, < 100000)");
            return MPC_OK;
        }
        if (isalpha(buf[0]) && isdigit(buf[1]) && isdigit(buf[2]) &&
            isdigit(buf[3]) && isdigit(buf[4])) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_PERMANENT;
            if (isupper(buf[0])) {
                strcpy(info->subtype, "permanent numbered (letter-prefix, 100000-359999)");
            } else {
                strcpy(info->subtype, "permanent numbered (letter-prefix, 360000-619999)");
            }
            return MPC_OK;
        }
        /* Check for packed numbered comet */
        if (isdigit(buf[0]) && isdigit(buf[1]) && isdigit(buf[2]) && isdigit(buf[3]) &&
            (buf[4] == 'P' || buf[4] == 'D')) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_COMET_NUMBERED;
            snprintf(info->subtype, sizeof(info->subtype), "comet numbered %s", get_comet_type_name(buf[4]));
            return MPC_OK;
        }
    }

    /* Check for packed provisional (7 chars) */
    if (len == 7) {
        if (buf[0] == '_') {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_PROVISIONAL_EXTENDED;
            strcpy(info->subtype, "provisional (extended, cycle >=620)");
            return MPC_OK;
        }
        /* Survey */
        if (strncmp(buf, "PLS", 3) == 0) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_SURVEY;
            strcpy(info->subtype, "survey (Palomar-Leiden)");
            return MPC_OK;
        }
        if (buf[0] == 'T' && buf[2] == 'S' && buf[1] >= '1' && buf[1] <= '3') {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_SURVEY;
            snprintf(info->subtype, sizeof(info->subtype), "survey (Trojan T-%c)", buf[1]);
            return MPC_OK;
        }
        /* Standard provisional */
        if (buf[0] >= 'A' && buf[0] <= 'L' && isdigit(buf[1]) && isdigit(buf[2]) &&
            isupper(buf[3]) && isupper(buf[6])) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_PROVISIONAL;
            strcpy(info->subtype, "provisional");
            return MPC_OK;
        }
        /* Comet provisional - note: packed comet provisional doesn't include type */
        if (buf[0] >= 'I' && buf[0] <= 'L' && isdigit(buf[1]) && isdigit(buf[2]) &&
            isupper(buf[3]) && (islower(buf[6]) || buf[6] == '0')) {
            info->format = MPC_FORMAT_PACKED;
            info->type = MPC_TYPE_COMET_PROVISIONAL;
            strcpy(info->subtype, "comet provisional");
            return MPC_OK;
        }
    }

    /* --- UNPACKED FORMATS --- */

    /* Check for unpacked satellite: S/YYYY P n */
    if (strncmp(buf, "S/", 2) == 0) {
        info->format = MPC_FORMAT_UNPACKED;
        info->type = MPC_TYPE_SATELLITE;
        /* Extract planet code (e.g., "S/2019 J 1" -> planet is at position 7) */
        char planet = '\0';
        int yr;
        char pl;
        if (sscanf(buf, "S/%d %c", &yr, &pl) == 2 && strchr(SATELLITE_PLANETS, pl)) {
            planet = pl;
        }
        if (planet) {
            snprintf(info->subtype, sizeof(info->subtype), "natural satellite (%s)", get_planet_name(planet));
        } else {
            strcpy(info->subtype, "natural satellite provisional");
        }
        return MPC_OK;
    }

    /* Check for unpacked permanent (all digits) */
    int all_digits = 1;
    for (size_t i = 0; i < len; i++) {
        if (!isdigit(buf[i])) { all_digits = 0; break; }
    }
    if (all_digits && len > 0) {
        info->format = MPC_FORMAT_UNPACKED;
        info->type = MPC_TYPE_PERMANENT;
        strcpy(info->subtype, "permanent numbered");
        return MPC_OK;
    }

    /* Check for unpacked survey - must have explicit space between number and code */
    int num;
    char survey[8];
    /* Find the space position - survey format is "NNNN P-L" */
    char *space_pos = strchr(buf, ' ');
    if (space_pos && space_pos > buf && sscanf(buf, "%d %7s", &num, survey) == 2) {
        if (strcmp(survey, "P-L") == 0) {
            info->format = MPC_FORMAT_UNPACKED;
            info->type = MPC_TYPE_SURVEY;
            strcpy(info->subtype, "survey (Palomar-Leiden)");
            return MPC_OK;
        }
        if (strcmp(survey, "T-1") == 0 || strcmp(survey, "T-2") == 0 ||
            strcmp(survey, "T-3") == 0) {
            info->format = MPC_FORMAT_UNPACKED;
            info->type = MPC_TYPE_SURVEY;
            snprintf(info->subtype, sizeof(info->subtype), "survey (Trojan %s)", survey);
            return MPC_OK;
        }
    }

    /* Check for old-style asteroid: "A908 CJ" - exactly 7 chars: Ayyy HH */
    char prefix;
    int cd, ys;
    char hm, sl;
    if (len == 7 && buf[4] == ' ' &&
        sscanf(buf, "%c%1d%2d %c%c", &prefix, &cd, &ys, &hm, &sl) == 5) {
        if ((prefix == 'A' || prefix == 'B') && isupper(hm) && isupper(sl) &&
            isdigit(buf[1]) && isdigit(buf[2]) && isdigit(buf[3])) {
            info->format = MPC_FORMAT_UNPACKED;
            info->type = MPC_TYPE_PROVISIONAL;
            strcpy(info->subtype, "provisional (old-style pre-1925)");
            return MPC_OK;
        }
    }

    /* Check for unpacked provisional asteroid: "1995 XA" or "2024 AB12" */
    /* Must have exactly one space after 4-digit year */
    int year;
    char letters[16];
    if (len >= 7 && isdigit(buf[0]) && isdigit(buf[1]) && isdigit(buf[2]) && isdigit(buf[3]) &&
        buf[4] == ' ' && isupper(buf[5]) &&
        sscanf(buf, "%4d %15s", &year, letters) == 2) {
        if (strlen(letters) >= 2 && isupper(letters[0]) && isupper(letters[1])) {
            /* Could be provisional asteroid */
            int has_only_digits_after = 1;
            for (size_t i = 2; i < strlen(letters); i++) {
                if (!isdigit(letters[i])) { has_only_digits_after = 0; break; }
            }
            if (has_only_digits_after) {
                info->format = MPC_FORMAT_UNPACKED;
                info->type = MPC_TYPE_PROVISIONAL;
                strcpy(info->subtype, "provisional");
                return MPC_OK;
            }
        }
    }

    /* Check for unpacked comet with type prefix */
    int comet_num = 0;
    char comet_type;
    char prov[32];

    /* Try with number: "1P/1982 U1" - must have space before provisional */
    if (strchr(buf, ' ') && sscanf(buf, "%d%c/%d %31s", &comet_num, &comet_type, &year, prov) == 4) {
        if (is_comet_type(comet_type)) {
            info->format = MPC_FORMAT_UNPACKED;
            info->type = MPC_TYPE_COMET_FULL;
            snprintf(info->subtype, sizeof(info->subtype), "comet numbered with provisional (%s)", get_comet_type_name(comet_type));
            return MPC_OK;
        }
    }

    /* Try without number: "C/1995 O1" - must have space before provisional */
    /* Find the space position to validate format */
    const char *comet_space_pos = strchr(buf, ' ');
    if (comet_space_pos && sscanf(buf, "%c/%d %31s", &comet_type, &year, prov) == 3) {
        if (is_comet_type(comet_type)) {
            info->format = MPC_FORMAT_UNPACKED;
            info->type = MPC_TYPE_COMET_FULL;
            if (year < 0) {
                snprintf(info->subtype, sizeof(info->subtype), "comet BCE provisional (%s)", get_comet_type_name(comet_type));
            } else if (year < 1000) {
                snprintf(info->subtype, sizeof(info->subtype), "comet ancient provisional (%s)", get_comet_type_name(comet_type));
            } else {
                snprintf(info->subtype, sizeof(info->subtype), "comet provisional (%s)", get_comet_type_name(comet_type));
            }
            return MPC_OK;
        }
    }

    /* Check for unpacked numbered comet: "1P" or "354P" - must be exactly digits followed by P/D */
    int chars_consumed = 0;
    if (sscanf(buf, "%d%c%n", &comet_num, &comet_type, &chars_consumed) == 2) {
        /* Ensure entire string was consumed (no trailing content) */
        if ((comet_type == 'P' || comet_type == 'D') && comet_num > 0 &&
            (size_t)chars_consumed == len) {
            info->format = MPC_FORMAT_UNPACKED;
            info->type = MPC_TYPE_COMET_NUMBERED;
            snprintf(info->subtype, sizeof(info->subtype), "comet numbered %s", get_comet_type_name(comet_type));
            return MPC_OK;
        }
    }

    return MPC_ERR_FORMAT;
}

/* ========================================================================= */
/* Main conversion function                                                  */
/* ========================================================================= */

int mpc_convert(const char *input, char *output, size_t outlen, mpc_info_t *info) {
    mpc_info_t local_info;
    if (info == NULL) info = &local_info;

    int err = mpc_detect_format(input, info);
    if (err != MPC_OK) return err;

    if (info->format == MPC_FORMAT_PACKED) {
        switch (info->type) {
            case MPC_TYPE_PERMANENT: {
                long number;
                err = mpc_unpack_permanent(input, &number);
                if (err != MPC_OK) return err;
                snprintf(output, outlen, "%ld", number);
                return MPC_OK;
            }
            case MPC_TYPE_PROVISIONAL:
            case MPC_TYPE_SURVEY:
            case MPC_TYPE_PROVISIONAL_EXTENDED:
                return mpc_unpack_provisional(input, output, outlen);
            case MPC_TYPE_COMET_NUMBERED:
                return unpack_comet_numbered(input, output, outlen);
            case MPC_TYPE_COMET_PROVISIONAL:
                return unpack_comet_provisional(input, output, outlen);
            case MPC_TYPE_COMET_FULL:
                return unpack_comet_full(input, output, outlen);
            case MPC_TYPE_COMET_ANCIENT:
            case MPC_TYPE_COMET_BCE:
                return unpack_ancient_comet(input, output, outlen);
            case MPC_TYPE_SATELLITE:
                return unpack_satellite(input, output, outlen);
            default:
                return MPC_ERR_FORMAT;
        }
    } else {
        switch (info->type) {
            case MPC_TYPE_PERMANENT: {
                long number = atol(input);
                return mpc_pack_permanent(number, output, outlen);
            }
            case MPC_TYPE_PROVISIONAL:
            case MPC_TYPE_SURVEY:
                return mpc_pack_provisional(input, output, outlen);
            case MPC_TYPE_COMET_NUMBERED:
                return pack_comet_numbered(input, output, outlen);
            case MPC_TYPE_COMET_FULL:
                return pack_comet_full(input, output, outlen);
            case MPC_TYPE_SATELLITE:
                return pack_satellite(input, output, outlen);
            default:
                return MPC_ERR_FORMAT;
        }
    }
}

int mpc_convert_simple(const char *input, char *output, size_t outlen) {
    return mpc_convert(input, output, outlen, NULL);
}

const char *mpc_strerror(int errcode) {
    switch (errcode) {
        case MPC_OK: return "Success";
        case MPC_ERR_INVALID: return "Invalid designation";
        case MPC_ERR_RANGE: return "Value out of range";
        case MPC_ERR_FORMAT: return "Unrecognized format";
        case MPC_ERR_BUFFER: return "Buffer too small";
        default: return "Unknown error";
    }
}

const char *mpc_version(void) {
    return MPC_VERSION;
}
