/*
 * test_errors.c - Test mpc_designation.c error handling
 *
 * Tests various classes of invalid input to ensure proper error detection.
 * Reads test cases from error_test_cases.csv.
 *
 * Compile: cc -O2 -I../src -o test_errors test_errors.c ../src/mpc_designation.c
 * Usage: ./test_errors [error_test_cases.csv]
 *
 * Or use: make test_errors (from c/ directory)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "../src/mpc_designation.h"

#define MAX_LINE 512
#define MAX_FIELD 256

/* Parse escape sequences in a string (modifies in place) */
static void unescape_string(char *s) {
    char *src = s;
    char *dst = s;

    while (*src) {
        if (*src == '\\' && *(src + 1)) {
            src++;
            switch (*src) {
                case 'n':  *dst++ = '\n'; src++; break;
                case 'r':  *dst++ = '\r'; src++; break;
                case 't':  *dst++ = '\t'; src++; break;
                case 'f':  *dst++ = '\f'; src++; break;
                case 'v':  *dst++ = '\v'; src++; break;
                case '0':  *dst++ = '\0'; src++; break;
                case '\\': *dst++ = '\\'; src++; break;
                case 'x':
                    /* Hex escape: \xNN */
                    if (isxdigit(src[1]) && isxdigit(src[2])) {
                        char hex[3] = {src[1], src[2], '\0'};
                        *dst++ = (char)strtol(hex, NULL, 16);
                        src += 3;
                    } else {
                        *dst++ = *src++;
                    }
                    break;
                default:
                    *dst++ = *src++;
            }
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';
}

/* Parse a CSV field, handling possible quotes */
static char *parse_csv_field(char **line, char *buffer, size_t bufsize) {
    char *src = *line;
    char *dst = buffer;
    size_t len = 0;

    /* Skip leading whitespace */
    while (*src && isspace(*src)) src++;

    if (*src == '"') {
        /* Quoted field */
        src++;
        while (*src && len < bufsize - 1) {
            if (*src == '"') {
                if (*(src + 1) == '"') {
                    *dst++ = '"';
                    src += 2;
                    len++;
                } else {
                    src++;
                    break;
                }
            } else {
                *dst++ = *src++;
                len++;
            }
        }
    } else {
        /* Unquoted field */
        while (*src && *src != ',' && len < bufsize - 1) {
            *dst++ = *src++;
            len++;
        }
    }

    *dst = '\0';

    /* Skip to next field */
    while (*src && *src != ',') src++;
    if (*src == ',') src++;

    *line = src;
    return buffer;
}

int main(int argc, char *argv[]) {
    const char *csv_file = (argc > 1) ? argv[1] : "error_test_cases.csv";

    FILE *fp = fopen(csv_file, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open file: %s\n", csv_file);
        return 1;
    }

    char line[MAX_LINE];
    int total = 0;
    int passed = 0;
    int failed = 0;

    printf("=== MPC Designation Error Tests ===\n\n");

    while (fgets(line, sizeof(line), fp)) {

        /* Remove newline */
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
        if (len > 1 && line[len-2] == '\r') line[len-2] = '\0';

        /* Skip empty lines and comments */
        if (line[0] == '\0' || line[0] == '#') continue;

        /* Skip header */
        if (strncmp(line, "category,", 9) == 0) continue;

        /* Parse CSV fields: category, subcategory, input, expected_error, description */
        char *ptr = line;
        char category[MAX_FIELD], subcategory[MAX_FIELD], input[MAX_FIELD];
        char expected_error[MAX_FIELD], description[MAX_FIELD];

        parse_csv_field(&ptr, category, sizeof(category));
        parse_csv_field(&ptr, subcategory, sizeof(subcategory));
        parse_csv_field(&ptr, input, sizeof(input));
        parse_csv_field(&ptr, expected_error, sizeof(expected_error));
        parse_csv_field(&ptr, description, sizeof(description));

        /* Unescape the input string */
        unescape_string(input);

        total++;

        /* Run the test */
        char output[MPC_MAX_PACKED];
        int err = mpc_convert_simple(input, output, sizeof(output));

        int test_passed = 0;

        if (strcmp(expected_error, "valid") == 0) {
            /* Expect success */
            if (err == MPC_OK) {
                test_passed = 1;
            } else {
                printf("FAIL [%s/%s]: '%s'\n", category, subcategory, description);
                printf("      Expected: valid conversion\n");
                printf("      Got:      %s\n", mpc_strerror(err));
                failed++;
            }
        } else {
            /* Expect error */
            if (err != MPC_OK) {
                test_passed = 1;
            } else {
                printf("FAIL [%s/%s]: '%s'\n", category, subcategory, description);
                printf("      Expected: error (%s)\n", expected_error);
                printf("      Got:      '%s' (success)\n", output);
                failed++;
            }
        }

        if (test_passed) {
            passed++;
        }
    }

    fclose(fp);

    printf("\n=== Error Test Results ===\n");
    printf("Total:  %d\n", total);
    printf("Passed: %d\n", passed);
    printf("Failed: %d\n", failed);

    return (failed == 0) ? 0 : 1;
}
