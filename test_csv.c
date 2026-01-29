/*
 * test_csv.c - Test mpc_designation.c against CSV file of known conversions
 *
 * Compile: cc -O2 -o test_csv test_csv.c mpc_designation.c
 * Usage: ./test_csv <csv_file> [max_errors]
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mpc_designation.h"

#define MAX_LINE 256

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <csv_file> [max_errors]\n", argv[0]);
        return 1;
    }

    const char *csv_file = argv[1];
    int max_errors = (argc > 2) ? atoi(argv[2]) : 100;

    FILE *fp = fopen(csv_file, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open file: %s\n", csv_file);
        return 1;
    }

    /* Skip header line */
    char line[MAX_LINE];
    if (!fgets(line, sizeof(line), fp)) {
        fclose(fp);
        return 1;
    }

    long total = 0;
    long passed = 0;
    long failed = 0;
    int errors_shown = 0;

    /* Store first N errors for display */
    typedef struct {
        char input[32];
        char got[32];
        char expected[32];
    } error_t;
    error_t *errors = calloc(max_errors, sizeof(error_t));

    clock_t start = clock();

    while (fgets(line, sizeof(line), fp)) {
        total++;

        /* Remove newline */
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
        if (len > 1 && line[len-2] == '\r') line[len-2] = '\0';

        /* Parse CSV: unpacked,packed */
        char *comma = strchr(line, ',');
        if (!comma) continue;

        *comma = '\0';
        const char *unpacked = line;
        const char *expected_packed = comma + 1;

        char got_packed[MPC_MAX_PACKED];
        int err = mpc_convert_simple(unpacked, got_packed, sizeof(got_packed));

        if (err != MPC_OK) {
            failed++;
            if (errors_shown < max_errors) {
                strncpy(errors[errors_shown].input, unpacked, 31);
                snprintf(errors[errors_shown].got, 32, "ERROR: %s", mpc_strerror(err));
                strncpy(errors[errors_shown].expected, expected_packed, 31);
                errors_shown++;
            }
            continue;
        }

        if (strcmp(got_packed, expected_packed) != 0) {
            failed++;
            if (errors_shown < max_errors) {
                strncpy(errors[errors_shown].input, unpacked, 31);
                strncpy(errors[errors_shown].got, got_packed, 31);
                strncpy(errors[errors_shown].expected, expected_packed, 31);
                errors_shown++;
            }
        } else {
            passed++;
        }

        /* Progress indicator every 100,000 entries */
        if (total % 100000 == 0) {
            printf("Processed %ld entries...\n", total);
        }
    }

    fclose(fp);

    clock_t end = clock();
    double elapsed_ms = (double)(end - start) / CLOCKS_PER_SEC * 1000.0;

    printf("\n=== Test Results ===\n");
    printf("Total:  %ld\n", total);
    printf("Passed: %ld\n", passed);
    printf("Failed: %ld\n", failed);
    printf("Time:   %.0fms (%.1f entries/sec)\n", elapsed_ms, total * 1000.0 / elapsed_ms);
    printf("\n");

    if (failed > 0 && errors_shown > 0) {
        printf("=== First %d failures ===\n", errors_shown);
        printf("%-25s %-15s %-15s\n", "Input", "Got", "Expected");
        printf("------------------------------------------------------------\n");
        for (int i = 0; i < errors_shown; i++) {
            printf("%-25s %-15s %-15s\n", errors[i].input, errors[i].got, errors[i].expected);
        }
    }

    free(errors);
    return (failed == 0) ? 0 : 1;
}
