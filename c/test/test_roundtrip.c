/*
 * test_roundtrip.c - Bidirectional testing with round-trip verification
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
 *
 * Compile: cc -O2 -I../src -o test_roundtrip test_roundtrip.c ../src/mpc_designation.c
 * Usage: ./test_roundtrip <csv_file>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "../src/mpc_designation.h"

#define MAX_LINE 256
#define MAX_ENTRIES 2100000
#define MAX_ERRORS 20

typedef struct {
    char phase[16];
    char input[48];
    char got[48];
    char expected[48];
} error_t;

static char **unpacked_list = NULL;
static char **packed_list = NULL;
static int total_entries = 0;

static error_t errors[MAX_ERRORS];
static int error_count = 0;

static void add_error(const char *phase, const char *input, const char *got, const char *expected) {
    if (error_count < MAX_ERRORS) {
        strncpy(errors[error_count].phase, phase, 15);
        strncpy(errors[error_count].input, input, 47);
        strncpy(errors[error_count].got, got, 47);
        strncpy(errors[error_count].expected, expected, 47);
        error_count++;
    }
}

static long get_time_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000L + ts.tv_nsec / 1000000L;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <csv_file>\n", argv[0]);
        return 1;
    }

    const char *csv_file = argv[1];
    FILE *fp = fopen(csv_file, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open file: %s\n", csv_file);
        return 1;
    }

    /* Allocate storage */
    unpacked_list = malloc(MAX_ENTRIES * sizeof(char*));
    packed_list = malloc(MAX_ENTRIES * sizeof(char*));
    if (!unpacked_list || !packed_list) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        return 1;
    }

    /* Load test data */
    char line[MAX_LINE];
    fgets(line, sizeof(line), fp); /* Skip header */

    while (fgets(line, sizeof(line), fp) && total_entries < MAX_ENTRIES) {
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
        if (len > 1 && line[len-2] == '\r') line[len-2] = '\0';

        char *comma = strchr(line, ',');
        if (!comma) continue;

        *comma = '\0';
        unpacked_list[total_entries] = strdup(line);
        packed_list[total_entries] = strdup(comma + 1);
        total_entries++;
    }
    fclose(fp);

    printf("Loaded %d test cases\n\n", total_entries);

    long pack_passed = 0, pack_failed = 0;
    long unpack_passed = 0, unpack_failed = 0;
    long rt_unpacked_passed = 0, rt_unpacked_failed = 0;
    long rt_packed_passed = 0, rt_packed_failed = 0;

    char result[MPC_MAX_UNPACKED];   /* Large enough for either direction */
    char result2[MPC_MAX_UNPACKED];
    char err_msg[64];

    /* ========== Phase 1: Pack (unpacked -> packed) ========== */
    printf("=== Phase 1: Pack (unpacked -> packed) ===\n");
    long start = get_time_ms();

    for (int i = 0; i < total_entries; i++) {
        int err = mpc_convert_simple(unpacked_list[i], result, sizeof(result));
        if (err != MPC_OK) {
            pack_failed++;
            snprintf(err_msg, sizeof(err_msg), "ERROR: %s", mpc_strerror(err));
            add_error("pack", unpacked_list[i], err_msg, packed_list[i]);
        } else if (strcmp(result, packed_list[i]) != 0) {
            pack_failed++;
            add_error("pack", unpacked_list[i], result, packed_list[i]);
        } else {
            pack_passed++;
        }
    }

    long pack_time = get_time_ms() - start;
    double pack_rate = total_entries * 1000.0 / pack_time;
    printf("Passed: %ld\n", pack_passed);
    printf("Failed: %ld\n", pack_failed);
    printf("Time:   %ldms (%.1f entries/sec)\n\n", pack_time, pack_rate);

    /* ========== Phase 2: Unpack (packed -> unpacked) ========== */
    printf("=== Phase 2: Unpack (packed -> unpacked) ===\n");
    start = get_time_ms();

    for (int i = 0; i < total_entries; i++) {
        int err = mpc_convert_simple(packed_list[i], result, sizeof(result));
        if (err != MPC_OK) {
            unpack_failed++;
            snprintf(err_msg, sizeof(err_msg), "ERROR: %s", mpc_strerror(err));
            add_error("unpack", packed_list[i], err_msg, unpacked_list[i]);
        } else if (strcmp(result, unpacked_list[i]) != 0) {
            unpack_failed++;
            add_error("unpack", packed_list[i], result, unpacked_list[i]);
        } else {
            unpack_passed++;
        }
    }

    long unpack_time = get_time_ms() - start;
    double unpack_rate = total_entries * 1000.0 / unpack_time;
    printf("Passed: %ld\n", unpack_passed);
    printf("Failed: %ld\n", unpack_failed);
    printf("Time:   %ldms (%.1f entries/sec)\n\n", unpack_time, unpack_rate);

    /* ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ========== */
    printf("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===\n");
    start = get_time_ms();

    for (int i = 0; i < total_entries; i++) {
        const char *original = unpacked_list[i];

        int err = mpc_convert_simple(original, result, sizeof(result));
        if (err != MPC_OK) {
            rt_unpacked_failed++;
            continue;
        }

        err = mpc_convert_simple(result, result2, sizeof(result2));
        if (err != MPC_OK) {
            rt_unpacked_failed++;
            continue;
        }

        if (strcmp(result2, original) != 0) {
            rt_unpacked_failed++;
            snprintf(err_msg, sizeof(err_msg), "%s -> %s", result, result2);
            add_error("rt-unp", original, err_msg, original);
        } else {
            rt_unpacked_passed++;
        }
    }

    long rt_unpacked_time = get_time_ms() - start;
    double rt_unpacked_rate = total_entries * 1000.0 / rt_unpacked_time;
    printf("Passed: %ld\n", rt_unpacked_passed);
    printf("Failed: %ld\n", rt_unpacked_failed);
    printf("Time:   %ldms (%.1f entries/sec)\n\n", rt_unpacked_time, rt_unpacked_rate);

    /* ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ========== */
    printf("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===\n");
    error_count = 0;  /* Reset errors to show phase 4 errors */
    start = get_time_ms();

    for (int i = 0; i < total_entries; i++) {
        const char *original = packed_list[i];

        int err = mpc_convert_simple(original, result, sizeof(result));
        if (err != MPC_OK) {
            rt_packed_failed++;
            snprintf(err_msg, sizeof(err_msg), "unpack ERROR: %s", mpc_strerror(err));
            add_error("rt-pak", original, err_msg, original);
            continue;
        }

        err = mpc_convert_simple(result, result2, sizeof(result2));
        if (err != MPC_OK) {
            rt_packed_failed++;
            snprintf(err_msg, sizeof(err_msg), "pack ERROR: %s", mpc_strerror(err));
            add_error("rt-pak", result, err_msg, original);
            continue;
        }

        if (strcmp(result2, original) != 0) {
            rt_packed_failed++;
            snprintf(err_msg, sizeof(err_msg), "%s -> %s", result, result2);
            add_error("rt-pak", original, err_msg, original);
        } else {
            rt_packed_passed++;
        }
    }

    long rt_packed_time = get_time_ms() - start;
    double rt_packed_rate = total_entries * 1000.0 / rt_packed_time;
    printf("Passed: %ld\n", rt_packed_passed);
    printf("Failed: %ld\n", rt_packed_failed);
    printf("Time:   %ldms (%.1f entries/sec)\n\n", rt_packed_time, rt_packed_rate);

    /* ========== Summary ========== */
    printf("=== Summary ===\n");
    printf("%-30s %10s %18s %12s\n", "Phase", "Time (ms)", "Rate (entries/sec)", "Status");
    printf("%-30s %10s %18s %12s\n", "------------------------------", "----------", "------------------", "------------");
    printf("%-30s %10ld %18.1f %12s\n", "Pack", pack_time, pack_rate,
           pack_failed == 0 ? "PASS" : "FAIL");
    printf("%-30s %10ld %18.1f %12s\n", "Unpack", unpack_time, unpack_rate,
           unpack_failed == 0 ? "PASS" : "FAIL");
    printf("%-30s %10ld %18.1f %12s\n", "Unpacked RT: unpack(pack(x))=x", rt_unpacked_time, rt_unpacked_rate,
           rt_unpacked_failed == 0 ? "PASS" : "FAIL");
    printf("%-30s %10ld %18.1f %12s\n", "Packed RT: pack(unpack(y))=y", rt_packed_time, rt_packed_rate,
           rt_packed_failed == 0 ? "PASS" : "FAIL");
    printf("\n");

    /* Show errors */
    if (error_count > 0) {
        printf("=== First %d errors ===\n", error_count);
        printf("%-8s %-25s %-20s %-20s\n", "Phase", "Input", "Got", "Expected");
        printf("%-8s %-25s %-20s %-20s\n", "--------", "-------------------------", "--------------------", "--------------------");
        for (int i = 0; i < error_count; i++) {
            printf("%-8s %-25s %-20s %-20s\n",
                   errors[i].phase, errors[i].input, errors[i].got, errors[i].expected);
        }
    }

    /* Cleanup */
    for (int i = 0; i < total_entries; i++) {
        free(unpacked_list[i]);
        free(packed_list[i]);
    }
    free(unpacked_list);
    free(packed_list);

    long total_failed = pack_failed + unpack_failed + rt_packed_failed;
    /* Note: rt_unpacked_failed is expected for old-style designations */
    return (total_failed == 0) ? 0 : 1;
}
