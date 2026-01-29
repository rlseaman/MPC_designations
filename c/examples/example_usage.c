/*
 * example_usage.c - Example usage of the MPC designation library
 *
 * Compile: cc -O2 -I../src -o example_usage example_usage.c ../src/mpc_designation.c
 */

#include <stdio.h>
#include "../src/mpc_designation.h"

/* Example 1: Simple conversion */
void example_simple_conversion(void) {
    printf("=== Simple Conversion ===\n");

    char output[MPC_MAX_UNPACKED];
    const char *designations[] = {
        "1995 XA",      /* Provisional asteroid */
        "J95X00A",      /* Packed provisional */
        "1",            /* Numbered asteroid */
        "C/1995 O1",    /* Comet */
        "S/2019 S 22",  /* Natural satellite */
        NULL
    };

    for (int i = 0; designations[i] != NULL; i++) {
        int err = mpc_convert_simple(designations[i], output, sizeof(output));
        if (err == MPC_OK) {
            printf("  %s -> %s\n", designations[i], output);
        } else {
            printf("  %s -> Error: %s\n", designations[i], mpc_strerror(err));
        }
    }
    printf("\n");
}

/* Example 2: Conversion with format information */
void example_with_info(void) {
    printf("=== Conversion with Info ===\n");

    const char *input = "1995 XA";
    char output[MPC_MAX_UNPACKED];
    mpc_info_t info;

    int err = mpc_convert(input, output, sizeof(output), &info);
    if (err == MPC_OK) {
        printf("  Input:   %s\n", input);
        printf("  Output:  %s\n", output);
        printf("  Format:  %s\n", info.format == MPC_FORMAT_PACKED ? "packed" : "unpacked");
        printf("  Type:    %s\n", info.subtype);
    }
    printf("\n");
}

/* Example 3: Error handling */
void example_error_handling(void) {
    printf("=== Error Handling ===\n");

    char output[MPC_MAX_UNPACKED];
    const char *invalid[] = {
        "invalid",      /* Not a valid designation */
        "",             /* Empty string */
        "99999999999",  /* Number too large */
        NULL
    };

    for (int i = 0; invalid[i] != NULL; i++) {
        int err = mpc_convert_simple(invalid[i], output, sizeof(output));
        if (err != MPC_OK) {
            printf("  '%s' -> %s (code %d)\n", invalid[i], mpc_strerror(err), err);
        }
    }
    printf("\n");
}

/* Example 4: Bidirectional conversion */
void example_bidirectional(void) {
    printf("=== Bidirectional Conversion ===\n");

    char packed[MPC_MAX_PACKED];
    char unpacked[MPC_MAX_UNPACKED];

    /* Pack */
    int err = mpc_convert_simple("2024 AB", packed, sizeof(packed));
    if (err == MPC_OK) {
        printf("  Pack:   '2024 AB' -> '%s'\n", packed);

        /* Unpack */
        err = mpc_convert_simple(packed, unpacked, sizeof(unpacked));
        if (err == MPC_OK) {
            printf("  Unpack: '%s' -> '%s'\n", packed, unpacked);
        }
    }
    printf("\n");
}

/* Example 5: Different designation types */
void example_designation_types(void) {
    printf("=== Designation Types ===\n");

    char output[MPC_MAX_UNPACKED];
    mpc_info_t info;

    struct {
        const char *input;
        const char *description;
    } examples[] = {
        {"1", "Numbered asteroid (Ceres)"},
        {"100001", "High numbered asteroid"},
        {"1995 XA", "Provisional asteroid"},
        {"2040 P-L", "Survey asteroid (Palomar-Leiden)"},
        {"1P", "Numbered comet (Halley)"},
        {"C/1995 O1", "Provisional comet (Hale-Bopp)"},
        {"D/1993 F2-B", "Comet fragment (Shoemaker-Levy 9)"},
        {"S/2019 S 22", "Natural satellite (Saturn moon)"},
        {NULL, NULL}
    };

    for (int i = 0; examples[i].input != NULL; i++) {
        int err = mpc_convert(examples[i].input, output, sizeof(output), &info);
        if (err == MPC_OK) {
            printf("  %-15s -> %-12s (%s)\n",
                   examples[i].input, output, examples[i].description);
        }
    }
    printf("\n");
}

int main(void) {
    printf("MPC Designation Library - Example Usage\n");
    printf("========================================\n\n");

    example_simple_conversion();
    example_with_info();
    example_error_handling();
    example_bidirectional();
    example_designation_types();

    return 0;
}
