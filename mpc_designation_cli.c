/*
 * mpc_designation_cli.c - Command-line interface for MPC designation converter
 *
 * Compile: cc -O2 -o mpc_designation_c mpc_designation_cli.c mpc_designation.c
 * Usage: ./mpc_designation_c [-v] <designation> [designation ...]
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpc_designation.h"

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [-v|--verbose] <designation> [designation ...]\n", prog);
    fprintf(stderr, "\nConvert between packed and unpacked MPC designations.\n");
    fprintf(stderr, "Auto-detects the input format and converts to the other.\n");
    fprintf(stderr, "\nOptions:\n");
    fprintf(stderr, "  -v, --verbose   Show detailed information about the conversion\n");
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }

    int verbose = 0;
    int start_arg = 1;

    /* Check for flags */
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            verbose = 1;
            start_arg = i + 1;
        } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else {
            break;
        }
    }

    int num_designations = argc - start_arg;
    if (num_designations == 0) {
        print_usage(argv[0]);
        return 1;
    }

    int multiple = (num_designations > 1);

    for (int i = start_arg; i < argc; i++) {
        const char *des = argv[i];
        char output[MPC_MAX_UNPACKED];
        mpc_info_t info;

        int err = mpc_convert(des, output, sizeof(output), &info);

        if (err != MPC_OK) {
            fprintf(stderr, "Error: %s: %s\n", des, mpc_strerror(err));
            return 1;
        }

        if (verbose) {
            printf("  Input:    %s\n", des);
            printf("  Detected: %s format, %s\n",
                   info.format == MPC_FORMAT_PACKED ? "packed" : "unpacked",
                   info.subtype);
            printf("  Action:   %s\n",
                   info.format == MPC_FORMAT_PACKED ?
                   "unpacking to human-readable form" :
                   "packing to MPC compact form");
            printf("  Output:   %s\n", output);
            if (multiple && i < argc - 1) printf("\n");
        } else if (multiple) {
            printf("%s -> %s\n", des, output);
        } else {
            printf("%s\n", output);
        }
    }

    return 0;
}
