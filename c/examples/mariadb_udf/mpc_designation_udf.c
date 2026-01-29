/*
 * mpc_designation_udf.c - MariaDB/MySQL User Defined Functions for MPC designation conversion
 *
 * This plugin provides SQL functions for converting between packed and unpacked
 * MPC (Minor Planet Center) designation formats.
 *
 * Functions:
 *   mpc_convert(designation) - Convert between packed/unpacked formats
 *   mpc_detect(designation)  - Detect format ('packed' or 'unpacked')
 *   mpc_type(designation)    - Detect type (permanent, provisional, comet, etc.)
 *
 * Compile:
 *   gcc -shared -fPIC -o mpc_designation_udf.so mpc_designation_udf.c \
 *       ../../src/mpc_designation.c \
 *       $(mysql_config --cflags) \
 *       -I../../src
 *
 * Install:
 *   sudo cp mpc_designation_udf.so $(mysql_config --plugindir)/
 *
 * Load in MariaDB/MySQL:
 *   CREATE FUNCTION mpc_convert RETURNS STRING SONAME 'mpc_designation_udf.so';
 *   CREATE FUNCTION mpc_detect RETURNS STRING SONAME 'mpc_designation_udf.so';
 *   CREATE FUNCTION mpc_type RETURNS STRING SONAME 'mpc_designation_udf.so';
 *
 * Unload:
 *   DROP FUNCTION mpc_convert;
 *   DROP FUNCTION mpc_detect;
 *   DROP FUNCTION mpc_type;
 */

#include <mysql.h>
#include <string.h>
#include <stdlib.h>
#include "mpc_designation.h"

/* ============================================================================
 * mpc_convert(designation) -> converted designation
 * Auto-detects format and converts to the other
 * ============================================================================
 */

my_bool mpc_convert_init(UDF_INIT *initid, UDF_ARGS *args, char *message) {
    if (args->arg_count != 1) {
        strcpy(message, "mpc_convert() requires exactly one argument");
        return 1;
    }
    if (args->arg_type[0] != STRING_RESULT) {
        args->arg_type[0] = STRING_RESULT;
    }
    initid->max_length = MPC_MAX_UNPACKED;
    initid->maybe_null = 1;
    initid->ptr = (char *)malloc(MPC_MAX_UNPACKED);
    if (initid->ptr == NULL) {
        strcpy(message, "Could not allocate memory");
        return 1;
    }
    return 0;
}

void mpc_convert_deinit(UDF_INIT *initid) {
    if (initid->ptr) {
        free(initid->ptr);
        initid->ptr = NULL;
    }
}

char *mpc_convert(UDF_INIT *initid, UDF_ARGS *args, char *result,
                  unsigned long *length, char *is_null, char *error) {
    if (args->args[0] == NULL) {
        *is_null = 1;
        return NULL;
    }

    /* Create null-terminated copy of input */
    size_t input_len = args->lengths[0];
    if (input_len >= MPC_MAX_UNPACKED - 1) {
        *is_null = 1;
        return NULL;
    }

    char input[MPC_MAX_UNPACKED];
    memcpy(input, args->args[0], input_len);
    input[input_len] = '\0';

    char *output = initid->ptr;
    int rc = mpc_convert_simple(input, output, MPC_MAX_UNPACKED);

    if (rc != MPC_OK) {
        *is_null = 1;
        return NULL;
    }

    *length = strlen(output);
    *is_null = 0;
    return output;
}


/* ============================================================================
 * mpc_detect(designation) -> 'packed' or 'unpacked' or NULL
 * ============================================================================
 */

my_bool mpc_detect_init(UDF_INIT *initid, UDF_ARGS *args, char *message) {
    if (args->arg_count != 1) {
        strcpy(message, "mpc_detect() requires exactly one argument");
        return 1;
    }
    if (args->arg_type[0] != STRING_RESULT) {
        args->arg_type[0] = STRING_RESULT;
    }
    initid->max_length = 16;
    initid->maybe_null = 1;
    return 0;
}

void mpc_detect_deinit(UDF_INIT *initid) {
    /* Nothing to clean up */
}

char *mpc_detect(UDF_INIT *initid, UDF_ARGS *args, char *result,
                 unsigned long *length, char *is_null, char *error) {
    if (args->args[0] == NULL) {
        *is_null = 1;
        return NULL;
    }

    /* Create null-terminated copy of input */
    size_t input_len = args->lengths[0];
    if (input_len >= MPC_MAX_UNPACKED - 1) {
        *is_null = 1;
        return NULL;
    }

    char input[MPC_MAX_UNPACKED];
    memcpy(input, args->args[0], input_len);
    input[input_len] = '\0';

    mpc_info_t info;
    int rc = mpc_detect_format(input, &info);

    if (rc != MPC_OK) {
        *is_null = 1;
        return NULL;
    }

    if (info.format == MPC_FORMAT_PACKED) {
        strcpy(result, "packed");
        *length = 6;
    } else if (info.format == MPC_FORMAT_UNPACKED) {
        strcpy(result, "unpacked");
        *length = 8;
    } else {
        *is_null = 1;
        return NULL;
    }

    *is_null = 0;
    return result;
}


/* ============================================================================
 * mpc_type(designation) -> type string (permanent, provisional, etc.) or NULL
 * ============================================================================
 */

my_bool mpc_type_init(UDF_INIT *initid, UDF_ARGS *args, char *message) {
    if (args->arg_count != 1) {
        strcpy(message, "mpc_type() requires exactly one argument");
        return 1;
    }
    if (args->arg_type[0] != STRING_RESULT) {
        args->arg_type[0] = STRING_RESULT;
    }
    initid->max_length = 64;
    initid->maybe_null = 1;
    return 0;
}

void mpc_type_deinit(UDF_INIT *initid) {
    /* Nothing to clean up */
}

char *mpc_type(UDF_INIT *initid, UDF_ARGS *args, char *result,
               unsigned long *length, char *is_null, char *error) {
    if (args->args[0] == NULL) {
        *is_null = 1;
        return NULL;
    }

    /* Create null-terminated copy of input */
    size_t input_len = args->lengths[0];
    if (input_len >= MPC_MAX_UNPACKED - 1) {
        *is_null = 1;
        return NULL;
    }

    char input[MPC_MAX_UNPACKED];
    memcpy(input, args->args[0], input_len);
    input[input_len] = '\0';

    mpc_info_t info;
    int rc = mpc_detect_format(input, &info);

    if (rc != MPC_OK) {
        *is_null = 1;
        return NULL;
    }

    const char *type_name;
    switch (info.type) {
        case MPC_TYPE_PERMANENT:
            type_name = "permanent";
            break;
        case MPC_TYPE_PROVISIONAL:
            type_name = "provisional";
            break;
        case MPC_TYPE_PROVISIONAL_EXTENDED:
            type_name = "provisional_extended";
            break;
        case MPC_TYPE_SURVEY:
            type_name = "survey";
            break;
        case MPC_TYPE_COMET_NUMBERED:
            type_name = "comet_numbered";
            break;
        case MPC_TYPE_COMET_PROVISIONAL:
            type_name = "comet_provisional";
            break;
        case MPC_TYPE_COMET_FULL:
            type_name = "comet_full";
            break;
        case MPC_TYPE_COMET_ANCIENT:
            type_name = "comet_ancient";
            break;
        case MPC_TYPE_COMET_BCE:
            type_name = "comet_bce";
            break;
        case MPC_TYPE_SATELLITE:
            type_name = "satellite";
            break;
        default:
            *is_null = 1;
            return NULL;
    }

    strcpy(result, type_name);
    *length = strlen(type_name);
    *is_null = 0;
    return result;
}
