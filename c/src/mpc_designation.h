/*
 * mpc_designation.h - Convert between packed and unpacked MPC designations
 *
 * Based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 *
 * Supports asteroids, comets, and natural satellites.
 */

#ifndef MPC_DESIGNATION_H
#define MPC_DESIGNATION_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Error codes */
#define MPC_OK              0
#define MPC_ERR_INVALID    -1
#define MPC_ERR_RANGE      -2
#define MPC_ERR_FORMAT     -3
#define MPC_ERR_BUFFER     -4

/* Maximum buffer sizes */
#define MPC_MAX_PACKED      16
#define MPC_MAX_UNPACKED    32

/* Version - can be overridden by build process from root VERSION file */
#ifndef MPC_VERSION
#define MPC_VERSION         "1.0.0"
#endif

/* Designation format types */
typedef enum {
    MPC_FORMAT_UNKNOWN = 0,
    MPC_FORMAT_PACKED,
    MPC_FORMAT_UNPACKED
} mpc_format_t;

/* Designation types */
typedef enum {
    MPC_TYPE_UNKNOWN = 0,
    MPC_TYPE_PERMANENT,
    MPC_TYPE_PROVISIONAL,
    MPC_TYPE_PROVISIONAL_EXTENDED,
    MPC_TYPE_SURVEY,
    MPC_TYPE_COMET_NUMBERED,
    MPC_TYPE_COMET_PROVISIONAL,
    MPC_TYPE_COMET_FULL,
    MPC_TYPE_COMET_ANCIENT,
    MPC_TYPE_COMET_BCE,
    MPC_TYPE_SATELLITE
} mpc_type_t;

/* Detection result structure */
typedef struct {
    mpc_format_t format;
    mpc_type_t type;
    char subtype[64];
} mpc_info_t;

/*
 * Convert a designation between packed and unpacked formats.
 * Auto-detects the input format and converts to the other.
 *
 * Parameters:
 *   input  - Input designation string
 *   output - Buffer to receive converted designation
 *   outlen - Size of output buffer
 *   info   - Optional pointer to receive detection info (can be NULL)
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_convert(const char *input, char *output, size_t outlen, mpc_info_t *info);

/*
 * Convert a designation, returning only the result string.
 * Simplified interface for common use cases.
 *
 * Parameters:
 *   input  - Input designation string
 *   output - Buffer to receive converted designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_convert_simple(const char *input, char *output, size_t outlen);

/*
 * Detect the format and type of a designation.
 *
 * Parameters:
 *   input - Input designation string
 *   info  - Pointer to receive detection info
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if unrecognized
 */
int mpc_detect_format(const char *input, mpc_info_t *info);

/*
 * Pack a permanent (numbered) asteroid designation.
 *
 * Parameters:
 *   number - Asteroid number (1 to ~15 million)
 *   output - Buffer to receive packed designation (min 6 bytes)
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_pack_permanent(long number, char *output, size_t outlen);

/*
 * Unpack a permanent (numbered) asteroid designation.
 *
 * Parameters:
 *   packed - Packed designation string
 *   number - Pointer to receive asteroid number
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_unpack_permanent(const char *packed, long *number);

/*
 * Pack a provisional asteroid designation.
 *
 * Parameters:
 *   unpacked - Unpacked designation (e.g., "1995 XA" or "2024 AB12")
 *   output   - Buffer to receive packed designation (min 8 bytes)
 *   outlen   - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_pack_provisional(const char *unpacked, char *output, size_t outlen);

/*
 * Unpack a provisional asteroid designation.
 *
 * Parameters:
 *   packed - Packed designation string
 *   output - Buffer to receive unpacked designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_unpack_provisional(const char *packed, char *output, size_t outlen);

/*
 * Get a human-readable error message.
 *
 * Parameters:
 *   errcode - Error code returned by mpc_* functions
 *
 * Returns:
 *   Static string describing the error
 */
const char *mpc_strerror(int errcode);

/*
 * Get the library version string.
 *
 * Returns:
 *   Version string (e.g., "1.0.0")
 */
const char *mpc_version(void);

#ifdef __cplusplus
}
#endif

#endif /* MPC_DESIGNATION_H */
