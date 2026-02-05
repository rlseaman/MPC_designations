/*
 * mpc_designation.h - Convert between packed and unpacked MPC designations
 *
 * Based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 *
 * Supports asteroids, comets, and natural satellites.
 *
 * =============================================================================
 * API OVERVIEW
 * =============================================================================
 *
 * High-level (auto-detect type, idempotent):
 *   mpc_pack()           - Ensure packed format
 *   mpc_unpack()         - Ensure unpacked format
 *   mpc_convert_simple() - Flip between formats
 *
 * Category-specific (for known input types):
 *   mpc_pack_asteroid()   / mpc_unpack_asteroid()
 *   mpc_pack_comet()      / mpc_unpack_comet()
 *   mpc_pack_satellite()  / mpc_unpack_satellite()
 *
 * Validation:
 *   mpc_is_valid()         - Check if valid (no error output)
 *   mpc_is_valid_chars()   - Check character set only
 *   mpc_detect_format()    - Full format detection
 *
 * Low-level (for advanced use):
 *   mpc_pack_permanent()   / mpc_unpack_permanent()
 *   mpc_pack_provisional() / mpc_unpack_provisional()
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

/* =============================================================================
 * HIGH-LEVEL FUNCTIONS
 * ============================================================================= */

/*
 * Ensure a designation is in packed format.
 * If already packed, copies as-is (after validation).
 * If unpacked, converts to packed format.
 *
 * Parameters:
 *   input  - Input designation (packed or unpacked)
 *   output - Buffer to receive packed designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_pack(const char *input, char *output, size_t outlen);

/*
 * Ensure a designation is in unpacked (human-readable) format.
 * If already unpacked, copies as-is (after validation).
 * If packed, converts to unpacked format.
 *
 * Parameters:
 *   input  - Input designation (packed or unpacked)
 *   output - Buffer to receive unpacked designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_unpack(const char *input, char *output, size_t outlen);

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

/* =============================================================================
 * CATEGORY-SPECIFIC FUNCTIONS
 * ============================================================================= */

/*
 * Pack an asteroid designation (permanent or provisional).
 * Use when you know the input is an asteroid (not comet/satellite).
 *
 * Parameters:
 *   input  - Asteroid designation (e.g., "1", "1995 XA", "00001", "J95X00A")
 *   output - Buffer to receive packed designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if not an asteroid
 */
int mpc_pack_asteroid(const char *input, char *output, size_t outlen);

/*
 * Unpack an asteroid designation (permanent or provisional).
 *
 * Parameters:
 *   input  - Packed asteroid designation
 *   output - Buffer to receive unpacked designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if not an asteroid
 */
int mpc_unpack_asteroid(const char *input, char *output, size_t outlen);

/*
 * Pack a comet designation (numbered, provisional, or full).
 *
 * Parameters:
 *   input  - Comet designation (e.g., "1P", "C/1995 O1")
 *   output - Buffer to receive packed designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if not a comet
 */
int mpc_pack_comet(const char *input, char *output, size_t outlen);

/*
 * Unpack a comet designation.
 *
 * Parameters:
 *   input  - Packed comet designation
 *   output - Buffer to receive unpacked designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if not a comet
 */
int mpc_unpack_comet(const char *input, char *output, size_t outlen);

/*
 * Pack a satellite designation.
 *
 * Parameters:
 *   input  - Satellite designation (e.g., "S/2019 S 22")
 *   output - Buffer to receive packed designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if not a satellite
 */
int mpc_pack_satellite(const char *input, char *output, size_t outlen);

/*
 * Unpack a satellite designation.
 *
 * Parameters:
 *   input  - Packed satellite designation
 *   output - Buffer to receive unpacked designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, MPC_ERR_FORMAT if not a satellite
 */
int mpc_unpack_satellite(const char *input, char *output, size_t outlen);

/* =============================================================================
 * VALIDATION FUNCTIONS
 * ============================================================================= */

/*
 * Check if a string is a valid MPC designation.
 * This function never produces error output - just returns 0 or 1.
 *
 * Parameters:
 *   input - String to validate
 *
 * Returns:
 *   1 if valid, 0 if invalid
 */
int mpc_is_valid(const char *input);

/*
 * Check if a string contains only valid MPC designation characters.
 * Valid characters are: A-Z, a-z, 0-9, space, /, -, ~, _, .
 * This is a fast pre-check before attempting conversion.
 *
 * Parameters:
 *   input - String to check
 *
 * Returns:
 *   1 if all characters are valid, 0 otherwise
 */
int mpc_is_valid_chars(const char *input);

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

/* =============================================================================
 * LOW-LEVEL FUNCTIONS
 * ============================================================================= */

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

/* =============================================================================
 * FORMAT CONVERSION FUNCTIONS
 * ============================================================================= */

/*
 * MPC 12-character report format buffer size.
 * The report format is always exactly 12 characters (plus null terminator).
 */
#define MPC_REPORT_FORMAT_SIZE 13

/*
 * Convert minimal packed format to 12-character MPC report format.
 *
 * The 12-character format is used in MPC observation records (columns 1-12).
 * For numbered comets with fragments, the fragment letter(s) go in columns 11-12.
 *
 * Examples:
 *   "0073Pa"  -> "0073P      a" (numbered comet with single fragment)
 *   "0073Paa" -> "0073P     aa" (numbered comet with double fragment)
 *   "00001"   -> "     00001  " (numbered asteroid)
 *   "J95X00A" -> "     J95X00A" (provisional asteroid)
 *   "CJ95O010"-> "    CJ95O010" (provisional comet - no fragment)
 *
 * Parameters:
 *   minimal - Input minimal packed designation
 *   report  - Buffer to receive 12-character format (must be >= 13 bytes)
 *   outlen  - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_to_report_format(const char *minimal, char *report, size_t outlen);

/*
 * Convert 12-character MPC report format to minimal packed format.
 *
 * Parameters:
 *   report  - Input 12-character format designation
 *   minimal - Buffer to receive minimal packed designation
 *   outlen  - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success, error code on failure
 */
int mpc_from_report_format(const char *report, char *minimal, size_t outlen);

/* =============================================================================
 * FRAGMENT HELPER FUNCTIONS
 * ============================================================================= */

/*
 * Check if a designation has a comet fragment suffix.
 *
 * Works with both packed and unpacked formats.
 *
 * Parameters:
 *   desig - Designation to check (packed or unpacked)
 *
 * Returns:
 *   1 if has fragment, 0 if no fragment or not a comet
 */
int mpc_has_fragment(const char *desig);

/*
 * Extract the fragment suffix from a comet designation.
 *
 * Works with both packed and unpacked formats.
 * Fragment is returned in uppercase (e.g., "A", "AA").
 *
 * Parameters:
 *   desig    - Designation to extract fragment from
 *   fragment - Buffer to receive fragment (uppercase)
 *   outlen   - Size of output buffer (3 bytes minimum for "AA\0")
 *
 * Returns:
 *   MPC_OK on success (fragment buffer filled, may be empty string if no fragment)
 *   MPC_ERR_FORMAT if not a valid designation
 */
int mpc_get_fragment(const char *desig, char *fragment, size_t outlen);

/*
 * Get the parent comet designation (without fragment suffix).
 *
 * Works with both packed and unpacked formats.
 * Returns the designation in the same format (packed or unpacked) as input.
 *
 * Examples:
 *   "73P-A"   -> "73P"
 *   "0073Pa"  -> "0073P"
 *   "73P"     -> "73P" (no change)
 *   "C/1995 O1-A" -> "C/1995 O1"
 *
 * Parameters:
 *   desig  - Designation to get parent from
 *   parent - Buffer to receive parent designation
 *   outlen - Size of output buffer
 *
 * Returns:
 *   MPC_OK on success
 *   MPC_ERR_FORMAT if not a valid comet designation
 */
int mpc_get_parent(const char *desig, char *parent, size_t outlen);

/* =============================================================================
 * COMPARISON FUNCTIONS
 * ============================================================================= */

/*
 * Check if two designations refer to the same object.
 *
 * This function normalizes both designations to a canonical form
 * and compares them, handling different formats (packed/unpacked).
 *
 * Examples:
 *   mpc_designations_equal("1995 XA", "J95X00A") -> 1 (same object)
 *   mpc_designations_equal("73P", "0073P") -> 1 (same object)
 *   mpc_designations_equal("73P-A", "73P-B") -> 0 (different fragments)
 *
 * Parameters:
 *   desig1 - First designation
 *   desig2 - Second designation
 *
 * Returns:
 *   1 if same object, 0 if different or invalid
 */
int mpc_designations_equal(const char *desig1, const char *desig2);

/* =============================================================================
 * UTILITY FUNCTIONS
 * ============================================================================= */

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
