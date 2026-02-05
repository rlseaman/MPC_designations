/*
 * test_fragments.c - Test comet fragment handling in MPC designation converter
 *
 * Tests numbered comets with fragments (73P-A, 73P-AA) and
 * provisional comets with fragments (P/1930 J1-A, P/1930 J1-AA).
 *
 * Fragment letters include all A-Z (including I, per MPC data).
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mpc_designation.h"

static int passed = 0;
static int failed = 0;

static void test_case(const char *input, const char *expected, const char *desc) {
    (void)desc;  /* Used only for documentation */
    char output[64];
    int err = mpc_convert_simple(input, output, sizeof(output));

    if (err == MPC_OK && strcmp(output, expected) == 0) {
        printf("  PASS: %s -> %s\n", input, output);
        passed++;
    } else if (err != MPC_OK) {
        printf("  FAIL: %s: error %d (%s)\n", input, err, mpc_strerror(err));
        failed++;
    } else {
        printf("  FAIL: %s: expected '%s', got '%s'\n", input, expected, output);
        failed++;
    }
}

static void test_error(const char *input, const char *desc) {
    (void)desc;  /* Used only for documentation */
    char output[64];
    int err = mpc_convert_simple(input, output, sizeof(output));

    if (err != MPC_OK) {
        printf("  PASS: %s correctly rejected\n", input);
        passed++;
    } else {
        printf("  FAIL: %s: expected error, got '%s'\n", input, output);
        failed++;
    }
}

static void test_roundtrip(const char *input) {
    char packed[64], unpacked[64];

    int err1 = mpc_convert_simple(input, packed, sizeof(packed));
    if (err1 != MPC_OK) {
        printf("  FAIL: %s: pack error %d\n", input, err1);
        failed++;
        return;
    }

    int err2 = mpc_convert_simple(packed, unpacked, sizeof(unpacked));
    if (err2 != MPC_OK) {
        printf("  FAIL: %s -> %s: unpack error %d\n", input, packed, err2);
        failed++;
        return;
    }

    if (strcmp(input, unpacked) == 0) {
        printf("  PASS: %s -> %s -> %s\n", input, packed, unpacked);
        passed++;
    } else {
        printf("  FAIL: %s -> %s -> %s (expected %s)\n", input, packed, unpacked, input);
        failed++;
    }
}

int main(void) {
    printf("=== MPC Designation Fragment Tests ===\n\n");

    /* Numbered Comet Fragments (Pack) */
    printf("--- Numbered Comet Fragments (Pack) ---\n");
    test_case("73P", "0073P", "Basic numbered comet");
    test_case("73P-A", "0073Pa", "Single-letter fragment A");
    test_case("73P-B", "0073Pb", "Single-letter fragment B");
    test_case("73P-H", "0073Ph", "Single-letter fragment H");
    test_case("73P-I", "0073Pi", "Single-letter fragment I (included!)");
    test_case("73P-J", "0073Pj", "Single-letter fragment J");
    test_case("73P-Z", "0073Pz", "Single-letter fragment Z");
    test_case("73P-AA", "0073Paa", "Two-letter fragment AA");
    test_case("73P-AB", "0073Pab", "Two-letter fragment AB");
    test_case("73P-AI", "0073Pai", "Two-letter fragment AI (I included)");
    test_case("73P-AZ", "0073Paz", "Two-letter fragment AZ");
    test_case("73P-BA", "0073Pba", "Two-letter fragment BA");
    test_case("73P-BI", "0073Pbi", "Two-letter fragment BI");
    test_case("73P-BZ", "0073Pbz", "Two-letter fragment BZ");
    test_case("73P-ZZ", "0073Pzz", "Two-letter fragment ZZ");
    test_case("1P-A", "0001Pa", "Comet 1P with fragment");
    test_case("9999P-ZZ", "9999Pzz", "Maximum comet with fragment");
    test_case("1D-A", "0001Da", "Defunct comet with fragment");
    test_case("354D-BB", "0354Dbb", "Defunct comet with two-letter fragment");

    /* Numbered Comet Fragments (Unpack) */
    printf("\n--- Numbered Comet Fragments (Unpack) ---\n");
    test_case("0073P", "73P", "Basic numbered comet");
    test_case("0073Pa", "73P-A", "Single-letter fragment A");
    test_case("0073Pb", "73P-B", "Single-letter fragment B");
    test_case("0073Ph", "73P-H", "Single-letter fragment H");
    test_case("0073Pi", "73P-I", "Single-letter fragment I");
    test_case("0073Pj", "73P-J", "Single-letter fragment J");
    test_case("0073Pz", "73P-Z", "Single-letter fragment Z");
    test_case("0073Paa", "73P-AA", "Two-letter fragment AA");
    test_case("0073Pab", "73P-AB", "Two-letter fragment AB");
    test_case("0073Pai", "73P-AI", "Two-letter fragment AI");
    test_case("0073Paz", "73P-AZ", "Two-letter fragment AZ");
    test_case("0073Pba", "73P-BA", "Two-letter fragment BA");
    test_case("0073Pbi", "73P-BI", "Two-letter fragment BI");
    test_case("0073Pbz", "73P-BZ", "Two-letter fragment BZ");
    test_case("0073Pzz", "73P-ZZ", "Two-letter fragment ZZ");
    test_case("0001Pa", "1P-A", "Comet 1P with fragment");
    test_case("9999Pzz", "9999P-ZZ", "Maximum comet with fragment");
    test_case("0001Da", "1D-A", "Defunct comet with fragment");
    test_case("0354Dbb", "354D-BB", "Defunct comet with two-letter fragment");

    /* Provisional Comet Fragments (Pack) */
    printf("\n--- Provisional Comet Fragments (Pack) ---\n");
    test_case("C/1995 O1", "CJ95O010", "Basic provisional comet");
    test_case("D/1993 F2-A", "DJ93F02a", "Single-letter fragment A");
    test_case("D/1993 F2-B", "DJ93F02b", "Single-letter fragment B");
    test_case("D/1993 F2-W", "DJ93F02w", "Single-letter fragment W");
    test_case("C/1996 B2-A", "CJ96B02a", "Hyakutake fragment A");
    test_case("P/1930 J1-AA", "PJ30J01aa", "Two-letter fragment AA");
    test_case("P/1930 J1-AI", "PJ30J01ai", "Two-letter fragment AI (I included)");
    test_case("P/1930 J1-AZ", "PJ30J01az", "Two-letter fragment AZ");
    test_case("P/1930 J1-BA", "PJ30J01ba", "Two-letter fragment BA");
    test_case("P/1930 J1-BI", "PJ30J01bi", "Two-letter fragment BI");

    /* Provisional Comet Fragments (Unpack) */
    printf("\n--- Provisional Comet Fragments (Unpack) ---\n");
    test_case("CJ95O010", "C/1995 O1", "Basic provisional comet");
    test_case("DJ93F02a", "D/1993 F2-A", "Single-letter fragment A");
    test_case("DJ93F02b", "D/1993 F2-B", "Single-letter fragment B");
    test_case("DJ93F02w", "D/1993 F2-W", "Single-letter fragment W");
    test_case("CJ96B02a", "C/1996 B2-A", "Hyakutake fragment A");
    test_case("PJ30J01aa", "P/1930 J1-AA", "Two-letter fragment AA");
    test_case("PJ30J01ai", "P/1930 J1-AI", "Two-letter fragment AI");
    test_case("PJ30J01az", "P/1930 J1-AZ", "Two-letter fragment AZ");
    test_case("PJ30J01ba", "P/1930 J1-BA", "Two-letter fragment BA");
    test_case("PJ30J01bi", "P/1930 J1-BI", "Two-letter fragment BI");

    /* Pre-1925 A-Prefix Format */
    printf("\n--- Pre-1925 A-Prefix Format ---\n");
    test_case("I01A00A", "A801 AA", "1801 unpacks to A801");
    test_case("J08C00J", "A908 CJ", "1908 unpacks to A908");
    test_case("J24Y00Z", "A924 YZ", "1924 unpacks to A924");
    test_case("J25A00A", "1925 AA", "1925 stays as 1925");
    test_case("J26B01C", "1926 BC1", "1926 stays as 1926");
    test_case("A908 CJ", "J08C00J", "A908 packs normally");
    test_case("A801 AA", "I01A00A", "A801 packs normally");

    /* Century Code Validation */
    printf("\n--- Century Code Validation ---\n");
    test_case("1800 AA", "I00A00A", "Minimum asteroid year 1800");
    test_case("2199 YZ", "L99Y00Z", "Maximum asteroid year 2199");
    test_case("C/1014 C1", "CA14C010", "Ancient comet 1014 CE");
    test_case("C/1596 S1", "CF96S010", "Historical comet 1596");

    /* Invalid Century Codes (should error) */
    printf("\n--- Invalid Century Codes (should error) ---\n");
    test_error("1700 AA", "Asteroid year before 1800");
    test_error("2200 AA", "Asteroid year after 2199");

    /* Roundtrip Tests */
    printf("\n--- Roundtrip Tests ---\n");
    test_roundtrip("73P-A");
    test_roundtrip("73P-AA");
    test_roundtrip("73P-AI");
    test_roundtrip("73P-ZZ");
    test_roundtrip("P/1930 J1-AA");
    test_roundtrip("P/1930 J1-AI");
    test_roundtrip("C/1995 O1");
    test_roundtrip("D/1993 F2-B");
    test_roundtrip("A908 CJ");
    test_roundtrip("1995 XA");

    /* Summary */
    printf("\n==================================================\n");
    printf("Total: %d, Passed: %d, Failed: %d\n", passed + failed, passed, failed);

    return failed > 0 ? 1 : 0;
}
