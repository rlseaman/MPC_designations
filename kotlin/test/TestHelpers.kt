package mpc

/**
 * Test helper functions in MPC designation converter.
 *
 * Tests format conversion (minimal <-> 12-char report format),
 * fragment extraction, and designation comparison functions.
 */

import mpc.MPCDesignation

var passed = 0
var failed = 0

fun testToReport(input: String, expected: String, desc: String) {
    try {
        val output = MPCDesignation.toReportFormat(input)
        if (output == expected) {
            println("  PASS: toReport(\"$input\") -> \"$output\"")
            passed++
        } else {
            println("  FAIL: toReport(\"$input\"): expected \"$expected\", got \"$output\"")
            failed++
        }
    } catch (e: Exception) {
        println("  FAIL: toReport(\"$input\"): error ${e.message}")
        failed++
    }
}

fun testFromReport(input: String, expected: String, desc: String) {
    try {
        val output = MPCDesignation.fromReportFormat(input)
        if (output == expected) {
            println("  PASS: fromReport(\"$input\") -> \"$output\"")
            passed++
        } else {
            println("  FAIL: fromReport(\"$input\"): expected \"$expected\", got \"$output\"")
            failed++
        }
    } catch (e: Exception) {
        println("  FAIL: fromReport(\"$input\"): error ${e.message}")
        failed++
    }
}

fun testHasFragment(input: String, expected: Boolean, desc: String) {
    val result = MPCDesignation.hasFragment(input)
    if (result == expected) {
        println("  PASS: hasFragment(\"$input\") -> $result")
        passed++
    } else {
        println("  FAIL: hasFragment(\"$input\"): expected $expected, got $result")
        failed++
    }
}

fun testGetFragment(input: String, expected: String, desc: String) {
    val output = MPCDesignation.getFragment(input)
    if (output == expected) {
        println("  PASS: getFragment(\"$input\") -> \"$output\"")
        passed++
    } else {
        println("  FAIL: getFragment(\"$input\"): expected \"$expected\", got \"$output\"")
        failed++
    }
}

fun testGetParent(input: String, expected: String, desc: String) {
    val output = MPCDesignation.getParent(input)
    if (output == expected) {
        println("  PASS: getParent(\"$input\") -> \"$output\"")
        passed++
    } else {
        println("  FAIL: getParent(\"$input\"): expected \"$expected\", got \"$output\"")
        failed++
    }
}

fun testEqual(d1: String, d2: String, expected: Boolean, desc: String) {
    val result = MPCDesignation.designationsEqual(d1, d2)
    if (result == expected) {
        println("  PASS: equal(\"$d1\", \"$d2\") -> $result")
        passed++
    } else {
        println("  FAIL: equal(\"$d1\", \"$d2\"): expected $expected, got $result")
        failed++
    }
}

fun main() {
    println("=== MPC Designation Helper Function Tests ===\n")

    // Test toReportFormat
    println("--- toReportFormat ---")

    // Numbered asteroids
    testToReport("00001", "       00001", "Numbered asteroid 1")
    testToReport("00433", "       00433", "Numbered asteroid 433")
    testToReport("99999", "       99999", "Numbered asteroid 99999")
    testToReport("A0000", "       A0000", "Numbered asteroid 100000")
    testToReport("~0000", "       ~0000", "Numbered asteroid 620000")

    // Provisional asteroids
    testToReport("J95X00A", "     J95X00A", "Provisional 1995 XA")
    testToReport("K24A12B", "     K24A12B", "Provisional 2024 AB12")

    // Survey designations
    testToReport("PLS2040", "     PLS2040", "Survey P-L")
    testToReport("T3S3141", "     T3S3141", "Survey T-3")

    // Numbered comets
    testToReport("0001P", "0001P       ", "Comet 1P")
    testToReport("0073P", "0073P       ", "Comet 73P")

    // Numbered comets with fragments
    testToReport("0073Pa", "0073P      a", "Comet 73P-A")
    testToReport("0073Pb", "0073P      b", "Comet 73P-B")
    testToReport("0073Paa", "0073P     aa", "Comet 73P-AA")
    testToReport("0073Paz", "0073P     az", "Comet 73P-AZ")
    testToReport("0073Pzz", "0073P     zz", "Comet 73P-ZZ")

    // Provisional comets
    testToReport("CJ95O010", "    CJ95O010", "Comet C/1995 O1")
    testToReport("DJ93F020", "    DJ93F020", "Comet D/1993 F2")
    testToReport("DJ93F02a", "    DJ93F02a", "Comet D/1993 F2-A")

    // Test fromReportFormat
    println("\n--- fromReportFormat ---")

    // Numbered asteroids
    testFromReport("       00001", "00001", "Numbered asteroid 1")
    testFromReport("       00433", "00433", "Numbered asteroid 433")
    testFromReport("       A0000", "A0000", "Numbered asteroid 100000")

    // Provisional asteroids
    testFromReport("     J95X00A", "J95X00A", "Provisional 1995 XA")

    // Numbered comets
    testFromReport("0073P       ", "0073P", "Comet 73P")

    // Numbered comets with fragments
    testFromReport("0073P      a", "0073Pa", "Comet 73P-A")
    testFromReport("0073P     aa", "0073Paa", "Comet 73P-AA")
    testFromReport("0073P     az", "0073Paz", "Comet 73P-AZ")

    // Provisional comets
    testFromReport("    CJ95O010", "CJ95O010", "Comet C/1995 O1")

    // Test hasFragment
    println("\n--- hasFragment ---")

    // Unpacked with fragments
    testHasFragment("73P-A", true, "Unpacked numbered comet with fragment")
    testHasFragment("73P-AA", true, "Unpacked numbered comet with 2-letter fragment")
    testHasFragment("D/1993 F2-A", true, "Unpacked provisional comet with fragment")
    testHasFragment("P/1930 J1-AA", true, "Unpacked provisional comet with 2-letter fragment")

    // Unpacked without fragments
    testHasFragment("73P", false, "Unpacked numbered comet no fragment")
    testHasFragment("C/1995 O1", false, "Unpacked provisional comet no fragment")

    // Packed with fragments
    testHasFragment("0073Pa", true, "Packed numbered comet with fragment")
    testHasFragment("0073Paa", true, "Packed numbered comet with 2-letter fragment")
    testHasFragment("DJ93F02a", true, "Packed provisional comet with fragment")

    // Packed without fragments
    testHasFragment("0073P", false, "Packed numbered comet no fragment")
    testHasFragment("CJ95O010", false, "Packed provisional comet no fragment")

    // Non-comets
    testHasFragment("1995 XA", false, "Asteroid no fragment")
    testHasFragment("00001", false, "Numbered asteroid")

    // Test getFragment
    println("\n--- getFragment ---")

    // Unpacked with fragments
    testGetFragment("73P-A", "A", "Unpacked single fragment")
    testGetFragment("73P-AA", "AA", "Unpacked 2-letter fragment")
    testGetFragment("73P-I", "I", "Unpacked fragment I")
    testGetFragment("D/1993 F2-B", "B", "Unpacked provisional fragment")
    testGetFragment("P/1930 J1-AZ", "AZ", "Unpacked provisional 2-letter")

    // Unpacked without fragments
    testGetFragment("73P", "", "Unpacked no fragment")
    testGetFragment("C/1995 O1", "", "Unpacked provisional no fragment")

    // Packed with fragments
    testGetFragment("0073Pa", "A", "Packed single fragment")
    testGetFragment("0073Paa", "AA", "Packed 2-letter fragment")
    testGetFragment("0073Pi", "I", "Packed fragment I")
    testGetFragment("DJ93F02b", "B", "Packed provisional fragment")

    // Packed without fragments
    testGetFragment("0073P", "", "Packed no fragment")
    testGetFragment("CJ95O010", "", "Packed provisional no fragment")

    // Test getParent
    println("\n--- getParent ---")

    // Unpacked with fragments
    testGetParent("73P-A", "73P", "Unpacked single fragment")
    testGetParent("73P-AA", "73P", "Unpacked 2-letter fragment")
    testGetParent("D/1993 F2-B", "D/1993 F2", "Unpacked provisional fragment")
    testGetParent("P/1930 J1-AA", "P/1930 J1", "Unpacked provisional 2-letter")

    // Unpacked without fragments
    testGetParent("73P", "73P", "Unpacked no fragment")
    testGetParent("C/1995 O1", "C/1995 O1", "Unpacked provisional no fragment")

    // Packed with fragments
    testGetParent("0073Pa", "0073P", "Packed single fragment")
    testGetParent("0073Paa", "0073P", "Packed 2-letter fragment")

    // Packed without fragments
    testGetParent("0073P", "0073P", "Packed no fragment")

    // Non-comets (should return as-is)
    testGetParent("1995 XA", "1995 XA", "Asteroid")
    testGetParent("00001", "00001", "Numbered asteroid")

    // Test designationsEqual
    println("\n--- designationsEqual ---")

    // Same designation, different formats
    testEqual("1995 XA", "J95X00A", true, "Provisional packed/unpacked")
    testEqual("73P", "0073P", true, "Numbered comet packed/unpacked")
    testEqual("73P-A", "0073Pa", true, "Comet with fragment packed/unpacked")
    testEqual("73P-AA", "0073Paa", true, "Comet with 2-letter fragment")
    testEqual("1", "00001", true, "Numbered asteroid")
    testEqual("C/1995 O1", "CJ95O010", true, "Provisional comet")

    // Different designations
    testEqual("1995 XA", "1995 XB", false, "Different provisional")
    testEqual("73P-A", "73P-B", false, "Different fragments")
    testEqual("73P", "74P", false, "Different comet numbers")
    testEqual("1", "2", false, "Different asteroid numbers")

    // Same designation (both packed or both unpacked)
    testEqual("1995 XA", "1995 XA", true, "Same unpacked")
    testEqual("J95X00A", "J95X00A", true, "Same packed")

    // Summary
    println("\n==================================================")
    println("Total: ${passed + failed}, Passed: $passed, Failed: $failed")

    if (failed > 0) {
        System.exit(1)
    }
}
