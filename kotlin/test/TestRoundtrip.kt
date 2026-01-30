package mpc

import mpc.MPCDesignation
import java.io.File

/**
 * Test MPC designation with bidirectional timing and round-trip verification.
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
 */
data class TestCase(val unpacked: String, val packed: String)

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        System.err.println("Usage: TestRoundtrip <prov_unpack_to_pack.csv>")
        System.exit(1)
    }

    val csvFile = args[0]

    // Load test data
    val testCases = mutableListOf<TestCase>()
    try {
        File(csvFile).useLines { lines ->
            var isFirstLine = true
            for (line in lines) {
                if (line.isEmpty()) continue
                if (isFirstLine) {
                    isFirstLine = false
                    if (line.startsWith("unpacked") || line.contains("designation")) continue
                }
                val parts = line.split(",", limit = 2)
                if (parts.size == 2) {
                    testCases.add(TestCase(parts[0], parts[1]))
                }
            }
        }
    } catch (e: Exception) {
        System.err.println("Cannot open file: $csvFile")
        System.exit(1)
    }

    println("Loaded ${testCases.size} test cases")
    println()

    var packPassed = 0L
    var packFailed = 0L
    var unpackPassed = 0L
    var unpackFailed = 0L
    var rtUnpackedPassed = 0L
    var rtUnpackedFailed = 0L
    var rtPackedPassed = 0L
    var rtPackedFailed = 0L

    // Phase 1: Pack (unpacked -> packed)
    println("=== Phase 1: Pack (unpacked -> packed) ===")
    var startTime = System.currentTimeMillis()

    for (tc in testCases) {
        try {
            val got = MPCDesignation.pack(tc.unpacked)
            if (got == tc.packed) packPassed++ else packFailed++
        } catch (e: Exception) {
            packFailed++
        }
    }

    var elapsed = System.currentTimeMillis() - startTime
    var rate = testCases.size * 1000.0 / elapsed
    println("Passed: $packPassed")
    println("Failed: $packFailed")
    println("Time:   ${elapsed}ms (${"%.1f".format(rate)} entries/sec)")
    println()

    // Phase 2: Unpack (packed -> unpacked)
    println("=== Phase 2: Unpack (packed -> unpacked) ===")
    startTime = System.currentTimeMillis()

    for (tc in testCases) {
        try {
            val got = MPCDesignation.unpack(tc.packed)
            if (got == tc.unpacked) unpackPassed++ else unpackFailed++
        } catch (e: Exception) {
            unpackFailed++
        }
    }

    elapsed = System.currentTimeMillis() - startTime
    rate = testCases.size * 1000.0 / elapsed
    println("Passed: $unpackPassed")
    println("Failed: $unpackFailed")
    println("Time:   ${elapsed}ms (${"%.1f".format(rate)} entries/sec)")
    println()

    // Phase 3: Unpacked round-trip: unpack(pack(x)) = x
    println("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===")
    startTime = System.currentTimeMillis()

    for (tc in testCases) {
        try {
            val packed = MPCDesignation.pack(tc.unpacked)
            val backToUnpacked = MPCDesignation.unpack(packed)
            if (backToUnpacked == tc.unpacked) rtUnpackedPassed++ else rtUnpackedFailed++
        } catch (e: Exception) {
            rtUnpackedFailed++
        }
    }

    elapsed = System.currentTimeMillis() - startTime
    rate = testCases.size * 1000.0 / elapsed
    println("Passed: $rtUnpackedPassed")
    println("Failed: $rtUnpackedFailed")
    println("Time:   ${elapsed}ms (${"%.1f".format(rate)} entries/sec)")
    println()

    // Phase 4: Packed round-trip: pack(unpack(y)) = y
    println("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===")
    startTime = System.currentTimeMillis()

    for (tc in testCases) {
        try {
            val unpacked = MPCDesignation.unpack(tc.packed)
            val backToPacked = MPCDesignation.pack(unpacked)
            if (backToPacked == tc.packed) rtPackedPassed++ else rtPackedFailed++
        } catch (e: Exception) {
            rtPackedFailed++
        }
    }

    elapsed = System.currentTimeMillis() - startTime
    rate = testCases.size * 1000.0 / elapsed
    println("Passed: $rtPackedPassed")
    println("Failed: $rtPackedFailed")
    println("Time:   ${elapsed}ms (${"%.1f".format(rate)} entries/sec)")
    println()

    // Summary
    println("=== Summary ===")
    println("Pack:       ${if (packFailed == 0L) "PASS" else "FAIL ($packFailed)"}")
    println("Unpack:     ${if (unpackFailed == 0L) "PASS" else "FAIL ($unpackFailed)"}")
    println("Unpacked RT: ${if (rtUnpackedFailed == 0L) "PASS" else "FAIL ($rtUnpackedFailed)"}")
    println("Packed RT:   ${if (rtPackedFailed == 0L) "PASS" else "FAIL ($rtPackedFailed)"}")

    if (packFailed > 0 || rtPackedFailed > 0) {
        System.exit(1)
    }
}
