import mpc.MPCDesignation
import java.io.File

/**
 * Test MPC designation round-trip conversions (pack -> unpack -> pack).
 */
fun main(args: Array<String>) {
    if (args.isEmpty()) {
        System.err.println("Usage: TestRoundtrip <prov_unpack_to_pack.csv>")
        System.exit(1)
    }

    val csvFile = args[0]
    var totalTests = 0
    var passedTests = 0
    var failedTests = 0

    println("=== MPC Designation Round-Trip Tests (Kotlin) ===")
    println()

    try {
        File(csvFile).useLines { lines ->
            var isFirstLine = true
            for (line in lines) {
                // Skip empty lines
                if (line.isEmpty()) continue

                // Skip header row
                if (isFirstLine) {
                    isFirstLine = false
                    if (line.startsWith("unpacked") || line.contains("designation")) {
                        continue
                    }
                }

                // Parse CSV line
                val parts = line.split(",", limit = 2)
                if (parts.size != 2) continue

                val unpacked = parts[0]
                val packed = parts[1]

                totalTests++

                try {
                    // Test round-trip: unpacked -> packed -> unpacked -> packed
                    val packed1 = MPCDesignation.convertSimple(unpacked)
                    val unpacked1 = MPCDesignation.convertSimple(packed1)
                    val packed2 = MPCDesignation.convertSimple(unpacked1)

                    if (packed1 != packed2) {
                        failedTests++
                        if (failedTests <= 10) {
                            println("FAIL: roundtrip('$unpacked')")
                            println("      unpacked -> '$packed1' -> '$unpacked1' -> '$packed2'")
                            println("      packed1 != packed2")
                        }
                    } else if (packed1 != packed) {
                        failedTests++
                        if (failedTests <= 10) {
                            println("FAIL: roundtrip('$unpacked')")
                            println("      Expected packed: '$packed'")
                            println("      Got packed:      '$packed1'")
                        }
                    } else {
                        passedTests++
                    }
                } catch (e: MPCDesignation.MPCDesignationException) {
                    failedTests++
                    if (failedTests <= 10) {
                        println("FAIL: roundtrip('$unpacked')")
                        println("      Error: ${e.message}")
                    }
                }
            }
        }
    } catch (e: Exception) {
        System.err.println("Cannot open file: $csvFile")
        System.exit(1)
    }

    println()
    println("=== Round-Trip Test Results ===")
    println("Total:  $totalTests")
    println("Passed: $passedTests")
    println("Failed: $failedTests")

    if (failedTests > 10) {
        println("(Showing first 10 failures only)")
    }

    if (failedTests > 0) {
        System.exit(1)
    }
}
