import mpc.MPCDesignation
import java.io.File

/**
 * Test MPC designation conversions against CSV test data.
 */
fun main(args: Array<String>) {
    if (args.isEmpty()) {
        System.err.println("Usage: TestCsv <prov_unpack_to_pack.csv>")
        System.exit(1)
    }

    val csvFile = args[0]
    var totalTests = 0
    var passedTests = 0
    var failedTests = 0

    println("=== MPC Designation Conversion Tests (Kotlin) ===")
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
                val expectedPacked = parts[1]

                totalTests++

                try {
                    // Test packing
                    val actualPacked = MPCDesignation.convertSimple(unpacked)
                    if (actualPacked != expectedPacked) {
                        failedTests++
                        if (failedTests <= 10) {
                            println("FAIL: pack('$unpacked')")
                            println("      Expected: '$expectedPacked'")
                            println("      Got:      '$actualPacked'")
                        }
                    } else {
                        passedTests++
                    }
                } catch (e: MPCDesignation.MPCDesignationException) {
                    failedTests++
                    if (failedTests <= 10) {
                        println("FAIL: pack('$unpacked')")
                        println("      Expected: '$expectedPacked'")
                        println("      Got error: ${e.message}")
                    }
                }
            }
        }
    } catch (e: Exception) {
        System.err.println("Cannot open file: $csvFile")
        System.exit(1)
    }

    println()
    println("=== Conversion Test Results ===")
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
