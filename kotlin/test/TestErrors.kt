import mpc.MPCDesignation
import java.io.File

/**
 * Test MPC designation error handling.
 */
fun main(args: Array<String>) {
    if (args.isEmpty()) {
        System.err.println("Usage: TestErrors <error_test_cases.csv>")
        System.exit(1)
    }

    val csvFile = args[0]
    var totalTests = 0
    var passedTests = 0
    var failedTests = 0

    println("=== MPC Designation Error Tests (Kotlin) ===")
    println()

    try {
        File(csvFile).useLines { lines ->
            for (line in lines) {
                // Skip empty lines, comments, and header
                if (line.isEmpty() || line.startsWith("#") || line.startsWith("category,")) {
                    continue
                }

                // Parse CSV line
                val parts = line.split(",", limit = 5)
                if (parts.size < 5) continue

                val category = parts[0]
                val subcategory = parts[1]
                val rawInput = parts[2]
                val expectedError = parts[3]
                val description = parts[4]

                val input = unescapeInput(rawInput)
                totalTests++

                var gotError = false
                var errorType = ""
                var resultOutput = ""

                try {
                    val result = MPCDesignation.convert(input)
                    resultOutput = result.output
                } catch (e: MPCDesignation.MPCDesignationException) {
                    gotError = true
                    val errStr = e.message ?: ""
                    errorType = if (errStr.contains("out of range") || errStr.contains("Invalid asteroid number")) {
                        "range"
                    } else {
                        "format"
                    }
                }

                val testId = "$category/$subcategory"

                if (expectedError == "valid") {
                    // Should succeed
                    if (gotError) {
                        failedTests++
                        println("FAIL [$testId]: '$description'")
                        println("      Expected: valid conversion")
                        println("      Got:      error ($errorType)")
                    } else {
                        passedTests++
                    }
                } else {
                    // Should fail
                    if (!gotError) {
                        failedTests++
                        println("FAIL [$testId]: '$description'")
                        println("      Expected: error ($expectedError)")
                        println("      Got:      '$resultOutput' (success)")
                    } else {
                        passedTests++
                    }
                }
            }
        }
    } catch (e: Exception) {
        System.err.println("Cannot open file: $csvFile")
        System.exit(1)
    }

    println()
    println("=== Error Test Results ===")
    println("Total:  $totalTests")
    println("Passed: $passedTests")
    println("Failed: $failedTests")

    if (failedTests > 0) {
        System.exit(1)
    }
}

/**
 * Convert escape sequences in test input.
 */
private fun unescapeInput(input: String): String {
    var result = input
        .replace("\\t", "\t")
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\f", "\u000C")
        .replace("\\v", "\u000B")

    // Handle \x00 style escapes
    val sb = StringBuilder()
    var i = 0
    while (i < result.length) {
        if (i + 3 < result.length && result[i] == '\\' && result[i + 1] == 'x') {
            try {
                val hexStr = result.substring(i + 2, i + 4)
                val value = hexStr.toInt(16)
                sb.append(value.toChar())
                i += 4
            } catch (e: NumberFormatException) {
                sb.append(result[i])
                i++
            }
        } else {
            sb.append(result[i])
            i++
        }
    }

    return sb.toString()
}
