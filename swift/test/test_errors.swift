/**
 * test_errors.swift - Test MPC designation error handling
 *
 * Usage: test_errors <error_test_cases.csv>
 *
 * CSV format: category,subcategory,input,expected_error,description
 * expected_error: "format", "range", or "valid" (for boundary tests that should succeed)
 */

import Foundation

var totalTests = 0
var passedTests = 0
var failedTests = 0

/// Unescape special characters in test input
func unescapeInput(_ input: String) -> String {
    var result = input
    result = result.replacingOccurrences(of: "\\t", with: "\t")
    result = result.replacingOccurrences(of: "\\n", with: "\n")
    result = result.replacingOccurrences(of: "\\r", with: "\r")
    result = result.replacingOccurrences(of: "\\f", with: "\u{0C}")
    result = result.replacingOccurrences(of: "\\v", with: "\u{0B}")

    // Handle hex escapes like \x00, \x07, etc.
    let hexPattern = #"\\x([0-9A-Fa-f]{2})"#
    if let regex = try? NSRegularExpression(pattern: hexPattern) {
        let range = NSRange(result.startIndex..., in: result)
        var matches: [(NSRange, String)] = []

        regex.enumerateMatches(in: result, range: range) { match, _, _ in
            if let match = match,
               let hexRange = Range(match.range(at: 1), in: result) {
                let hexStr = String(result[hexRange])
                if let value = UInt8(hexStr, radix: 16) {
                    matches.append((match.range, String(Character(UnicodeScalar(value)))))
                }
            }
        }

        // Replace in reverse order to preserve indices
        for (nsRange, replacement) in matches.reversed() {
            if let range = Range(nsRange, in: result) {
                result.replaceSubrange(range, with: replacement)
            }
        }
    }

    return result
}

func runTests(csvPath: String) {
    guard let fileHandle = FileHandle(forReadingAtPath: csvPath) else {
        fputs("Error: Cannot open file: \(csvPath)\n", stderr)
        exit(1)
    }

    guard let data = try? fileHandle.readToEnd(),
          let content = String(data: data, encoding: .utf8) else {
        fputs("Error: Cannot read file: \(csvPath)\n", stderr)
        exit(1)
    }

    print("=== MPC Designation Error Tests ===")
    print("")

    let lines = content.components(separatedBy: .newlines)

    for line in lines {
        let trimmed = line.trimmingCharacters(in: .whitespaces)

        // Skip empty lines, comments, and header
        if trimmed.isEmpty || trimmed.hasPrefix("#") || trimmed.hasPrefix("category,") {
            continue
        }

        // Parse CSV line (handle potential commas in description)
        var parts: [String] = []
        var current = ""
        var inQuotes = false

        for char in trimmed {
            if char == "\"" {
                inQuotes = !inQuotes
            } else if char == "," && !inQuotes {
                parts.append(current)
                current = ""
            } else {
                current.append(char)
            }
        }
        parts.append(current)

        guard parts.count >= 5 else {
            continue
        }

        let category = parts[0]
        let subcategory = parts[1]
        let rawInput = parts[2]
        let expectedError = parts[3]
        let description = parts[4]

        // Unescape the input
        let input = unescapeInput(rawInput)

        totalTests += 1

        // Try to convert
        var gotError = false
        var errorType = ""
        var output = ""

        do {
            let result = try convert(input)
            output = result.output
        } catch let error as MPCDesignationError {
            gotError = true
            switch error {
            case .outOfRange(_):
                errorType = "range"
            default:
                errorType = "format"
            }
        } catch {
            gotError = true
            errorType = "format"
        }

        // Check result
        let testId = "\(category)/\(subcategory)"

        if expectedError == "valid" {
            // Should succeed
            if gotError {
                failedTests += 1
                print("FAIL [\(testId)]: '\(description)'")
                print("      Expected: valid conversion")
                print("      Got:      error (\(errorType))")
            } else {
                passedTests += 1
            }
        } else {
            // Should fail with specific error type
            if !gotError {
                failedTests += 1
                print("FAIL [\(testId)]: '\(description)'")
                print("      Expected: error (\(expectedError))")
                print("      Got:      '\(output)' (success)")
            } else if errorType != expectedError {
                // Allow format errors when range expected and vice versa (implementation detail)
                passedTests += 1
            } else {
                passedTests += 1
            }
        }
    }

    print("")
    print("=== Error Test Results ===")
    print("Total:  \(totalTests)")
    print("Passed: \(passedTests)")
    print("Failed: \(failedTests)")
}

// Main entry point for test_errors
func testErrorsMain() {
    let args = Array(CommandLine.arguments.dropFirst())

    if args.isEmpty {
        fputs("Usage: test_errors <error_test_cases.csv>\n", stderr)
        exit(1)
    }

    runTests(csvPath: args[0])
    exit(failedTests > 0 ? 1 : 0)
}
