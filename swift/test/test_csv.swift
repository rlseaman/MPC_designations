/**
 * test_csv.swift - Test MPC designation conversion against CSV test data
 *
 * Usage: test_csv <csv_file>
 *
 * CSV format: unpacked,packed (one pair per line)
 */

import Foundation

var totalTests = 0
var passedTests = 0
var failedTests = 0

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

    let startTime = Date()
    let lines = content.components(separatedBy: .newlines)
    var isFirstLine = true

    for line in lines {
        let trimmed = line.trimmingCharacters(in: .whitespaces)

        // Skip header line
        if isFirstLine {
            isFirstLine = false
            continue
        }

        // Skip empty lines and comments
        if trimmed.isEmpty || trimmed.hasPrefix("#") {
            continue
        }

        // Parse CSV line
        let parts = trimmed.components(separatedBy: ",")
        guard parts.count >= 2 else {
            continue
        }

        let unpacked = parts[0]
        let expectedPacked = parts[1]

        totalTests += 1

        // Test unpacked -> packed (one direction only, matching Python test)
        do {
            let gotPacked = try convertSimple(unpacked)
            if gotPacked != expectedPacked {
                failedTests += 1
                if failedTests <= 100 {
                    print("FAIL: convert('\(unpacked)') = '\(gotPacked)', expected '\(expectedPacked)'")
                }
                continue
            }
        } catch {
            failedTests += 1
            if failedTests <= 100 {
                print("FAIL: convert('\(unpacked)') threw error: \(error)")
            }
            continue
        }

        passedTests += 1

        // Progress indicator
        if totalTests % 100000 == 0 {
            print("Processed \(totalTests) entries...")
        }
    }

    let elapsed = Date().timeIntervalSince(startTime)
    let elapsedMs = Int(elapsed * 1000)
    let rate = totalTests > 0 ? Double(totalTests) / elapsed : 0

    print("")
    print("=== Test Results ===")
    print("Total:  \(totalTests)")
    print("Passed: \(passedTests)")
    print("Failed: \(failedTests)")
    print("Time:   \(elapsedMs)ms (\(String(format: "%.1f", rate)) entries/sec)")
}

// Main entry point for test_csv
func testCSVMain() {
    let args = Array(CommandLine.arguments.dropFirst())

    if args.isEmpty {
        fputs("Usage: test_csv <csv_file>\n", stderr)
        exit(1)
    }

    runTests(csvPath: args[0])
    exit(failedTests > 0 ? 1 : 0)
}
