/**
 * test_roundtrip.swift - Bidirectional testing with round-trip verification
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
 *
 * Usage: test_roundtrip <csv_file>
 */

import Foundation

let MAX_ERRORS = 20

struct TestError {
    let phase: String
    let input: String
    let got: String
    let expected: String
}

var unpackedList: [String] = []
var packedList: [String] = []
var errors: [TestError] = []

func loadTestData(csvPath: String) -> Bool {
    guard let fileHandle = FileHandle(forReadingAtPath: csvPath) else {
        fputs("Error: Cannot open file: \(csvPath)\n", stderr)
        return false
    }

    guard let data = try? fileHandle.readToEnd(),
          let content = String(data: data, encoding: .utf8) else {
        fputs("Error: Cannot read file: \(csvPath)\n", stderr)
        return false
    }

    let lines = content.components(separatedBy: .newlines)
    var isFirstLine = true

    for line in lines {
        let trimmed = line.trimmingCharacters(in: .whitespaces)

        if isFirstLine {
            isFirstLine = false
            continue
        }

        if trimmed.isEmpty {
            continue
        }

        let parts = trimmed.components(separatedBy: ",")
        if parts.count >= 2 {
            unpackedList.append(parts[0])
            packedList.append(parts[1])
        }
    }

    return true
}

func statusString(_ failed: Int) -> String {
    return failed == 0 ? "PASS" : "FAIL (\(failed))"
}

func runRoundtripTests() {
    let total = unpackedList.count
    print("Loaded \(total) test cases\n")

    // ========== Phase 1: Pack (unpacked -> packed) ==========
    print("=== Phase 1: Pack (unpacked -> packed) ===")
    var packPassed = 0
    var packFailed = 0
    var startTime = Date()

    for i in 0..<total {
        let unpacked = unpackedList[i]
        let expected = packedList[i]

        do {
            let got = try convertSimple(unpacked)
            if got == expected {
                packPassed += 1
            } else {
                packFailed += 1
                if errors.count < MAX_ERRORS {
                    errors.append(TestError(phase: "pack", input: unpacked, got: got, expected: expected))
                }
            }
        } catch {
            packFailed += 1
            if errors.count < MAX_ERRORS {
                errors.append(TestError(phase: "pack", input: unpacked, got: "ERROR: \(error)", expected: expected))
            }
        }
    }

    let packTime = Int(Date().timeIntervalSince(startTime) * 1000)
    let packRate = Double(total) / (Double(packTime) / 1000.0)
    print("Passed: \(packPassed)")
    print("Failed: \(packFailed)")
    print("Time:   \(packTime)ms (\(String(format: "%.1f", packRate)) entries/sec)\n")

    // ========== Phase 2: Unpack (packed -> unpacked) ==========
    print("=== Phase 2: Unpack (packed -> unpacked) ===")
    var unpackPassed = 0
    var unpackFailed = 0
    startTime = Date()

    for i in 0..<total {
        let packed = packedList[i]
        let expected = unpackedList[i]

        do {
            let got = try convertSimple(packed)
            if got == expected {
                unpackPassed += 1
            } else {
                unpackFailed += 1
                if errors.count < MAX_ERRORS {
                    errors.append(TestError(phase: "unpack", input: packed, got: got, expected: expected))
                }
            }
        } catch {
            unpackFailed += 1
            if errors.count < MAX_ERRORS {
                errors.append(TestError(phase: "unpack", input: packed, got: "ERROR: \(error)", expected: expected))
            }
        }
    }

    let unpackTime = Int(Date().timeIntervalSince(startTime) * 1000)
    let unpackRate = Double(total) / (Double(unpackTime) / 1000.0)
    print("Passed: \(unpackPassed)")
    print("Failed: \(unpackFailed)")
    print("Time:   \(unpackTime)ms (\(String(format: "%.1f", unpackRate)) entries/sec)\n")

    // ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    print("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===")
    var rtUnpackedPassed = 0
    var rtUnpackedFailed = 0
    startTime = Date()

    for i in 0..<total {
        let original = unpackedList[i]

        do {
            let packed = try convertSimple(original)
            let unpacked = try convertSimple(packed)
            if unpacked == original {
                rtUnpackedPassed += 1
            } else {
                rtUnpackedFailed += 1
                if errors.count < MAX_ERRORS {
                    errors.append(TestError(phase: "rt-unp", input: original, got: "\(packed) -> \(unpacked)", expected: original))
                }
            }
        } catch {
            rtUnpackedFailed += 1
        }
    }

    let rtUnpackedTime = Int(Date().timeIntervalSince(startTime) * 1000)
    let rtUnpackedRate = Double(total) / (Double(rtUnpackedTime) / 1000.0)
    print("Passed: \(rtUnpackedPassed)")
    print("Failed: \(rtUnpackedFailed)")
    print("Time:   \(rtUnpackedTime)ms (\(String(format: "%.1f", rtUnpackedRate)) entries/sec)\n")

    // ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    print("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===")
    errors = []  // Reset to show phase 4 errors
    var rtPackedPassed = 0
    var rtPackedFailed = 0
    startTime = Date()

    for i in 0..<total {
        let original = packedList[i]

        do {
            let unpacked = try convertSimple(original)
            let repacked = try convertSimple(unpacked)
            if repacked == original {
                rtPackedPassed += 1
            } else {
                rtPackedFailed += 1
                if errors.count < MAX_ERRORS {
                    errors.append(TestError(phase: "rt-pak", input: original, got: "\(unpacked) -> \(repacked)", expected: original))
                }
            }
        } catch {
            rtPackedFailed += 1
            if errors.count < MAX_ERRORS {
                errors.append(TestError(phase: "rt-pak", input: original, got: "ERROR: \(error)", expected: original))
            }
        }
    }

    let rtPackedTime = Int(Date().timeIntervalSince(startTime) * 1000)
    let rtPackedRate = Double(total) / (Double(rtPackedTime) / 1000.0)
    print("Passed: \(rtPackedPassed)")
    print("Failed: \(rtPackedFailed)")
    print("Time:   \(rtPackedTime)ms (\(String(format: "%.1f", rtPackedRate)) entries/sec)\n")

    // ========== Summary ==========
    print("=== Summary ===")
    print(String(format: "%-30@ %10@ %18@ %12@", "Phase" as NSString, "Time (ms)" as NSString, "Rate (entries/sec)" as NSString, "Status" as NSString))
    print(String(format: "%-30@ %10@ %18@ %12@",
                 String(repeating: "-", count: 30) as NSString,
                 String(repeating: "-", count: 10) as NSString,
                 String(repeating: "-", count: 18) as NSString,
                 String(repeating: "-", count: 12) as NSString))
    print(String(format: "%-30@ %10d %18.1f %12@", "Pack" as NSString, packTime, packRate, statusString(packFailed) as NSString))
    print(String(format: "%-30@ %10d %18.1f %12@", "Unpack" as NSString, unpackTime, unpackRate, statusString(unpackFailed) as NSString))
    print(String(format: "%-30@ %10d %18.1f %12@", "Unpacked RT: unpack(pack(x))=x" as NSString, rtUnpackedTime, rtUnpackedRate, statusString(rtUnpackedFailed) as NSString))
    print(String(format: "%-30@ %10d %18.1f %12@", "Packed RT: pack(unpack(y))=y" as NSString, rtPackedTime, rtPackedRate, statusString(rtPackedFailed) as NSString))
    print("")

    // Show errors
    if !errors.isEmpty {
        print("=== First \(errors.count) errors ===")
        print(String(format: "%-8@ %-25@ %-20@ %-20@", "Phase" as NSString, "Input" as NSString, "Got" as NSString, "Expected" as NSString))
        print(String(format: "%-8@ %-25@ %-20@ %-20@",
                     String(repeating: "-", count: 8) as NSString,
                     String(repeating: "-", count: 25) as NSString,
                     String(repeating: "-", count: 20) as NSString,
                     String(repeating: "-", count: 20) as NSString))
        for e in errors {
            print(String(format: "%-8@ %-25@ %-20@ %-20@", e.phase as NSString, e.input as NSString, e.got as NSString, e.expected as NSString))
        }
    }

    // Exit with error only if pack or packed RT failed
    let totalFailed = packFailed + rtPackedFailed
    exit(totalFailed > 0 ? 1 : 0)
}

// Main entry point
func testRoundtripMain() {
    let args = Array(CommandLine.arguments.dropFirst())

    if args.isEmpty {
        fputs("Usage: test_roundtrip <csv_file>\n", stderr)
        exit(1)
    }

    if !loadTestData(csvPath: args[0]) {
        exit(1)
    }

    runRoundtripTests()
}
