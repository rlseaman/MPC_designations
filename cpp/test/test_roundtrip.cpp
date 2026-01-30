/*
 * test_roundtrip.cpp - Test mpc_designation roundtrip conversion
 *
 * Usage: test_roundtrip <csv_file> [max_errors]
 */

#include "../src/mpc_designation.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <chrono>
#include <iomanip>
#include <regex>

struct TestError {
    std::string test;
    std::string input;
    std::string step1;
    std::string step2;
    std::string expected;
};

bool runRoundtripTests(const std::string& csvFile, int maxErrors) {
    std::ifstream fp(csvFile);
    if (!fp.is_open()) {
        std::cerr << "Error: Cannot open file: " << csvFile << "\n";
        return false;
    }

    long total = 0;
    long passed = 0;
    long failed = 0;
    std::vector<TestError> errors;

    auto startTime = std::chrono::high_resolution_clock::now();

    std::string line;
    // Skip header
    std::getline(fp, line);

    std::regex oldStyleRe(R"(^[AB]\d{3} [A-Z][A-Z]$)");

    while (std::getline(fp, line)) {
        // Trim trailing whitespace
        while (!line.empty() && (line.back() == '\r' || line.back() == '\n')) {
            line.pop_back();
        }
        if (line.empty()) continue;

        // Skip header if encountered again
        if (line.substr(0, 8) == "unpacked") continue;

        total++;

        size_t comma = line.find(',');
        if (comma == std::string::npos) continue;

        std::string unpacked = line.substr(0, comma);
        std::string packed = line.substr(comma + 1);

        // Test packed -> unpacked -> packed roundtrip
        try {
            std::string toUnpacked = mpc::MPCDesignation::unpack(packed);
            std::string backToPacked = mpc::MPCDesignation::pack(toUnpacked);

            if (backToPacked != packed) {
                failed++;
                if (static_cast<int>(errors.size()) < maxErrors) {
                    errors.push_back({"packed roundtrip", packed, toUnpacked, backToPacked, packed});
                }
                continue;
            }
        } catch (const mpc::MPCDesignationError& e) {
            failed++;
            if (static_cast<int>(errors.size()) < maxErrors) {
                errors.push_back({"packed roundtrip", packed, "ERROR", e.what(), packed});
            }
            continue;
        }

        // Test unpacked -> packed -> unpacked roundtrip
        try {
            std::string toPacked = mpc::MPCDesignation::pack(unpacked);
            std::string backToUnpacked = mpc::MPCDesignation::unpack(toPacked);

            // Check for old-style designation
            bool isOldStyle = std::regex_match(unpacked, oldStyleRe);

            if (!isOldStyle && backToUnpacked != unpacked) {
                failed++;
                if (static_cast<int>(errors.size()) < maxErrors) {
                    errors.push_back({"unpacked roundtrip", unpacked, toPacked, backToUnpacked, unpacked});
                }
                continue;
            }

            // For old-style, verify the modern form roundtrips
            if (isOldStyle) {
                std::string repackedModern = mpc::MPCDesignation::pack(backToUnpacked);
                if (repackedModern != toPacked) {
                    failed++;
                    if (static_cast<int>(errors.size()) < maxErrors) {
                        errors.push_back({"old-style modern roundtrip", unpacked, backToUnpacked, repackedModern, toPacked});
                    }
                    continue;
                }
            }
        } catch (const mpc::MPCDesignationError& e) {
            failed++;
            if (static_cast<int>(errors.size()) < maxErrors) {
                errors.push_back({"unpacked roundtrip", unpacked, "ERROR", e.what(), unpacked});
            }
            continue;
        }

        passed++;

        if (total % 100000 == 0) {
            std::cout << "Processed " << total << " entries...\n";
        }
    }

    auto endTime = std::chrono::high_resolution_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    std::cout << "\n";
    std::cout << "=== Roundtrip Test Results ===\n";
    std::cout << "Total:  " << total << "\n";
    std::cout << "Passed: " << passed << "\n";
    std::cout << "Failed: " << failed << "\n";
    std::cout << "Time:   " << elapsed << "ms ("
              << std::fixed << std::setprecision(1) << (total * 1000.0 / elapsed)
              << " entries/sec)\n";
    std::cout << "\n";

    if (failed > 0) {
        std::cout << "=== First " << errors.size() << " failures ===\n";
        std::cout << std::left << std::setw(20) << "Test"
                  << std::setw(20) << "Input"
                  << std::setw(15) << "Step1"
                  << std::setw(20) << "Step2"
                  << std::setw(15) << "Expected" << "\n";
        std::cout << std::string(90, '-') << "\n";
        for (const auto& err : errors) {
            std::cout << std::left << std::setw(20) << err.test
                      << std::setw(20) << err.input
                      << std::setw(15) << err.step1
                      << std::setw(20) << err.step2
                      << std::setw(15) << err.expected << "\n";
        }
    }

    return failed == 0;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: test_roundtrip <csv_file> [max_errors]\n";
        return 1;
    }

    std::string csvFile = argv[1];
    int maxErrors = (argc > 2) ? std::stoi(argv[2]) : 100;

    bool success = runRoundtripTests(csvFile, maxErrors);
    return success ? 0 : 1;
}
