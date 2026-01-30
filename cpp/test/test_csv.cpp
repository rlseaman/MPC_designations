/*
 * test_csv.cpp - Test mpc_designation against CSV file of known conversions
 *
 * Usage: test_csv <csv_file> [max_errors]
 */

#include "../src/mpc_designation.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <chrono>
#include <iomanip>

struct TestError {
    std::string input;
    std::string got;
    std::string expected;
};

bool runTests(const std::string& csvFile, int maxErrors) {
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
        std::string expectedPacked = line.substr(comma + 1);

        try {
            std::string gotPacked = mpc::MPCDesignation::convertSimple(unpacked);

            if (gotPacked != expectedPacked) {
                failed++;
                if (static_cast<int>(errors.size()) < maxErrors) {
                    errors.push_back({unpacked, gotPacked, expectedPacked});
                }
            } else {
                passed++;
            }
        } catch (const mpc::MPCDesignationError& e) {
            failed++;
            if (static_cast<int>(errors.size()) < maxErrors) {
                errors.push_back({unpacked, std::string("ERROR: ") + e.what(), expectedPacked});
            }
        }

        if (total % 100000 == 0) {
            std::cout << "Processed " << total << " entries...\n";
        }
    }

    auto endTime = std::chrono::high_resolution_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    std::cout << "\n";
    std::cout << "=== Test Results ===\n";
    std::cout << "Total:  " << total << "\n";
    std::cout << "Passed: " << passed << "\n";
    std::cout << "Failed: " << failed << "\n";
    std::cout << "Time:   " << elapsed << "ms ("
              << std::fixed << std::setprecision(1) << (total * 1000.0 / elapsed)
              << " entries/sec)\n";
    std::cout << "\n";

    if (failed > 0) {
        std::cout << "=== First " << errors.size() << " failures ===\n";
        std::cout << std::left << std::setw(25) << "Input"
                  << std::setw(15) << "Got"
                  << std::setw(15) << "Expected" << "\n";
        std::cout << std::string(60, '-') << "\n";
        for (const auto& err : errors) {
            std::cout << std::left << std::setw(25) << err.input
                      << std::setw(15) << err.got
                      << std::setw(15) << err.expected << "\n";
        }
    }

    return failed == 0;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: test_csv <csv_file> [max_errors]\n";
        return 1;
    }

    std::string csvFile = argv[1];
    int maxErrors = (argc > 2) ? std::stoi(argv[2]) : 100;

    bool success = runTests(csvFile, maxErrors);
    return success ? 0 : 1;
}
