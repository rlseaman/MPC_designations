/*
 * test_roundtrip.cpp - Test mpc_designation with bidirectional timing and round-trip verification
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
 *
 * Usage: test_roundtrip <csv_file>
 */

#include "../src/mpc_designation.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <chrono>
#include <iomanip>
#include <regex>

struct TestCase {
    std::string unpacked;
    std::string packed;
};

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: test_roundtrip <csv_file>\n";
        return 1;
    }

    std::string csvFile = argv[1];

    // Load test data
    std::ifstream fp(csvFile);
    if (!fp.is_open()) {
        std::cerr << "Error: Cannot open file: " << csvFile << "\n";
        return 1;
    }

    std::vector<TestCase> testCases;
    std::string line;

    // Skip header
    std::getline(fp, line);

    while (std::getline(fp, line)) {
        while (!line.empty() && (line.back() == '\r' || line.back() == '\n')) {
            line.pop_back();
        }
        if (line.empty()) continue;

        size_t comma = line.find(',');
        if (comma == std::string::npos) continue;

        TestCase tc;
        tc.unpacked = line.substr(0, comma);
        tc.packed = line.substr(comma + 1);
        testCases.push_back(tc);
    }
    fp.close();

    std::cout << "Loaded " << testCases.size() << " test cases\n\n";

    long packPassed = 0, packFailed = 0;
    long unpackPassed = 0, unpackFailed = 0;
    long rtUnpackedPassed = 0, rtUnpackedFailed = 0;
    long rtPackedPassed = 0, rtPackedFailed = 0;

    // ========== Phase 1: Pack (unpacked -> packed) ==========
    std::cout << "=== Phase 1: Pack (unpacked -> packed) ===\n";
    auto startTime = std::chrono::high_resolution_clock::now();

    for (const auto& tc : testCases) {
        try {
            std::string got = mpc::MPCDesignation::pack(tc.unpacked);
            if (got == tc.packed) {
                packPassed++;
            } else {
                packFailed++;
            }
        } catch (...) {
            packFailed++;
        }
    }

    auto endTime = std::chrono::high_resolution_clock::now();
    auto packTime = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();
    double packRate = testCases.size() * 1000.0 / packTime;

    std::cout << "Passed: " << packPassed << "\n";
    std::cout << "Failed: " << packFailed << "\n";
    std::cout << "Time:   " << packTime << "ms ("
              << std::fixed << std::setprecision(1) << packRate << " entries/sec)\n\n";

    // ========== Phase 2: Unpack (packed -> unpacked) ==========
    std::cout << "=== Phase 2: Unpack (packed -> unpacked) ===\n";
    startTime = std::chrono::high_resolution_clock::now();

    for (const auto& tc : testCases) {
        try {
            std::string got = mpc::MPCDesignation::unpack(tc.packed);
            if (got == tc.unpacked) {
                unpackPassed++;
            } else {
                unpackFailed++;
            }
        } catch (...) {
            unpackFailed++;
        }
    }

    endTime = std::chrono::high_resolution_clock::now();
    auto unpackTime = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();
    double unpackRate = testCases.size() * 1000.0 / unpackTime;

    std::cout << "Passed: " << unpackPassed << "\n";
    std::cout << "Failed: " << unpackFailed << "\n";
    std::cout << "Time:   " << unpackTime << "ms ("
              << std::fixed << std::setprecision(1) << unpackRate << " entries/sec)\n\n";

    // ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    std::cout << "=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===\n";
    startTime = std::chrono::high_resolution_clock::now();

    for (const auto& tc : testCases) {
        try {
            std::string packed = mpc::MPCDesignation::pack(tc.unpacked);
            std::string backToUnpacked = mpc::MPCDesignation::unpack(packed);
            if (backToUnpacked == tc.unpacked) {
                rtUnpackedPassed++;
            } else {
                rtUnpackedFailed++;
            }
        } catch (...) {
            rtUnpackedFailed++;
        }
    }

    endTime = std::chrono::high_resolution_clock::now();
    auto rtUnpackedTime = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();
    double rtUnpackedRate = testCases.size() * 1000.0 / rtUnpackedTime;

    std::cout << "Passed: " << rtUnpackedPassed << "\n";
    std::cout << "Failed: " << rtUnpackedFailed << "\n";
    std::cout << "Time:   " << rtUnpackedTime << "ms ("
              << std::fixed << std::setprecision(1) << rtUnpackedRate << " entries/sec)\n\n";

    // ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    std::cout << "=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===\n";
    startTime = std::chrono::high_resolution_clock::now();

    for (const auto& tc : testCases) {
        try {
            std::string unpacked = mpc::MPCDesignation::unpack(tc.packed);
            std::string backToPacked = mpc::MPCDesignation::pack(unpacked);
            if (backToPacked == tc.packed) {
                rtPackedPassed++;
            } else {
                rtPackedFailed++;
            }
        } catch (...) {
            rtPackedFailed++;
        }
    }

    endTime = std::chrono::high_resolution_clock::now();
    auto rtPackedTime = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();
    double rtPackedRate = testCases.size() * 1000.0 / rtPackedTime;

    std::cout << "Passed: " << rtPackedPassed << "\n";
    std::cout << "Failed: " << rtPackedFailed << "\n";
    std::cout << "Time:   " << rtPackedTime << "ms ("
              << std::fixed << std::setprecision(1) << rtPackedRate << " entries/sec)\n\n";

    // ========== Summary ==========
    std::cout << "=== Summary ===\n";
    std::cout << "Pack:       " << (packFailed == 0 ? "PASS" : "FAIL") << "\n";
    std::cout << "Unpack:     " << (unpackFailed == 0 ? "PASS" : "FAIL (" + std::to_string(unpackFailed) + ")") << "\n";
    std::cout << "Unpacked RT: " << (rtUnpackedFailed == 0 ? "PASS" : "FAIL (" + std::to_string(rtUnpackedFailed) + ")") << "\n";
    std::cout << "Packed RT:   " << (rtPackedFailed == 0 ? "PASS" : "FAIL (" + std::to_string(rtPackedFailed) + ")") << "\n";

    // Exit with error only if pack or packed RT failed
    return (packFailed > 0 || rtPackedFailed > 0) ? 1 : 0;
}
