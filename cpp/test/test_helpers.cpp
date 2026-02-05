/*
 * test_helpers.cpp - Test helper functions for MPC designation library
 * Tests the 6 helper functions with 77 test cases
 */

#include "../src/mpc_designation.hpp"
#include <iostream>
#include <string>

using namespace mpc;

int passed = 0;
int failed = 0;

void test_str(const std::string& result, const std::string& expected, const std::string& desc) {
    if (result == expected) {
        std::cout << "  PASS: \"" << result << "\"" << std::endl;
        passed++;
    } else {
        std::cout << "  FAIL: " << desc << " - expected \"" << expected << "\", got \"" << result << "\"" << std::endl;
        failed++;
    }
}

void test_bool(bool result, bool expected, const std::string& desc) {
    if (result == expected) {
        std::cout << "  PASS: " << desc << " -> " << (result ? "true" : "false") << std::endl;
        passed++;
    } else {
        std::cout << "  FAIL: " << desc << " - expected " << (expected ? "true" : "false")
                  << ", got " << (result ? "true" : "false") << std::endl;
        failed++;
    }
}

int main() {
    std::cout << "=== MPC Designation Helper Function Tests (C++) ===" << std::endl;
    std::cout << std::endl;

    // Test toReportFormat
    std::cout << "--- toReportFormat ---" << std::endl;

    // Numbered asteroids
    test_str(MPCDesignation::toReportFormat("00001"), "       00001", "Numbered asteroid 1");
    test_str(MPCDesignation::toReportFormat("00433"), "       00433", "Numbered asteroid 433");
    test_str(MPCDesignation::toReportFormat("99999"), "       99999", "Numbered asteroid 99999");
    test_str(MPCDesignation::toReportFormat("A0000"), "       A0000", "Numbered asteroid 100000");
    test_str(MPCDesignation::toReportFormat("~0000"), "       ~0000", "Numbered asteroid 620000");

    // Provisional asteroids
    test_str(MPCDesignation::toReportFormat("J95X00A"), "     J95X00A", "Provisional 1995 XA");
    test_str(MPCDesignation::toReportFormat("K24A12B"), "     K24A12B", "Provisional 2024 AB12");

    // Survey designations
    test_str(MPCDesignation::toReportFormat("PLS2040"), "     PLS2040", "Survey P-L");
    test_str(MPCDesignation::toReportFormat("T3S3141"), "     T3S3141", "Survey T-3");

    // Numbered comets
    test_str(MPCDesignation::toReportFormat("0001P"), "0001P       ", "Comet 1P");
    test_str(MPCDesignation::toReportFormat("0073P"), "0073P       ", "Comet 73P");

    // Numbered comets with fragments
    test_str(MPCDesignation::toReportFormat("0073Pa"), "0073P      a", "Comet 73P-A");
    test_str(MPCDesignation::toReportFormat("0073Pb"), "0073P      b", "Comet 73P-B");
    test_str(MPCDesignation::toReportFormat("0073Paa"), "0073P     aa", "Comet 73P-AA");
    test_str(MPCDesignation::toReportFormat("0073Paz"), "0073P     az", "Comet 73P-AZ");
    test_str(MPCDesignation::toReportFormat("0073Pzz"), "0073P     zz", "Comet 73P-ZZ");

    // Provisional comets
    test_str(MPCDesignation::toReportFormat("CJ95O010"), "    CJ95O010", "Comet C/1995 O1");
    test_str(MPCDesignation::toReportFormat("DJ93F020"), "    DJ93F020", "Comet D/1993 F2");
    test_str(MPCDesignation::toReportFormat("DJ93F02a"), "    DJ93F02a", "Comet D/1993 F2-A");

    // Test fromReportFormat
    std::cout << std::endl << "--- fromReportFormat ---" << std::endl;

    // Numbered asteroids
    test_str(MPCDesignation::fromReportFormat("       00001"), "00001", "Numbered asteroid 1");
    test_str(MPCDesignation::fromReportFormat("       00433"), "00433", "Numbered asteroid 433");
    test_str(MPCDesignation::fromReportFormat("       A0000"), "A0000", "Numbered asteroid 100000");

    // Provisional asteroids
    test_str(MPCDesignation::fromReportFormat("     J95X00A"), "J95X00A", "Provisional 1995 XA");

    // Numbered comets
    test_str(MPCDesignation::fromReportFormat("0073P       "), "0073P", "Comet 73P");

    // Numbered comets with fragments
    test_str(MPCDesignation::fromReportFormat("0073P      a"), "0073Pa", "Comet 73P-A");
    test_str(MPCDesignation::fromReportFormat("0073P     aa"), "0073Paa", "Comet 73P-AA");
    test_str(MPCDesignation::fromReportFormat("0073P     az"), "0073Paz", "Comet 73P-AZ");

    // Provisional comets
    test_str(MPCDesignation::fromReportFormat("    CJ95O010"), "CJ95O010", "Comet C/1995 O1");

    // Test hasFragment
    std::cout << std::endl << "--- hasFragment ---" << std::endl;

    // Unpacked with fragments
    test_bool(MPCDesignation::hasFragment("73P-A"), true, "Unpacked numbered comet with fragment");
    test_bool(MPCDesignation::hasFragment("73P-AA"), true, "Unpacked numbered comet with 2-letter fragment");
    test_bool(MPCDesignation::hasFragment("D/1993 F2-A"), true, "Unpacked provisional comet with fragment");
    test_bool(MPCDesignation::hasFragment("P/1930 J1-AA"), true, "Unpacked provisional comet with 2-letter fragment");

    // Unpacked without fragments
    test_bool(MPCDesignation::hasFragment("73P"), false, "Unpacked numbered comet no fragment");
    test_bool(MPCDesignation::hasFragment("C/1995 O1"), false, "Unpacked provisional comet no fragment");

    // Packed with fragments
    test_bool(MPCDesignation::hasFragment("0073Pa"), true, "Packed numbered comet with fragment");
    test_bool(MPCDesignation::hasFragment("0073Paa"), true, "Packed numbered comet with 2-letter fragment");
    test_bool(MPCDesignation::hasFragment("DJ93F02a"), true, "Packed provisional comet with fragment");

    // Packed without fragments
    test_bool(MPCDesignation::hasFragment("0073P"), false, "Packed numbered comet no fragment");
    test_bool(MPCDesignation::hasFragment("CJ95O010"), false, "Packed provisional comet no fragment");

    // Non-comets
    test_bool(MPCDesignation::hasFragment("1995 XA"), false, "Asteroid no fragment");
    test_bool(MPCDesignation::hasFragment("00001"), false, "Numbered asteroid");

    // Test getFragment
    std::cout << std::endl << "--- getFragment ---" << std::endl;

    // Unpacked with fragments
    test_str(MPCDesignation::getFragment("73P-A"), "A", "Unpacked single fragment");
    test_str(MPCDesignation::getFragment("73P-AA"), "AA", "Unpacked 2-letter fragment");
    test_str(MPCDesignation::getFragment("73P-I"), "I", "Unpacked fragment I");
    test_str(MPCDesignation::getFragment("D/1993 F2-B"), "B", "Unpacked provisional fragment");
    test_str(MPCDesignation::getFragment("P/1930 J1-AZ"), "AZ", "Unpacked provisional 2-letter");

    // Unpacked without fragments
    test_str(MPCDesignation::getFragment("73P"), "", "Unpacked no fragment");
    test_str(MPCDesignation::getFragment("C/1995 O1"), "", "Unpacked provisional no fragment");

    // Packed with fragments
    test_str(MPCDesignation::getFragment("0073Pa"), "A", "Packed single fragment");
    test_str(MPCDesignation::getFragment("0073Paa"), "AA", "Packed 2-letter fragment");
    test_str(MPCDesignation::getFragment("0073Pi"), "I", "Packed fragment I");
    test_str(MPCDesignation::getFragment("DJ93F02b"), "B", "Packed provisional fragment");

    // Packed without fragments
    test_str(MPCDesignation::getFragment("0073P"), "", "Packed no fragment");
    test_str(MPCDesignation::getFragment("CJ95O010"), "", "Packed provisional no fragment");

    // Test getParent
    std::cout << std::endl << "--- getParent ---" << std::endl;

    // Unpacked with fragments
    test_str(MPCDesignation::getParent("73P-A"), "73P", "Unpacked single fragment");
    test_str(MPCDesignation::getParent("73P-AA"), "73P", "Unpacked 2-letter fragment");
    test_str(MPCDesignation::getParent("D/1993 F2-B"), "D/1993 F2", "Unpacked provisional fragment");
    test_str(MPCDesignation::getParent("P/1930 J1-AA"), "P/1930 J1", "Unpacked provisional 2-letter");

    // Unpacked without fragments
    test_str(MPCDesignation::getParent("73P"), "73P", "Unpacked no fragment");
    test_str(MPCDesignation::getParent("C/1995 O1"), "C/1995 O1", "Unpacked provisional no fragment");

    // Packed with fragments
    test_str(MPCDesignation::getParent("0073Pa"), "0073P", "Packed single fragment");
    test_str(MPCDesignation::getParent("0073Paa"), "0073P", "Packed 2-letter fragment");

    // Packed without fragments
    test_str(MPCDesignation::getParent("0073P"), "0073P", "Packed no fragment");

    // Non-comets (should return as-is)
    test_str(MPCDesignation::getParent("1995 XA"), "1995 XA", "Asteroid");
    test_str(MPCDesignation::getParent("00001"), "00001", "Numbered asteroid");

    // Test designationsEqual
    std::cout << std::endl << "--- designationsEqual ---" << std::endl;

    // Same designation, different formats
    test_bool(MPCDesignation::designationsEqual("1995 XA", "J95X00A"), true, "Provisional packed/unpacked");
    test_bool(MPCDesignation::designationsEqual("73P", "0073P"), true, "Numbered comet packed/unpacked");
    test_bool(MPCDesignation::designationsEqual("73P-A", "0073Pa"), true, "Comet with fragment packed/unpacked");
    test_bool(MPCDesignation::designationsEqual("73P-AA", "0073Paa"), true, "Comet with 2-letter fragment");
    test_bool(MPCDesignation::designationsEqual("1", "00001"), true, "Numbered asteroid");
    test_bool(MPCDesignation::designationsEqual("C/1995 O1", "CJ95O010"), true, "Provisional comet");

    // Different designations
    test_bool(MPCDesignation::designationsEqual("1995 XA", "1995 XB"), false, "Different provisional");
    test_bool(MPCDesignation::designationsEqual("73P-A", "73P-B"), false, "Different fragments");
    test_bool(MPCDesignation::designationsEqual("73P", "74P"), false, "Different comet numbers");
    test_bool(MPCDesignation::designationsEqual("1", "2"), false, "Different asteroid numbers");

    // Same designation (both packed or both unpacked)
    test_bool(MPCDesignation::designationsEqual("1995 XA", "1995 XA"), true, "Same unpacked");
    test_bool(MPCDesignation::designationsEqual("J95X00A", "J95X00A"), true, "Same packed");

    // Summary
    std::cout << std::endl;
    std::cout << "==================================================" << std::endl;
    std::cout << "Total: " << (passed + failed) << ", Passed: " << passed << ", Failed: " << failed << std::endl;

    return (failed > 0) ? 1 : 0;
}
