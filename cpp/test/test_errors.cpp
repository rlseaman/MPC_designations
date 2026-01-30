/*
 * test_errors.cpp - Test mpc_designation error handling
 *
 * Usage: test_errors [error_test_cases.csv]
 */

#include "../src/mpc_designation.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>

std::string unescapeString(const std::string& s) {
    std::string result;
    for (size_t i = 0; i < s.length(); i++) {
        if (s[i] == '\\' && i + 1 < s.length()) {
            char next = s[i + 1];
            if (next == 'x' && i + 3 < s.length()) {
                // Hex escape \xNN
                std::string hex = s.substr(i + 2, 2);
                try {
                    int val = std::stoi(hex, nullptr, 16);
                    result += static_cast<char>(val);
                    i += 3;
                    continue;
                } catch (...) {
                    // Fall through
                }
            }
            switch (next) {
                case 'n': result += '\n'; i++; break;
                case 'r': result += '\r'; i++; break;
                case 't': result += '\t'; i++; break;
                case 'f': result += '\f'; i++; break;
                case 'v': result += '\v'; i++; break;
                case '0': result += '\0'; i++; break;
                case '\\': result += '\\'; i++; break;
                default: result += s[i]; break;
            }
        } else {
            result += s[i];
        }
    }
    return result;
}

bool runErrorTests(const std::string& csvFile) {
    std::ifstream fp(csvFile);
    if (!fp.is_open()) {
        std::cerr << "Error: Cannot open file: " << csvFile << "\n";
        return false;
    }

    int total = 0;
    int passed = 0;
    int failed = 0;

    std::cout << "=== MPC Designation Error Tests ===\n\n";

    std::string line;
    while (std::getline(fp, line)) {
        // Trim trailing whitespace
        while (!line.empty() && (line.back() == '\r' || line.back() == '\n')) {
            line.pop_back();
        }

        // Skip empty lines and comments
        if (line.empty() || line[0] == '#') continue;

        // Skip header
        if (line.substr(0, 9) == "category,") continue;

        // Parse CSV (simple split by comma, limit to 5 parts)
        std::vector<std::string> parts;
        std::stringstream ss(line);
        std::string part;
        int count = 0;
        while (std::getline(ss, part, ',') && count < 5) {
            parts.push_back(part);
            count++;
            if (count == 4) {
                // Get the rest as description
                std::string rest;
                std::getline(ss, rest);
                if (!rest.empty()) {
                    parts.push_back(rest);
                }
                break;
            }
        }

        if (parts.size() < 5) continue;

        std::string category = parts[0];
        std::string subcategory = parts[1];
        std::string inputStr = unescapeString(parts[2]);
        std::string expectedError = parts[3];
        std::string description = parts[4];

        total++;

        bool gotError = false;
        std::string errorMsg;
        std::string output;

        try {
            output = mpc::MPCDesignation::convertSimple(inputStr);
        } catch (const mpc::MPCDesignationError& e) {
            gotError = true;
            errorMsg = e.what();
        } catch (const std::exception& e) {
            gotError = true;
            errorMsg = e.what();
        }

        bool testPassed = false;

        if (expectedError == "valid") {
            if (!gotError) {
                testPassed = true;
            } else {
                std::cout << "FAIL [" << category << "/" << subcategory << "]: '" << description << "'\n";
                std::cout << "      Expected: valid conversion\n";
                std::cout << "      Got:      " << errorMsg << "\n";
                failed++;
            }
        } else {
            if (gotError) {
                testPassed = true;
            } else {
                std::cout << "FAIL [" << category << "/" << subcategory << "]: '" << description << "'\n";
                std::cout << "      Expected: error (" << expectedError << ")\n";
                std::cout << "      Got:      '" << output << "' (success)\n";
                failed++;
            }
        }

        if (testPassed) {
            passed++;
        }
    }

    std::cout << "\n=== Error Test Results ===\n";
    std::cout << "Total:  " << total << "\n";
    std::cout << "Passed: " << passed << "\n";
    std::cout << "Failed: " << failed << "\n";

    return failed == 0;
}

int main(int argc, char* argv[]) {
    std::string csvFile = (argc > 1) ? argv[1] : "error_test_cases.csv";

    bool success = runErrorTests(csvFile);
    return success ? 0 : 1;
}
