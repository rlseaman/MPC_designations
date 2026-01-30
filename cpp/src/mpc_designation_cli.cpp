/*
 * mpc_designation_cli.cpp - CLI for MPC designation converter
 */

#include "mpc_designation.hpp"
#include <iostream>
#include <vector>
#include <string>

void printUsage() {
    std::cerr << R"(Usage: mpc_designation [-v|--verbose] <designation> [designation ...]

Convert between packed and unpacked MPC designations.
Auto-detects the input format and converts to the other.

Options:
  -v, --verbose   Show detailed information about the conversion
  --version       Show version information

Examples:
  mpc_designation 00001             -> 1
  mpc_designation 1                 -> 00001
  mpc_designation J95X00A           -> 1995 XA
  mpc_designation '1995 XA'         -> J95X00A
  mpc_designation 'C/1995 O1'       -> CJ95O010
  mpc_designation 1P                -> 0001P
)";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage();
        return 1;
    }

    bool verbose = false;
    std::vector<std::string> designations;

    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "-v" || arg == "--verbose") {
            verbose = true;
        } else if (arg == "-h" || arg == "--help") {
            printUsage();
            return 0;
        } else if (arg == "--version") {
            std::cout << "mpc_designation " << mpc::VERSION << "\n";
            return 0;
        } else {
            designations.push_back(arg);
        }
    }

    if (designations.empty()) {
        printUsage();
        return 1;
    }

    bool multiple = designations.size() > 1;

    for (const auto& des : designations) {
        try {
            auto result = mpc::MPCDesignation::convert(des);

            if (verbose) {
                std::cout << "  Input:    " << des << "\n";
                std::cout << "  Detected: "
                          << (result.info.format == mpc::Format::Packed ? "packed" : "unpacked")
                          << " format, " << result.info.subtype << "\n";
                std::string action = (result.info.format == mpc::Format::Packed)
                    ? "unpacking to human-readable form"
                    : "packing to MPC compact form";
                std::cout << "  Action:   " << action << "\n";
                std::cout << "  Output:   " << result.output << "\n";
                if (multiple) {
                    std::cout << "\n";
                }
            } else if (multiple) {
                std::cout << des << " -> " << result.output << "\n";
            } else {
                std::cout << result.output << "\n";
            }
        } catch (const mpc::MPCDesignationError& e) {
            std::cerr << "Error: " << e.what() << "\n";
            return 1;
        }
    }

    return 0;
}
