/*
 * mpc_designation.hpp - Convert between packed and unpacked MPC designations
 *
 * C++ implementation based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 *
 * Supports asteroids, comets, and natural satellites.
 */

#ifndef MPC_DESIGNATION_HPP
#define MPC_DESIGNATION_HPP

#include <string>
#include <stdexcept>

namespace mpc {

// Version
constexpr const char* VERSION = "1.0.0";

// Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
constexpr long MAX_ASTEROID_NUMBER = 15396335L;

// Exception class for MPC designation errors
class MPCDesignationError : public std::runtime_error {
public:
    explicit MPCDesignationError(const std::string& msg) : std::runtime_error(msg) {}
};

// Format type
enum class Format {
    Unknown,
    Packed,
    Unpacked
};

// Designation type
enum class Type {
    Unknown,
    Permanent,
    Provisional,
    ProvisionalExtended,
    Survey,
    CometNumbered,
    CometProvisional,
    CometFull,
    CometAncient,
    CometBCE,
    Satellite
};

// Format detection result
struct FormatInfo {
    Format format = Format::Unknown;
    Type type = Type::Unknown;
    std::string subtype;
};

// Conversion result
struct ConversionResult {
    std::string input;
    std::string output;
    FormatInfo info;
};

// Main converter class
class MPCDesignation {
public:
    // High-level conversion functions

    // Convert between packed and unpacked formats (auto-detect)
    static ConversionResult convert(const std::string& designation);

    // Simple conversion - returns just the output string
    static std::string convertSimple(const std::string& designation);

    // Ensure packed format (no-op if already packed)
    static std::string pack(const std::string& designation);

    // Ensure unpacked format (no-op if already unpacked)
    static std::string unpack(const std::string& designation);

    // Format detection
    static FormatInfo detectFormat(const std::string& designation);

    // Validation
    static bool isValid(const std::string& designation) noexcept;

    // Helper functions for format conversion and fragment handling
    static std::string toReportFormat(const std::string& minimal);
    static std::string fromReportFormat(const std::string& report);
    static bool hasFragment(const std::string& designation);
    static std::string getFragment(const std::string& designation);
    static std::string getParent(const std::string& designation);
    static bool designationsEqual(const std::string& d1, const std::string& d2);

    // Low-level functions for permanent asteroids
    static std::string packPermanent(long number);
    static long unpackPermanent(const std::string& packed);

    // Low-level functions for provisional designations
    static std::string packProvisional(const std::string& unpacked);
    static std::string unpackProvisional(const std::string& packed);

private:
    // Internal utilities
    static int base62ToNum(char c);
    static char numToBase62(int n);
    static long base62StringToNum(const std::string& s);
    static std::string numToBase62String(long n, int width);

    static int centuryToCode(int century);
    static int codeToCentury(char code);

    static int letterToPosition(char letter);
    static char positionToLetter(int pos);

    static int decodeCycleCount(const std::string& encoded);
    static std::string encodeCycleCount(int count);

    static bool isCometType(char c);
    static bool isSatellitePlanet(char c);
    static bool isValidHalfMonth(char c);

    static std::string trim(const std::string& s);
    static void validateRawInput(const std::string& s);
    static void validateWhitespace(const std::string& s);

    // Comet functions
    static std::string packCometProvisional(const std::string& unpacked);
    static std::string unpackCometProvisional(const std::string& packed);
    static std::string packCometNumbered(const std::string& unpacked);
    static std::string unpackCometNumbered(const std::string& packed);
    static std::string packCometFull(const std::string& unpacked);
    static std::string unpackCometFull(const std::string& packed);
    static std::string packAncientComet(const std::string& unpacked);
    static std::string unpackAncientComet(const std::string& packed);

    // Satellite functions
    static std::string packSatellite(const std::string& unpacked);
    static std::string unpackSatellite(const std::string& packed);

    // BCE year encoding
    static std::pair<char, std::string> encodeBCEYear(int year);
    static int decodeBCEYear(char prefix, const std::string& code);

    // Helper for comet format detection
    static bool isAsteroidStylePacked(const std::string& provisional);
    static bool isAsteroidStyleUnpacked(const std::string& provisional);

    // Type descriptions
    static std::string getCometTypeName(char type);
    static std::string getPlanetName(char code);
};

} // namespace mpc

#endif // MPC_DESIGNATION_HPP
