/*
 * mpc_designation.cpp - Convert between packed and unpacked MPC designations
 *
 * C++ implementation based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 */

#include "mpc_designation.hpp"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <sstream>
#include <regex>

namespace mpc {

// Constants
static const char BASE62_CHARS[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const char CENTURY_LETTERS[] = "ABCDEFGHIJKL";
static const char COMET_TYPES[] = "PCDXAI";
static const char SATELLITE_PLANETS[] = "JSUN";

// =========================================================================
// Utility functions
// =========================================================================

std::string MPCDesignation::trim(const std::string& s) {
    size_t start = s.find_first_not_of(' ');
    if (start == std::string::npos) return "";
    size_t end = s.find_last_not_of(' ');
    return s.substr(start, end - start + 1);
}

void MPCDesignation::validateRawInput(const std::string& s) {
    for (char c : s) {
        unsigned char uc = static_cast<unsigned char>(c);
        if (uc < 32 || uc > 126) {
            char buf[64];
            snprintf(buf, sizeof(buf), "Invalid character in designation: '\\x%02x'", uc);
            throw MPCDesignationError(buf);
        }
    }
}

void MPCDesignation::validateWhitespace(const std::string& s) {
    bool prevSpace = false;
    for (char c : s) {
        unsigned char uc = static_cast<unsigned char>(c);
        if (uc < 32 || uc > 126) {
            throw MPCDesignationError("Invalid character in designation");
        }
        if (c == ' ') {
            if (prevSpace) {
                throw MPCDesignationError("Consecutive spaces in designation");
            }
            prevSpace = true;
        } else {
            prevSpace = false;
        }
    }
}

int MPCDesignation::base62ToNum(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    if (c >= 'a' && c <= 'z') return c - 'a' + 36;
    throw MPCDesignationError(std::string("Invalid base-62 character: ") + c);
}

char MPCDesignation::numToBase62(int n) {
    if (n < 0 || n > 61) {
        throw MPCDesignationError("Number out of base-62 range: " + std::to_string(n));
    }
    return BASE62_CHARS[n];
}

long MPCDesignation::base62StringToNum(const std::string& s) {
    long result = 0;
    for (char c : s) {
        result = result * 62 + base62ToNum(c);
    }
    return result;
}

std::string MPCDesignation::numToBase62String(long n, int width) {
    std::string result(width, '0');
    for (int i = width - 1; i >= 0; i--) {
        result[i] = numToBase62(n % 62);
        n /= 62;
    }
    return result;
}

int MPCDesignation::centuryToCode(int century) {
    if (century < 10 || century > 21) return -1;
    return CENTURY_LETTERS[century - 10];
}

int MPCDesignation::codeToCentury(char code) {
    if (code >= 'A' && code <= 'L') return code - 'A' + 10;
    return -1;
}

int MPCDesignation::letterToPosition(char letter) {
    if (letter < 'A' || letter > 'Z') {
        throw MPCDesignationError(std::string("Invalid half-month letter: ") + letter);
    }
    int pos = letter - 'A' + 1;
    if (letter > 'I') pos--;
    return pos;
}

char MPCDesignation::positionToLetter(int pos) {
    if (pos < 1 || pos > 25) {
        throw MPCDesignationError("Invalid letter position: " + std::to_string(pos));
    }
    int p = pos;
    if (p >= 9) p++;
    return 'A' + p - 1;
}

int MPCDesignation::decodeCycleCount(const std::string& encoded) {
    if (encoded.length() < 2) {
        throw MPCDesignationError("Invalid cycle count encoding");
    }

    char first = encoded[0];
    char second = encoded[1];
    int tens;

    if (first >= '0' && first <= '9') {
        tens = first - '0';
    } else if (first >= 'A' && first <= 'Z') {
        tens = first - 'A' + 10;
    } else if (first >= 'a' && first <= 'z') {
        tens = first - 'a' + 36;
    } else {
        throw MPCDesignationError("Invalid cycle count encoding");
    }

    if (second < '0' || second > '9') {
        throw MPCDesignationError("Invalid cycle count encoding");
    }

    return tens * 10 + (second - '0');
}

std::string MPCDesignation::encodeCycleCount(int count) {
    if (count < 0 || count >= 620) {
        throw MPCDesignationError("Cycle count out of range (0-619): " + std::to_string(count));
    }

    int tens = count / 10;
    int ones = count % 10;
    char first;

    if (tens < 10) {
        first = '0' + tens;
    } else if (tens < 36) {
        first = 'A' + tens - 10;
    } else {
        first = 'a' + tens - 36;
    }

    return std::string(1, first) + static_cast<char>('0' + ones);
}

bool MPCDesignation::isCometType(char c) {
    return std::strchr(COMET_TYPES, c) != nullptr;
}

bool MPCDesignation::isSatellitePlanet(char c) {
    return std::strchr(SATELLITE_PLANETS, c) != nullptr;
}

bool MPCDesignation::isValidHalfMonth(char c) {
    return c >= 'A' && c <= 'Y' && c != 'I';
}

std::string MPCDesignation::getCometTypeName(char type) {
    switch (type) {
        case 'P': return "periodic";
        case 'C': return "non-periodic";
        case 'D': return "defunct";
        case 'X': return "uncertain orbit";
        case 'A': return "asteroid with comet designation";
        case 'I': return "interstellar";
        default: return "unknown";
    }
}

std::string MPCDesignation::getPlanetName(char code) {
    switch (code) {
        case 'J': return "Jupiter";
        case 'S': return "Saturn";
        case 'U': return "Uranus";
        case 'N': return "Neptune";
        default: return "unknown";
    }
}

// =========================================================================
// Permanent (numbered) asteroid designations
// =========================================================================

std::string MPCDesignation::packPermanent(long number) {
    if (number < 1 || number > MAX_ASTEROID_NUMBER) {
        throw MPCDesignationError("Invalid asteroid number: " + std::to_string(number));
    }

    char buf[16];
    if (number < 100000) {
        snprintf(buf, sizeof(buf), "%05ld", number);
    } else if (number < 620000) {
        int div = number / 10000;
        int mod = number % 10000;
        char letter;
        if (div < 36) {
            letter = 'A' + div - 10;
        } else {
            letter = 'a' + div - 36;
        }
        snprintf(buf, sizeof(buf), "%c%04d", letter, mod);
    } else {
        long offset = number - 620000;
        buf[0] = '~';
        std::string encoded = numToBase62String(offset, 4);
        std::strcpy(buf + 1, encoded.c_str());
    }
    return std::string(buf);
}

long MPCDesignation::unpackPermanent(const std::string& packed) {
    std::string p = trim(packed);
    if (p.length() != 5) {
        throw MPCDesignationError("Invalid packed permanent designation length");
    }

    char first = p[0];

    // Tilde format (>= 620,000)
    if (first == '~') {
        return 620000 + base62StringToNum(p.substr(1, 4));
    }

    // Simple numeric format (< 100,000)
    if (first >= '0' && first <= '9') {
        return std::stol(p);
    }

    // Extended format with uppercase letter (100,000 - 359,999)
    if (first >= 'A' && first <= 'Z') {
        int val = first - 'A' + 10;
        return val * 10000L + std::stol(p.substr(1, 4));
    }

    // Extended format with lowercase letter (360,000 - 619,999)
    if (first >= 'a' && first <= 'z') {
        int val = first - 'a' + 36;
        return val * 10000L + std::stol(p.substr(1, 4));
    }

    throw MPCDesignationError("Invalid packed permanent designation");
}

// =========================================================================
// Provisional asteroid designations
// =========================================================================

std::string MPCDesignation::packProvisional(const std::string& unpacked) {
    std::string u = trim(unpacked);

    // Check for survey designations
    std::regex surveyRe(R"(^(\d+) (P-L|T-[123])$)");
    std::smatch surveyMatch;
    if (std::regex_match(u, surveyMatch, surveyRe)) {
        int number = std::stoi(surveyMatch[1].str());
        std::string survey = surveyMatch[2].str();
        if (number < 1) {
            throw MPCDesignationError("Survey number must be positive");
        }
        char buf[16];
        if (survey == "P-L") {
            snprintf(buf, sizeof(buf), "PLS%04d", number);
        } else if (survey == "T-1") {
            snprintf(buf, sizeof(buf), "T1S%04d", number);
        } else if (survey == "T-2") {
            snprintf(buf, sizeof(buf), "T2S%04d", number);
        } else {
            snprintf(buf, sizeof(buf), "T3S%04d", number);
        }
        return std::string(buf);
    }

    // Check for old-style designation: "A908 CJ" or "B842 FA"
    std::regex oldStyleRe(R"(^([AB])(\d)(\d{2}) ([A-Z])([A-Z])$)");
    std::smatch oldMatch;
    if (std::regex_match(u, oldMatch, oldStyleRe)) {
        int centuryDigit = std::stoi(oldMatch[2].str());
        int yearShort = std::stoi(oldMatch[3].str());
        char halfMonth = oldMatch[4].str()[0];
        char secondLetter = oldMatch[5].str()[0];

        char centuryCode;
        if (centuryDigit == 8) centuryCode = 'I';
        else if (centuryDigit == 9) centuryCode = 'J';
        else if (centuryDigit == 0) centuryCode = 'K';
        else throw MPCDesignationError("Invalid century digit in old-style designation");

        char buf[16];
        snprintf(buf, sizeof(buf), "%c%02d%c00%c", centuryCode, yearShort, halfMonth, secondLetter);
        return std::string(buf);
    }

    // Standard provisional: "1995 XA" or "1995 XA12"
    std::regex provRe(R"(^(\d{4}) ([A-Z])([A-Z])(\d*)$)");
    std::smatch provMatch;
    if (!std::regex_match(u, provMatch, provRe)) {
        throw MPCDesignationError("Invalid unpacked provisional designation: " + u);
    }

    int year = std::stoi(provMatch[1].str());
    char halfMonth = provMatch[2].str()[0];
    char secondLetter = provMatch[3].str()[0];
    std::string orderStr = provMatch[4].str();

    if (!isValidHalfMonth(halfMonth)) {
        throw MPCDesignationError(std::string("Invalid half-month letter: ") + halfMonth);
    }

    int century = year / 100;
    int yearShort = year % 100;

    int centuryCodeInt = centuryToCode(century);
    if (centuryCodeInt < 0) {
        throw MPCDesignationError("Invalid century in year: " + std::to_string(year));
    }
    char centuryCode = static_cast<char>(centuryCodeInt);

    // Asteroid provisionals only valid for I-L (1800-2199)
    if (centuryCode < 'I') {
        throw MPCDesignationError("Asteroid provisional year must be >= 1800: " + std::to_string(year));
    }

    long orderNum = 0;
    if (!orderStr.empty()) {
        try {
            orderNum = std::stol(orderStr);
        } catch (...) {
            throw MPCDesignationError("Cycle count out of range (overflow): " + orderStr);
        }
    }

    // Check if we need extended format
    if (orderNum >= 620) {
        int yearDigit = year % 100;
        long baseSequence = (orderNum - 620) * 25 + letterToPosition(secondLetter) - 1;
        std::string seqEncoded = numToBase62String(baseSequence, 4);
        char buf[16];
        snprintf(buf, sizeof(buf), "_%c%c%s", numToBase62(yearDigit), halfMonth, seqEncoded.c_str());
        return std::string(buf);
    }

    std::string orderEncoded = encodeCycleCount(static_cast<int>(orderNum));
    char buf[16];
    snprintf(buf, sizeof(buf), "%c%02d%c%s%c", centuryCode, yearShort, halfMonth, orderEncoded.c_str(), secondLetter);
    return std::string(buf);
}

std::string MPCDesignation::unpackProvisional(const std::string& packed) {
    std::string p = trim(packed);

    // Check for survey designations
    if (p.length() == 7) {
        if (p.substr(0, 3) == "PLS") {
            int num = std::stoi(p.substr(3, 4));
            return std::to_string(num) + " P-L";
        } else if (p.substr(0, 3) == "T1S") {
            int num = std::stoi(p.substr(3, 4));
            return std::to_string(num) + " T-1";
        } else if (p.substr(0, 3) == "T2S") {
            int num = std::stoi(p.substr(3, 4));
            return std::to_string(num) + " T-2";
        } else if (p.substr(0, 3) == "T3S") {
            int num = std::stoi(p.substr(3, 4));
            return std::to_string(num) + " T-3";
        }
    }

    // Extended format with underscore
    if (p.length() == 7 && p[0] == '_') {
        int yearDigit = base62ToNum(p[1]);
        char halfMonth = p[2];
        long baseSequence = base62StringToNum(p.substr(3, 4));

        int cycle = 620 + baseSequence / 25;
        int letterPos = (baseSequence % 25) + 1;
        char secondLetter = positionToLetter(letterPos);

        int year = 2000 + yearDigit;

        char buf[32];
        snprintf(buf, sizeof(buf), "%d %c%c%d", year, halfMonth, secondLetter, cycle);
        return std::string(buf);
    }

    if (p.length() != 7) {
        throw MPCDesignationError("Invalid packed provisional designation length");
    }

    char centuryCode = p[0];
    int century = codeToCentury(centuryCode);
    if (century < 0) {
        throw MPCDesignationError(std::string("Invalid century code: ") + centuryCode);
    }

    int yearShort = std::stoi(p.substr(1, 2));
    char halfMonth = p[3];
    int orderNum = decodeCycleCount(p.substr(4, 2));
    char secondLetter = p[6];

    int fullYear = century * 100 + yearShort;

    char buf[32];
    // For pre-1925 years, output in A-prefix format (A=1xxx, B=2xxx)
    if (fullYear < 1925) {
        char prefix = (fullYear < 2000) ? 'A' : 'B';
        int shortYear = fullYear % 1000;
        if (orderNum == 0) {
            snprintf(buf, sizeof(buf), "%c%03d %c%c", prefix, shortYear, halfMonth, secondLetter);
        } else {
            snprintf(buf, sizeof(buf), "%c%03d %c%c%d", prefix, shortYear, halfMonth, secondLetter, orderNum);
        }
    } else {
        if (orderNum == 0) {
            snprintf(buf, sizeof(buf), "%d %c%c", fullYear, halfMonth, secondLetter);
        } else {
            snprintf(buf, sizeof(buf), "%d %c%c%d", fullYear, halfMonth, secondLetter, orderNum);
        }
    }
    return std::string(buf);
}

// =========================================================================
// Comet provisional designations
// =========================================================================

std::string MPCDesignation::packCometProvisional(const std::string& unpacked) {
    std::string u = trim(unpacked);

    std::regex cometProvRe(R"(^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$)");
    std::smatch match;
    if (!std::regex_match(u, match, cometProvRe)) {
        throw MPCDesignationError("Invalid unpacked comet provisional designation: " + u);
    }

    int year = std::stoi(match[1].str());
    char halfMonth = match[2].str()[0];
    int orderNum = std::stoi(match[3].str());
    std::string fragment = match[4].matched ? match[4].str() : "";

    if (orderNum < 1) {
        throw MPCDesignationError("Comet order number must be positive");
    }

    int century = year / 100;
    int yearShort = year % 100;

    int centuryCodeInt = centuryToCode(century);
    if (centuryCodeInt < 0) {
        throw MPCDesignationError("Invalid century in year: " + std::to_string(year));
    }
    char centuryCode = static_cast<char>(centuryCodeInt);

    std::string orderEncoded = encodeCycleCount(orderNum);
    std::string fragmentCode;
    if (fragment.empty()) {
        fragmentCode = "0";
    } else {
        fragmentCode = "";
        for (char c : fragment) {
            fragmentCode += static_cast<char>(std::tolower(c));
        }
    }

    char buf[16];
    snprintf(buf, sizeof(buf), "%c%02d%c%s%s", centuryCode, yearShort, halfMonth, orderEncoded.c_str(), fragmentCode.c_str());
    return std::string(buf);
}

std::string MPCDesignation::unpackCometProvisional(const std::string& packed) {
    std::string p = trim(packed);
    size_t len = p.length();

    if (len != 7 && len != 8) {
        throw MPCDesignationError("Invalid packed comet provisional designation length");
    }

    char centuryCode = p[0];
    int century = codeToCentury(centuryCode);
    if (century < 0) {
        throw MPCDesignationError(std::string("Invalid century code: ") + centuryCode);
    }

    int yearShort = std::stoi(p.substr(1, 2));
    char halfMonth = p[3];
    int orderNum = decodeCycleCount(p.substr(4, 2));
    std::string fragment = (len == 7) ? std::string(1, p[6]) : p.substr(6, 2);

    int fullYear = century * 100 + yearShort;

    std::stringstream ss;
    ss << fullYear << " " << halfMonth << orderNum;
    if (fragment != "0" && fragment[0] != '0') {
        ss << "-";
        for (char c : fragment) {
            ss << static_cast<char>(std::toupper(c));
        }
    }
    return ss.str();
}

// =========================================================================
// Numbered comet designations
// =========================================================================

std::string MPCDesignation::packCometNumbered(const std::string& unpacked) {
    std::string u = trim(unpacked);

    // Match numbered comet with optional fragment: "73P", "73P-A", "73P-AA"
    std::regex numberedRe(R"(^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$)");
    std::smatch match;
    if (!std::regex_match(u, match, numberedRe)) {
        throw MPCDesignationError("Invalid unpacked numbered comet designation");
    }

    int number = std::stoi(match[1].str());
    char cometType = match[2].str()[0];
    std::string fragment = match[3].matched ? match[3].str() : "";

    if (number < 1 || number > 9999) {
        throw MPCDesignationError("Comet number out of range (1-9999): " + std::to_string(number));
    }

    char buf[16];
    if (fragment.empty()) {
        snprintf(buf, sizeof(buf), "%04d%c", number, cometType);
    } else {
        // Fragment letters are lowercase in packed format
        std::string fragLower;
        for (char c : fragment) {
            fragLower += static_cast<char>(std::tolower(c));
        }
        snprintf(buf, sizeof(buf), "%04d%c%s", number, cometType, fragLower.c_str());
    }
    return std::string(buf);
}

std::string MPCDesignation::unpackCometNumbered(const std::string& packed) {
    std::string p = trim(packed);
    size_t len = p.length();

    // Length 5: no fragment, 6: single-letter fragment, 7: two-letter fragment
    if (len < 5 || len > 7) {
        throw MPCDesignationError("Invalid packed numbered comet designation");
    }

    // Match with optional fragment: "0073P", "0073Pa", "0073Paa"
    std::regex numberedRe(R"(^(\d{4})([PD])([a-z]{0,2})$)");
    std::smatch match;
    if (!std::regex_match(p, match, numberedRe)) {
        throw MPCDesignationError("Invalid packed numbered comet designation");
    }

    int number = std::stoi(match[1].str());
    char cometType = match[2].str()[0];
    std::string fragment = match[3].str();

    std::stringstream ss;
    ss << number << cometType;
    if (!fragment.empty()) {
        ss << "-";
        for (char c : fragment) {
            ss << static_cast<char>(std::toupper(c));
        }
    }
    return ss.str();
}

// =========================================================================
// Satellite designations
// =========================================================================

std::string MPCDesignation::packSatellite(const std::string& unpacked) {
    std::string u = trim(unpacked);

    std::regex satRe(R"(^S/(\d{4}) ([JSUN]) (\d+)$)");
    std::smatch match;
    if (!std::regex_match(u, match, satRe)) {
        throw MPCDesignationError("Invalid unpacked satellite designation");
    }

    int year = std::stoi(match[1].str());
    char planet = match[2].str()[0];
    int number = std::stoi(match[3].str());

    if (number < 1) {
        throw MPCDesignationError("Satellite number must be positive");
    }

    int century = year / 100;
    int yearShort = year % 100;

    int centuryCodeInt = centuryToCode(century);
    if (centuryCodeInt < 0) {
        throw MPCDesignationError("Invalid century in year: " + std::to_string(year));
    }
    char centuryCode = static_cast<char>(centuryCodeInt);

    std::string numberEncoded = encodeCycleCount(number);

    char buf[16];
    snprintf(buf, sizeof(buf), "S%c%02d%c%s0", centuryCode, yearShort, planet, numberEncoded.c_str());
    return std::string(buf);
}

std::string MPCDesignation::unpackSatellite(const std::string& packed) {
    std::string p = trim(packed);

    if (p.length() != 8 || p[0] != 'S') {
        throw MPCDesignationError("Invalid packed satellite designation");
    }

    char centuryCode = p[1];
    int century = codeToCentury(centuryCode);
    if (century < 0) {
        throw MPCDesignationError(std::string("Invalid century code: ") + centuryCode);
    }

    int yearShort = std::stoi(p.substr(2, 2));
    char planet = p[4];

    if (!isSatellitePlanet(planet)) {
        throw MPCDesignationError(std::string("Invalid planet code: ") + planet);
    }

    int number = decodeCycleCount(p.substr(5, 2));
    int fullYear = century * 100 + yearShort;

    char buf[32];
    snprintf(buf, sizeof(buf), "S/%d %c %d", fullYear, planet, number);
    return std::string(buf);
}

// =========================================================================
// BCE year encoding
// =========================================================================

std::pair<char, std::string> MPCDesignation::encodeBCEYear(int year) {
    if (year >= 0) {
        throw MPCDesignationError("Not a BCE year: " + std::to_string(year));
    }

    int absYear = -year;
    int code = 99 - (absYear % 100);

    char prefix;
    if (absYear < 100) {
        prefix = '/';
    } else if (absYear < 200) {
        prefix = '.';
    } else if (absYear < 300) {
        prefix = '-';
    } else {
        throw MPCDesignationError("BCE year out of supported range: " + std::to_string(year));
    }

    char buf[4];
    snprintf(buf, sizeof(buf), "%02d", code);
    return {prefix, std::string(buf)};
}

int MPCDesignation::decodeBCEYear(char prefix, const std::string& code) {
    int codeNum = std::stoi(code);
    int yearPart = 99 - codeNum;

    switch (prefix) {
        case '/': return -yearPart;
        case '.': return -(yearPart + 100);
        case '-': return -(yearPart + 200);
        default: throw MPCDesignationError(std::string("Invalid BCE prefix: ") + prefix);
    }
}

// =========================================================================
// Ancient/BCE comet designations
// =========================================================================

std::string MPCDesignation::packAncientComet(const std::string& unpacked) {
    std::string u = trim(unpacked);

    std::regex ancientRe(R"(^([PCDXAI])/(-?\d+) ([A-Z])(\d+)(?:-([A-Z]))?$)");
    std::smatch match;
    if (!std::regex_match(u, match, ancientRe)) {
        throw MPCDesignationError("Invalid ancient comet designation");
    }

    char cometType = match[1].str()[0];
    int year = std::stoi(match[2].str());
    char halfMonth = match[3].str()[0];
    int orderNum = std::stoi(match[4].str());
    std::string fragment = match[5].matched ? match[5].str() : "";

    if (orderNum < 1) {
        throw MPCDesignationError("Comet order number must be positive");
    }

    std::string orderEncoded = encodeCycleCount(orderNum);
    char fragmentCode = fragment.empty() ? '0' : static_cast<char>(std::tolower(fragment[0]));

    char buf[16];
    if (year < 0) {
        auto [prefix, yearCode] = encodeBCEYear(year);
        snprintf(buf, sizeof(buf), "%c%c%s%c%s%c", cometType, prefix, yearCode.c_str(), halfMonth, orderEncoded.c_str(), fragmentCode);
    } else {
        snprintf(buf, sizeof(buf), "%c%03d%c%s%c", cometType, year, halfMonth, orderEncoded.c_str(), fragmentCode);
    }
    return std::string(buf);
}

std::string MPCDesignation::unpackAncientComet(const std::string& packed) {
    std::string p = trim(packed);

    if (p.length() != 8) {
        throw MPCDesignationError("Invalid ancient comet designation length");
    }

    char cometType = p[0];
    if (!isCometType(cometType)) {
        throw MPCDesignationError(std::string("Invalid comet type: ") + cometType);
    }

    int year;
    char halfMonth;
    int orderNum;
    char fragment;

    if (p[1] == '/' || p[1] == '.' || p[1] == '-') {
        year = decodeBCEYear(p[1], p.substr(2, 2));
        halfMonth = p[4];
        orderNum = decodeCycleCount(p.substr(5, 2));
        fragment = p[7];
    } else {
        year = std::stoi(p.substr(1, 3));
        halfMonth = p[4];
        orderNum = decodeCycleCount(p.substr(5, 2));
        fragment = p[7];
    }

    std::stringstream ss;
    ss << cometType << "/" << year << " " << halfMonth << orderNum;
    if (fragment != '0') {
        ss << "-" << static_cast<char>(std::toupper(fragment));
    }
    return ss.str();
}

// =========================================================================
// Full comet designations
// =========================================================================

bool MPCDesignation::isAsteroidStylePacked(const std::string& provisional) {
    if (provisional.length() != 7) return false;
    return std::isupper(provisional[6]);
}

bool MPCDesignation::isAsteroidStyleUnpacked(const std::string& provisional) {
    size_t spacePos = provisional.find(' ');
    if (spacePos == std::string::npos || spacePos + 2 >= provisional.length()) return false;
    return std::isalpha(provisional[spacePos + 2]);
}

std::string MPCDesignation::packCometFull(const std::string& unpacked) {
    std::string u = trim(unpacked);

    std::regex fullRe(R"(^(\d*)([PCDXAI])/(-?\d+) (.+)$)");
    std::smatch match;
    if (!std::regex_match(u, match, fullRe)) {
        throw MPCDesignationError("Invalid unpacked comet designation: " + u);
    }

    std::string numberStr = match[1].str();
    char cometType = match[2].str()[0];
    int year = std::stoi(match[3].str());
    std::string provPart = match[4].str();

    // Check for ancient or BCE year
    if (year < 1000) {
        return packAncientComet(u);
    }

    // Modern comet
    std::string provisional = std::to_string(year) + " " + provPart;
    std::string provisionalPacked;

    if (isAsteroidStyleUnpacked(provisional)) {
        provisionalPacked = packProvisional(provisional);
    } else {
        provisionalPacked = packCometProvisional(provisional);
    }

    if (numberStr.empty()) {
        return std::string(1, cometType) + provisionalPacked;
    }

    int num = std::stoi(numberStr);
    if (num < 1 || num > 9999) {
        throw MPCDesignationError("Comet number out of range (1-9999): " + std::to_string(num));
    }

    char buf[16];
    snprintf(buf, sizeof(buf), "%04d%c%s", num, cometType, provisionalPacked.c_str());
    return std::string(buf);
}

std::string MPCDesignation::unpackCometFull(const std::string& packed) {
    std::string p = packed;  // Don't trim - need leading spaces for 12-char format
    size_t len = p.length();

    char cometType;
    std::string provisionalPart;
    int number = 0;

    if (len == 8) {
        cometType = p[0];
        provisionalPart = p.substr(1, 7);
    } else if (len == 9) {
        cometType = p[0];
        provisionalPart = p.substr(1, 8);
    } else if (len == 12 || (len < 12 && p[0] == ' ')) {
        // Pad with leading spaces if needed
        while (p.length() < 12) {
            p = " " + p;
        }
        std::string numPart = p.substr(0, 4);
        cometType = p[4];
        provisionalPart = p.substr(5, 7);

        // Parse number, trimming spaces
        size_t start = numPart.find_first_not_of(' ');
        if (start != std::string::npos) {
            number = std::stoi(numPart.substr(start));
        }
    } else {
        throw MPCDesignationError("Invalid packed full comet designation length");
    }

    if (!isCometType(cometType)) {
        throw MPCDesignationError(std::string("Invalid comet type: ") + cometType);
    }

    std::string provisional;
    if (isAsteroidStylePacked(provisionalPart)) {
        provisional = unpackProvisional(provisionalPart);
    } else {
        provisional = unpackCometProvisional(provisionalPart);
    }

    std::stringstream ss;
    if (number == 0) {
        ss << cometType << "/" << provisional;
    } else {
        ss << number << cometType << "/" << provisional;
    }
    return ss.str();
}

// =========================================================================
// Format detection
// =========================================================================

FormatInfo MPCDesignation::detectFormat(const std::string& designation) {
    FormatInfo info;

    // Validate raw input
    validateRawInput(designation);

    size_t origLen = designation.length();

    // Check for packed 12-char comet before trimming
    if (origLen == 12 && isCometType(designation[4])) {
        info.format = Format::Packed;
        info.type = Type::CometFull;
        info.subtype = "comet with provisional designation (12-char)";
        return info;
    }

    // Check for packed 8-char comet
    if (origLen == 8 && isCometType(designation[0]) &&
        designation[1] >= 'A' && designation[1] <= 'L') {
        info.format = Format::Packed;
        info.type = Type::CometFull;
        info.subtype = "comet with provisional designation (8-char)";
        return info;
    }

    // Check for packed 9-char comet with 2-letter fragment
    if (origLen == 9 && isCometType(designation[0]) &&
        designation[1] >= 'A' && designation[1] <= 'L' &&
        std::islower(designation[7]) && std::islower(designation[8])) {
        info.format = Format::Packed;
        info.type = Type::CometFull;
        info.subtype = "comet with provisional designation (9-char, 2-letter fragment)";
        return info;
    }

    // Check for packed ancient comet
    if (origLen == 8 && isCometType(designation[0]) &&
        std::isdigit(designation[1]) && std::isdigit(designation[2]) && std::isdigit(designation[3])) {
        info.format = Format::Packed;
        info.type = Type::CometAncient;
        info.subtype = "comet with ancient provisional (year < 1000)";
        return info;
    }

    // Check for packed BCE comet
    if (origLen == 8 && isCometType(designation[0]) &&
        (designation[1] == '/' || designation[1] == '.' || designation[1] == '-') &&
        designation.find(' ') == std::string::npos) {
        info.format = Format::Packed;
        info.type = Type::CometBCE;
        info.subtype = "comet with BCE provisional";
        return info;
    }

    std::string des = trim(designation);
    size_t len = des.length();

    // Validate whitespace
    validateWhitespace(des);

    // Check for packed satellite
    if (len == 8 && des[0] == 'S' && des[1] >= 'A' && des[1] <= 'L') {
        info.format = Format::Packed;
        info.type = Type::Satellite;
        info.subtype = "natural satellite (" + getPlanetName(des[4]) + ")";
        return info;
    }

    // Check for packed permanent
    if (len == 5) {
        if (des[0] == '~') {
            info.format = Format::Packed;
            info.type = Type::Permanent;
            info.subtype = "permanent numbered (tilde/base-62, >= 620000)";
            return info;
        }

        bool allDigits = true;
        for (char c : des) {
            if (!std::isdigit(c)) { allDigits = false; break; }
        }
        if (allDigits) {
            info.format = Format::Packed;
            info.type = Type::Permanent;
            info.subtype = "permanent numbered (5-digit, < 100000)";
            return info;
        }

        if (std::isalpha(des[0]) && std::isdigit(des[1]) && std::isdigit(des[2]) &&
            std::isdigit(des[3]) && std::isdigit(des[4])) {
            info.format = Format::Packed;
            info.type = Type::Permanent;
            info.subtype = std::isupper(des[0])
                ? "permanent numbered (letter-prefix, 100000-359999)"
                : "permanent numbered (letter-prefix, 360000-619999)";
            return info;
        }

        // Check for packed numbered comet
        if (std::isdigit(des[0]) && std::isdigit(des[1]) && std::isdigit(des[2]) &&
            std::isdigit(des[3]) && (des[4] == 'P' || des[4] == 'D')) {
            info.format = Format::Packed;
            info.type = Type::CometNumbered;
            info.subtype = "comet numbered " + getCometTypeName(des[4]);
            return info;
        }
    }

    // Check for packed numbered comet with single-letter fragment (6 chars)
    if (len == 6 && des.find(' ') == std::string::npos) {
        if (std::isdigit(des[0]) && std::isdigit(des[1]) && std::isdigit(des[2]) &&
            std::isdigit(des[3]) && (des[4] == 'P' || des[4] == 'D') && std::islower(des[5])) {
            info.format = Format::Packed;
            info.type = Type::CometNumbered;
            info.subtype = "comet numbered with fragment " + getCometTypeName(des[4]);
            return info;
        }
    }

    // Check for packed numbered comet with two-letter fragment (7 chars) - before provisional check
    if (len == 7 && des.find(' ') == std::string::npos) {
        if (std::isdigit(des[0]) && std::isdigit(des[1]) && std::isdigit(des[2]) &&
            std::isdigit(des[3]) && (des[4] == 'P' || des[4] == 'D') &&
            std::islower(des[5]) && std::islower(des[6])) {
            info.format = Format::Packed;
            info.type = Type::CometNumbered;
            info.subtype = "comet numbered with 2-letter fragment " + getCometTypeName(des[4]);
            return info;
        }
    }

    // Check for packed provisional (7 chars)
    if (len == 7) {
        if (des[0] == '_') {
            info.format = Format::Packed;
            info.type = Type::ProvisionalExtended;
            info.subtype = "provisional (extended format, cycle >=620)";
            return info;
        }

        if (des.substr(0, 3) == "PLS") {
            info.format = Format::Packed;
            info.type = Type::Survey;
            info.subtype = "survey (Palomar-Leiden)";
            return info;
        }

        if (des[0] == 'T' && des[2] == 'S' && des[1] >= '1' && des[1] <= '3') {
            info.format = Format::Packed;
            info.type = Type::Survey;
            info.subtype = std::string("survey (Trojan T-") + des[1] + ")";
            return info;
        }

        if (des[0] >= 'A' && des[0] <= 'L' && std::isdigit(des[1]) && std::isdigit(des[2]) &&
            std::isupper(des[3]) && std::isupper(des[6])) {
            info.format = Format::Packed;
            info.type = Type::Provisional;
            info.subtype = "provisional";
            return info;
        }

        if (des[0] >= 'I' && des[0] <= 'L' && std::isdigit(des[1]) && std::isdigit(des[2]) &&
            std::isupper(des[3]) && (std::islower(des[6]) || des[6] == '0')) {
            info.format = Format::Packed;
            info.type = Type::CometProvisional;
            info.subtype = "comet provisional";
            return info;
        }
    }

    // --- UNPACKED FORMATS ---

    // Check for unpacked satellite
    std::regex satRe(R"(^S/\d{4} ([JSUN]) \d+$)");
    std::smatch satMatch;
    if (std::regex_match(des, satMatch, satRe)) {
        info.format = Format::Unpacked;
        info.type = Type::Satellite;
        info.subtype = "natural satellite (" + getPlanetName(satMatch[1].str()[0]) + ")";
        return info;
    }

    // Check for unpacked permanent (all digits)
    bool allDigits = !des.empty();
    for (char c : des) {
        if (!std::isdigit(c)) { allDigits = false; break; }
    }
    if (allDigits) {
        info.format = Format::Unpacked;
        info.type = Type::Permanent;
        info.subtype = "permanent numbered";
        return info;
    }

    // Check for unpacked survey
    std::regex surveyRe(R"(^\d+ (P-L|T-[123])$)");
    std::smatch surveyMatch;
    if (std::regex_match(des, surveyMatch, surveyRe)) {
        std::string survey = surveyMatch[1].str();
        info.format = Format::Unpacked;
        info.type = Type::Survey;
        info.subtype = (survey == "P-L") ? "survey (Palomar-Leiden)" : "survey (Trojan " + survey + ")";
        return info;
    }

    // Check for old-style asteroid
    std::regex oldStyleRe(R"(^[AB]\d{3} [A-Z][A-Z]$)");
    if (std::regex_match(des, oldStyleRe)) {
        info.format = Format::Unpacked;
        info.type = Type::Provisional;
        info.subtype = "provisional (old-style pre-1925)";
        return info;
    }

    // Check for unpacked provisional asteroid
    std::regex provRe(R"(^\d{4} [A-Z][A-Z]\d*$)");
    if (std::regex_match(des, provRe)) {
        info.format = Format::Unpacked;
        info.type = Type::Provisional;
        info.subtype = "provisional";
        return info;
    }

    // Check for unpacked comet with type prefix
    std::regex cometFullRe(R"(^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$)");
    std::smatch cometMatch;
    if (std::regex_match(des, cometMatch, cometFullRe)) {
        std::string numStr = cometMatch[1].str();
        char ctype = cometMatch[2].str()[0];
        int year = std::stoi(cometMatch[3].str());

        std::string yearDesc;
        if (year < 0) yearDesc = "BCE";
        else if (year < 1000) yearDesc = "ancient";

        std::string typeDesc = getCometTypeName(ctype);

        info.format = Format::Unpacked;
        info.type = Type::CometFull;
        if (!numStr.empty()) {
            info.subtype = yearDesc.empty()
                ? "comet numbered with provisional (" + typeDesc + ")"
                : "comet numbered with " + yearDesc + " provisional (" + typeDesc + ")";
        } else {
            info.subtype = yearDesc.empty()
                ? "comet provisional (" + typeDesc + ")"
                : "comet " + yearDesc + " provisional (" + typeDesc + ")";
        }
        return info;
    }

    // Check for unpacked numbered comet (with optional fragment)
    std::regex numberedRe(R"(^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$)");
    std::smatch numberedMatch;
    if (std::regex_match(des, numberedMatch, numberedRe)) {
        char cometType = numberedMatch[2].str()[0];
        bool hasFragment = numberedMatch[3].matched;
        info.format = Format::Unpacked;
        info.type = Type::CometNumbered;
        info.subtype = hasFragment
            ? "comet numbered with fragment " + getCometTypeName(cometType)
            : "comet numbered " + getCometTypeName(cometType);
        return info;
    }

    throw MPCDesignationError("Unable to detect designation format: " + designation);
}

// =========================================================================
// Main conversion functions
// =========================================================================

ConversionResult MPCDesignation::convert(const std::string& designation) {
    FormatInfo info = detectFormat(designation);
    std::string output;

    if (info.format == Format::Packed) {
        switch (info.type) {
            case Type::Permanent:
                output = std::to_string(unpackPermanent(designation));
                break;
            case Type::Provisional:
            case Type::Survey:
            case Type::ProvisionalExtended:
                output = unpackProvisional(designation);
                break;
            case Type::CometNumbered:
                output = unpackCometNumbered(designation);
                break;
            case Type::CometProvisional:
                output = unpackCometProvisional(designation);
                break;
            case Type::CometFull:
                output = unpackCometFull(designation);
                break;
            case Type::CometAncient:
            case Type::CometBCE:
                output = unpackAncientComet(designation);
                break;
            case Type::Satellite:
                output = unpackSatellite(designation);
                break;
            default:
                throw MPCDesignationError("Unknown type");
        }
    } else {
        std::string trimmed = trim(designation);
        switch (info.type) {
            case Type::Permanent: {
                long number = std::stol(trimmed);
                if (number < 1 || number > MAX_ASTEROID_NUMBER) {
                    throw MPCDesignationError("Invalid asteroid number: " + trimmed);
                }
                output = packPermanent(number);
                break;
            }
            case Type::Provisional:
            case Type::Survey:
                output = packProvisional(designation);
                break;
            case Type::CometNumbered:
                output = packCometNumbered(designation);
                break;
            case Type::CometFull:
                output = packCometFull(designation);
                break;
            case Type::Satellite:
                output = packSatellite(designation);
                break;
            default:
                throw MPCDesignationError("Unknown type");
        }
    }

    return {designation, output, info};
}

std::string MPCDesignation::convertSimple(const std::string& designation) {
    return convert(designation).output;
}

std::string MPCDesignation::pack(const std::string& designation) {
    FormatInfo info = detectFormat(designation);
    if (info.format == Format::Packed) {
        return trim(designation);
    }
    return convert(designation).output;
}

std::string MPCDesignation::unpack(const std::string& designation) {
    FormatInfo info = detectFormat(designation);
    if (info.format == Format::Unpacked) {
        return trim(designation);
    }
    return convert(designation).output;
}

bool MPCDesignation::isValid(const std::string& designation) noexcept {
    if (designation.empty()) return false;
    try {
        detectFormat(designation);
        return true;
    } catch (...) {
        return false;
    }
}

// =========================================================================
// Helper functions
// =========================================================================

std::string MPCDesignation::toReportFormat(const std::string& minimal) {
    std::string m = trim(minimal);
    size_t len = m.length();

    // Check for numbered comet with fragment: 0073Pa (6 chars) or 0073Paa (7 chars)
    if ((len == 6 || len == 7) && std::isdigit(m[0]) && std::isdigit(m[1]) &&
        std::isdigit(m[2]) && std::isdigit(m[3]) && (m[4] == 'P' || m[4] == 'D') &&
        std::islower(m[5])) {
        // Numbered comet with fragment
        std::string base = m.substr(0, 5);  // "0073P"
        std::string frag = m.substr(5);     // "a" or "aa"
        // Format: "0073P      a" or "0073P     aa"
        int padding = 12 - 5 - static_cast<int>(frag.length());
        return base + std::string(padding, ' ') + frag;
    }

    // Check for numbered comet without fragment: 0073P (5 chars)
    if (len == 5 && std::isdigit(m[0]) && std::isdigit(m[1]) &&
        std::isdigit(m[2]) && std::isdigit(m[3]) && (m[4] == 'P' || m[4] == 'D')) {
        return m + std::string(7, ' ');  // "0073P       "
    }

    // All other designations: right-align in 12 characters
    if (len >= 12) return m;
    return std::string(12 - len, ' ') + m;
}

std::string MPCDesignation::fromReportFormat(const std::string& report) {
    if (report.length() != 12) {
        throw MPCDesignationError("Report format must be exactly 12 characters");
    }

    // Check for numbered comet format: "0073P      a" or "0073P     aa"
    if (std::isdigit(report[0]) && std::isdigit(report[1]) &&
        std::isdigit(report[2]) && std::isdigit(report[3]) &&
        (report[4] == 'P' || report[4] == 'D')) {
        std::string base = report.substr(0, 5);
        std::string rest = report.substr(5);
        // Find fragment letters at the end (skip spaces)
        size_t fragStart = rest.find_last_not_of(' ');
        if (fragStart == std::string::npos) {
            return base;  // No fragment
        }
        // Get fragment (1 or 2 lowercase letters)
        std::string frag;
        if (fragStart > 0 && std::islower(rest[fragStart - 1])) {
            frag = rest.substr(fragStart - 1, 2);
        } else if (std::islower(rest[fragStart])) {
            frag = rest.substr(fragStart, 1);
        }
        if (frag.empty()) {
            return base;
        }
        return base + frag;
    }

    // Other designations: strip leading and trailing spaces
    return trim(report);
}

bool MPCDesignation::hasFragment(const std::string& designation) {
    std::string d = trim(designation);

    // Unpacked format: "73P-A" or "D/1993 F2-B"
    if (d.find('-') != std::string::npos) {
        // Check if it's a comet with fragment (not a survey like "P-L" or "T-1")
        size_t dashPos = d.find('-');
        if (dashPos > 0 && dashPos < d.length() - 1) {
            // Check characters after dash are uppercase letters
            std::string afterDash = d.substr(dashPos + 1);
            if (!afterDash.empty() && afterDash.length() <= 2) {
                bool allUpper = true;
                for (char c : afterDash) {
                    if (!std::isupper(c)) { allUpper = false; break; }
                }
                if (allUpper) {
                    // Make sure it's not a survey designation
                    if (d.find("P-L") == std::string::npos &&
                        d.find("T-1") == std::string::npos &&
                        d.find("T-2") == std::string::npos &&
                        d.find("T-3") == std::string::npos) {
                        return true;
                    }
                }
            }
        }
    }

    // Packed format: "0073Pa" (6 chars) or "0073Paa" (7 chars)
    size_t len = d.length();
    if ((len == 6 || len == 7) && d.find(' ') == std::string::npos) {
        if (std::isdigit(d[0]) && std::isdigit(d[1]) && std::isdigit(d[2]) &&
            std::isdigit(d[3]) && (d[4] == 'P' || d[4] == 'D') && std::islower(d[5])) {
            return true;
        }
    }

    // Packed provisional comet with fragment: "DJ93F02a" (8 chars) or "DJ93F02aa" (9 chars)
    if (len >= 8 && len <= 9 && isCometType(d[0])) {
        if (std::islower(d[len - 1]) && d[len - 1] != '0') {
            return true;
        }
    }

    return false;
}

std::string MPCDesignation::getFragment(const std::string& designation) {
    std::string d = trim(designation);

    // Unpacked format: "73P-A" or "D/1993 F2-B"
    size_t dashPos = d.rfind('-');
    if (dashPos != std::string::npos && dashPos > 0 && dashPos < d.length() - 1) {
        std::string afterDash = d.substr(dashPos + 1);
        if (!afterDash.empty() && afterDash.length() <= 2) {
            bool allUpper = true;
            for (char c : afterDash) {
                if (!std::isupper(c)) { allUpper = false; break; }
            }
            if (allUpper) {
                // Make sure it's not a survey designation
                if (d.find("P-L") == std::string::npos &&
                    d.find("T-1") == std::string::npos &&
                    d.find("T-2") == std::string::npos &&
                    d.find("T-3") == std::string::npos) {
                    return afterDash;
                }
            }
        }
    }

    // Packed numbered comet format: "0073Pa" or "0073Paa"
    size_t len = d.length();
    if ((len == 6 || len == 7) && d.find(' ') == std::string::npos) {
        if (std::isdigit(d[0]) && std::isdigit(d[1]) && std::isdigit(d[2]) &&
            std::isdigit(d[3]) && (d[4] == 'P' || d[4] == 'D') && std::islower(d[5])) {
            std::string frag;
            for (size_t i = 5; i < len; i++) {
                frag += static_cast<char>(std::toupper(d[i]));
            }
            return frag;
        }
    }

    // Packed provisional comet with fragment: "DJ93F02a" or "DJ93F02aa"
    if (len >= 8 && len <= 9 && isCometType(d[0])) {
        if (std::islower(d[len - 1]) && d[len - 1] != '0') {
            std::string frag;
            if (len == 9 && std::islower(d[7])) {
                frag += static_cast<char>(std::toupper(d[7]));
                frag += static_cast<char>(std::toupper(d[8]));
            } else if (len == 8) {
                frag += static_cast<char>(std::toupper(d[7]));
            }
            return frag;
        }
    }

    return "";
}

std::string MPCDesignation::getParent(const std::string& designation) {
    std::string d = trim(designation);

    // Unpacked format: "73P-A" -> "73P"
    size_t dashPos = d.rfind('-');
    if (dashPos != std::string::npos && dashPos > 0 && dashPos < d.length() - 1) {
        std::string afterDash = d.substr(dashPos + 1);
        if (!afterDash.empty() && afterDash.length() <= 2) {
            bool allUpper = true;
            for (char c : afterDash) {
                if (!std::isupper(c)) { allUpper = false; break; }
            }
            if (allUpper) {
                // Make sure it's not a survey designation
                if (d.find("P-L") == std::string::npos &&
                    d.find("T-1") == std::string::npos &&
                    d.find("T-2") == std::string::npos &&
                    d.find("T-3") == std::string::npos) {
                    return d.substr(0, dashPos);
                }
            }
        }
    }

    // Packed numbered comet format: "0073Pa" -> "0073P"
    size_t len = d.length();
    if ((len == 6 || len == 7) && d.find(' ') == std::string::npos) {
        if (std::isdigit(d[0]) && std::isdigit(d[1]) && std::isdigit(d[2]) &&
            std::isdigit(d[3]) && (d[4] == 'P' || d[4] == 'D') && std::islower(d[5])) {
            return d.substr(0, 5);
        }
    }

    // Packed provisional comet with fragment: "DJ93F02a" -> "DJ93F020"
    if (len >= 8 && len <= 9 && isCometType(d[0])) {
        if (std::islower(d[len - 1]) && d[len - 1] != '0') {
            // Return without the fragment, with '0' as the fragment placeholder
            return d.substr(0, 7) + "0";
        }
    }

    // No fragment, return as-is
    return d;
}

bool MPCDesignation::designationsEqual(const std::string& d1, const std::string& d2) {
    try {
        std::string p1 = pack(d1);
        std::string p2 = pack(d2);
        return p1 == p2;
    } catch (...) {
        return false;
    }
}

} // namespace mpc
