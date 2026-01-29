/**
 * MPCDesignation.swift - Convert between packed and unpacked MPC designations
 *
 * Based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 *
 * Supports asteroids, comets, and natural satellites.
 */

import Foundation

// MARK: - Version

let MPC_VERSION = "1.0.0"

// MARK: - Errors

enum MPCDesignationError: Error, CustomStringConvertible {
    case invalidCharacter(Character)
    case consecutiveSpaces
    case emptyDesignation
    case invalidFormat(String)
    case outOfRange(String)
    case invalidBase62Character(Character)

    var description: String {
        switch self {
        case .invalidCharacter(let c):
            return "Invalid character in designation: '\(c)'"
        case .consecutiveSpaces:
            return "Consecutive spaces in designation"
        case .emptyDesignation:
            return "Empty designation"
        case .invalidFormat(let msg):
            return "Invalid format: \(msg)"
        case .outOfRange(let msg):
            return "Out of range: \(msg)"
        case .invalidBase62Character(let c):
            return "Invalid base-62 character: '\(c)'"
        }
    }
}

// MARK: - Constants

/// Base-62 character set: 0-9, A-Z, a-z
let BASE62_CHARS = Array("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

/// Century codes for provisional designations
let CENTURY_CODES: [Character: Int] = [
    "A": 10, "B": 11, "C": 12, "D": 13, "E": 14, "F": 15,
    "G": 16, "H": 17, "I": 18, "J": 19, "K": 20, "L": 21
]

let REVERSE_CENTURY_CODES: [Int: Character] = Dictionary(uniqueKeysWithValues: CENTURY_CODES.map { ($1, $0) })

/// Survey codes mapping
let SURVEY_PACKED_TO_UNPACKED: [String: String] = [
    "PLS": "P-L",
    "T1S": "T-1",
    "T2S": "T-2",
    "T3S": "T-3"
]

let SURVEY_UNPACKED_TO_PACKED: [String: String] = Dictionary(uniqueKeysWithValues: SURVEY_PACKED_TO_UNPACKED.map { ($1, $0) })

/// Valid comet type prefixes
let COMET_TYPES: Set<Character> = ["P", "C", "D", "X", "A", "I"]

/// Human-readable descriptions of comet types
let COMET_TYPE_DESCRIPTIONS: [Character: String] = [
    "P": "periodic",
    "C": "non-periodic",
    "D": "defunct",
    "X": "uncertain orbit",
    "A": "asteroid with comet designation",
    "I": "interstellar"
]

/// Planet codes for natural satellite designations
let SATELLITE_PLANETS: Set<Character> = ["J", "S", "U", "N"]

let SATELLITE_PLANET_NAMES: [Character: String] = [
    "J": "Jupiter",
    "S": "Saturn",
    "U": "Uranus",
    "N": "Neptune"
]

/// Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
let MAX_ASTEROID_NUMBER = 15396335

// MARK: - Pre-compiled Regex Patterns

/// Pre-compiled regex patterns for performance (compiled once at startup)
struct Patterns {
    // Pack patterns
    static let surveyUnpacked = try! NSRegularExpression(pattern: #"^(\d+) (P-L|T-[123])$"#)
    static let oldStyleUnpacked = try! NSRegularExpression(pattern: #"^[AB](\d)(\d{2}) ([A-Z])([A-Z])$"#)
    static let provisionalUnpacked = try! NSRegularExpression(pattern: #"^(\d{4}) ([A-Z])([A-Z])(\d*)$"#)
    static let cometProvUnpacked = try! NSRegularExpression(pattern: #"^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$"#)
    static let cometNumberedPacked = try! NSRegularExpression(pattern: #"^(\d{4})([PD])$"#)
    static let cometNumberedUnpacked = try! NSRegularExpression(pattern: #"^(\d+)([PD])(?:/[A-Za-z].*)?$"#)
    static let satelliteUnpacked = try! NSRegularExpression(pattern: #"^S/(\d{4}) ([JSUN]) (\d+)$"#)
    static let cometFullUnpacked = try! NSRegularExpression(pattern: #"^(\d*)([PCDXAI])/(-?\d+) (.+)$"#)
    static let ancientCometProv = try! NSRegularExpression(pattern: #"^([A-Z])(\d+)(?:-([A-Z]))?$"#)

    // Detect format patterns - packed
    static let packedCometFull12 = try! NSRegularExpression(pattern: #"^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$"#)
    static let packedCometFull8 = try! NSRegularExpression(pattern: #"^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$"#)
    static let packedCometFull9 = try! NSRegularExpression(pattern: #"^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$"#)
    static let packedAncientComet = try! NSRegularExpression(pattern: #"^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$"#)
    static let packedBCEComet = try! NSRegularExpression(pattern: #"^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$"#)
    static let packedSatellite = try! NSRegularExpression(pattern: #"^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$"#)
    static let packedTilde = try! NSRegularExpression(pattern: #"^~[0-9A-Za-z]{4}$"#)
    static let packedLetterPrefix = try! NSRegularExpression(pattern: #"^[A-Za-z][0-9]{4}$"#)
    // Year code: digit (0-9 for 2000-2009) or letter (A=10 for 2010, etc.)
    static let packedExtended = try! NSRegularExpression(pattern: #"^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$"#)
    static let packedProvisional = try! NSRegularExpression(pattern: #"^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$"#)
    static let packedSurveyT = try! NSRegularExpression(pattern: #"^T[123]S\d{4}$"#)
    static let packedCometNumbered = try! NSRegularExpression(pattern: #"^[0-9]{4}[PD]$"#)
    static let packedCometProv = try! NSRegularExpression(pattern: #"^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$"#)

    // Detect format patterns - unpacked
    static let unpackedSatellite = try! NSRegularExpression(pattern: #"^S/\d{4} ([JSUN]) \d+$"#)
    static let unpackedSurvey = try! NSRegularExpression(pattern: #"^\d+ (P-L|T-[123])$"#)
    static let unpackedOldStyle = try! NSRegularExpression(pattern: #"^[AB]\d{3} [A-Z][A-Z]$"#)
    static let unpackedProvisional = try! NSRegularExpression(pattern: #"^\d{4} [A-Z][A-Z]\d*$"#)
    static let unpackedCometFull = try! NSRegularExpression(pattern: #"^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$"#)
    static let unpackedCometNumbered = try! NSRegularExpression(pattern: #"^(\d+)([PD])(?:/[A-Za-z].*)?$"#)

    // Unpack patterns
    static let asteroidStylePacked = try! NSRegularExpression(pattern: #"^\d{4} ([A-Z])(.)"#)
}

// MARK: - Validation Functions

/// Validate raw input string BEFORE trimming
func validateRawInput(_ s: String) throws {
    for c in s {
        guard let code = c.asciiValue else {
            throw MPCDesignationError.invalidCharacter(c)
        }
        if code < 32 || code > 126 {
            throw MPCDesignationError.invalidCharacter(c)
        }
    }
}

/// Validate whitespace in a trimmed designation string
func validateWhitespace(_ s: String) throws {
    var prevSpace = false
    for c in s {
        guard let code = c.asciiValue else {
            throw MPCDesignationError.invalidCharacter(c)
        }
        if code < 32 || code > 126 {
            throw MPCDesignationError.invalidCharacter(c)
        }
        if c == " " {
            if prevSpace {
                throw MPCDesignationError.consecutiveSpaces
            }
            prevSpace = true
        } else {
            prevSpace = false
        }
    }
}

/// Check if a letter is a valid half-month letter (A-Y excluding I)
func isValidHalfMonth(_ letter: Character) -> Bool {
    return letter.isLetter && letter >= "A" && letter <= "Y" && letter != "I"
}

/// Sanitize a designation string for processing
func sanitize(_ designation: String) throws -> String {
    try validateRawInput(designation)
    let result = designation.trimmingCharacters(in: .whitespaces)
    if result.isEmpty {
        throw MPCDesignationError.emptyDesignation
    }
    return result
}

// MARK: - Base-62 Encoding Utilities

/// Convert a single base-62 character to its numeric value
func base62ToNum(_ char: Character) throws -> Int {
    if let idx = BASE62_CHARS.firstIndex(of: char) {
        return BASE62_CHARS.distance(from: BASE62_CHARS.startIndex, to: idx)
    }
    throw MPCDesignationError.invalidBase62Character(char)
}

/// Convert a numeric value (0-61) to its base-62 character
func numToBase62(_ num: Int) throws -> Character {
    guard num >= 0 && num <= 61 else {
        throw MPCDesignationError.outOfRange("Number out of base-62 range: \(num)")
    }
    return BASE62_CHARS[num]
}

/// Convert a base-62 string to a number
func base62StringToNum(_ s: String) throws -> Int {
    var result = 0
    for char in s {
        result = result * 62 + (try base62ToNum(char))
    }
    return result
}

/// Convert a number to a base-62 string of specified width
func numToBase62String(_ num: Int, width: Int = 4) throws -> String {
    var n = num
    var result: [Character] = []
    for _ in 0..<width {
        result.append(try numToBase62(n % 62))
        n /= 62
    }
    return String(result.reversed())
}

// MARK: - Permanent (Numbered) Asteroid Designations

/// Unpack a permanent (numbered) asteroid designation
func unpackPermanent(_ packed: String) throws -> Int {
    let p = packed.trimmingCharacters(in: .whitespaces)
    let chars = Array(p)
    let length = chars.count
    let first = chars[0]

    // Check for tilde format first (>= 620,000)
    if first == "~" && length == 5 {
        let base62Part = String(chars[1...4])
        return 620000 + (try base62StringToNum(base62Part))
    }

    guard length == 5 else {
        throw MPCDesignationError.invalidFormat("Invalid packed permanent designation length: \(packed)")
    }

    if first.isNumber {
        // Simple numeric format (< 100,000)
        guard let num = Int(p) else {
            throw MPCDesignationError.invalidFormat("Invalid packed permanent designation: \(packed)")
        }
        return num
    } else if first >= "A" && first <= "Z" {
        // Extended format with uppercase letter (100,000 - 359,999)
        guard let ascii = first.asciiValue else {
            throw MPCDesignationError.invalidFormat("Invalid packed permanent designation: \(packed)")
        }
        let val = Int(ascii) - 55  // A=10, B=11, etc.
        guard let rest = Int(String(chars[1...4])) else {
            throw MPCDesignationError.invalidFormat("Invalid packed permanent designation: \(packed)")
        }
        return val * 10000 + rest
    } else if first >= "a" && first <= "z" {
        // Extended format with lowercase letter (360,000 - 619,999)
        guard let ascii = first.asciiValue else {
            throw MPCDesignationError.invalidFormat("Invalid packed permanent designation: \(packed)")
        }
        let val = Int(ascii) - 61  // a=36, b=37, etc.
        guard let rest = Int(String(chars[1...4])) else {
            throw MPCDesignationError.invalidFormat("Invalid packed permanent designation: \(packed)")
        }
        return val * 10000 + rest
    } else {
        throw MPCDesignationError.invalidFormat("Invalid packed permanent designation: \(packed)")
    }
}

/// Pack a permanent (numbered) asteroid designation
func packPermanent(_ number: Int) throws -> String {
    guard number >= 1 && number <= MAX_ASTEROID_NUMBER else {
        throw MPCDesignationError.outOfRange("Invalid asteroid number: \(number)")
    }

    if number < 100000 {
        // Simple numeric format
        return String(format: "%05d", number)
    } else if number < 620000 {
        // Extended letter format
        let div = number / 10000
        let mod = number % 10000
        let letter: Character
        if div < 36 {
            // A-Z for 10-35
            letter = Character(UnicodeScalar(div + 55)!)
        } else {
            // a-z for 36-61
            letter = Character(UnicodeScalar(div + 61)!)
        }
        return String(format: "%c%04d", letter.asciiValue!, mod)
    } else {
        // Tilde + base-62 format
        let offset = number - 620000
        return "~" + (try numToBase62String(offset))
    }
}

// MARK: - Cycle Count Encoding

/// Decode the cycle count from packed provisional format
func decodeCycleCount(_ encoded: String) throws -> Int {
    let chars = Array(encoded)
    guard chars.count >= 2 else {
        throw MPCDesignationError.invalidFormat("Invalid cycle count encoding: \(encoded)")
    }

    let first = chars[0]
    let second = chars[1]

    let tens: Int
    if first.isNumber {
        tens = Int(String(first))!
    } else if first >= "A" && first <= "Z" {
        tens = Int(first.asciiValue!) - 55  // A=10, B=11, ..., Z=35
    } else if first >= "a" && first <= "z" {
        tens = Int(first.asciiValue!) - 61  // a=36, b=37, ..., z=61
    } else {
        throw MPCDesignationError.invalidFormat("Invalid cycle count encoding: \(encoded)")
    }

    guard second.isNumber else {
        throw MPCDesignationError.invalidFormat("Invalid cycle count encoding: \(encoded)")
    }

    return tens * 10 + Int(String(second))!
}

/// Encode a cycle count for packed provisional format
func encodeCycleCount(_ count: Int) throws -> String {
    guard count >= 0 && count < 620 else {
        throw MPCDesignationError.outOfRange("Cycle count out of range (0-619): \(count)")
    }

    let tens = count / 10
    let ones = count % 10

    let first: Character
    if tens < 10 {
        first = Character(String(tens))
    } else if tens < 36 {
        first = Character(UnicodeScalar(tens + 55)!)  // A-Z
    } else {
        first = Character(UnicodeScalar(tens + 61)!)  // a-z
    }

    return "\(first)\(ones)"
}

// MARK: - Letter/Position Utilities

/// Convert a half-month letter to its position (A=1, B=2, ..., skipping I)
func letterToPosition(_ letter: Character) throws -> Int {
    guard letter.isLetter else {
        throw MPCDesignationError.invalidFormat("Invalid half-month letter: \(letter)")
    }

    var pos = Int(letter.asciiValue!) - Int(Character("A").asciiValue!) + 1
    if letter > "I" {
        pos -= 1  // Skip I
    }
    return pos
}

/// Convert a position to half-month letter (1=A, 2=B, ..., skipping I)
func positionToLetter(_ pos: Int) throws -> Character {
    // Second letters A-Z excluding I = 25 positions (1-25)
    // A-H = positions 1-8, J-Z = positions 9-25 (I is skipped)
    guard pos >= 1 && pos <= 25 else {
        throw MPCDesignationError.outOfRange("Invalid letter position: \(pos)")
    }

    var p = pos
    if p >= 9 {
        p += 1  // Skip I
    }
    return Character(UnicodeScalar(Int(Character("A").asciiValue!) + p - 1)!)
}

// MARK: - Extended Provisional Format

/// Check if a cycle count requires the extended underscore format
func needsExtendedFormat(_ cycle: Int) -> Bool {
    return cycle >= 620
}

/// Pack a provisional designation using extended format (underscore prefix)
func packExtendedProvisional(year: Int, halfMonth: Character, secondLetter: Character, cycle: Int) throws -> String {
    let yearShort = year % 100

    // Calculate the sequence number within the extended range
    let baseSequence = (cycle - 620) * 25 + (try letterToPosition(secondLetter)) - 1

    // Encode as 4-character base-62
    let seqEncoded = try numToBase62String(baseSequence, width: 4)

    return "_\(try numToBase62(yearShort))\(halfMonth)\(seqEncoded)"
}

/// Unpack an extended provisional designation (underscore format)
func unpackExtendedProvisional(_ packed: String) throws -> String {
    let p = packed.trimmingCharacters(in: .whitespaces)
    let chars = Array(p)

    guard chars.count == 7 && chars[0] == "_" else {
        throw MPCDesignationError.invalidFormat("Invalid extended packed provisional: \(packed)")
    }

    let yearDigit = chars[1]
    let halfMonth = chars[2]
    let seqEncoded = String(chars[3...6])

    // Decode the base-62 sequence
    let baseSequence = try base62StringToNum(seqEncoded)

    // Convert sequence to cycle and letter
    let cycle = 620 + baseSequence / 25
    let letterPos = (baseSequence % 25) + 1
    let secondLetter = try positionToLetter(letterPos)

    // Year code is base-62: digit (0-9 for 2000-2009) or letter (A=10 for 2010, etc.)
    // Extended format is for years 2000-2099, year code is year % 100
    let year = 2000 + (try base62ToNum(yearDigit))

    return "\(year) \(halfMonth)\(secondLetter)\(cycle)"
}

// MARK: - Standard Provisional Asteroid Designations

/// Unpack a provisional asteroid designation
func unpackProvisional(_ packed: String) throws -> String {
    let p = packed.trimmingCharacters(in: .whitespaces)
    let chars = Array(p)

    // Check for survey designations first
    if chars.count == 7 {
        let prefix = String(chars[0...2])
        if let survey = SURVEY_PACKED_TO_UNPACKED[prefix] {
            let number = String(chars[3...6])
            guard let num = Int(number) else {
                throw MPCDesignationError.invalidFormat("Invalid survey number: \(packed)")
            }
            return "\(num) \(survey)"
        }
    }

    guard chars.count == 7 else {
        throw MPCDesignationError.invalidFormat("Invalid packed provisional designation length: \(packed)")
    }

    let century = chars[0]
    let year = String(chars[1...2])
    let halfMonth = chars[3]
    let orderEncoded = String(chars[4...5])
    let secondLetter = chars[6]

    guard let centuryValue = CENTURY_CODES[century] else {
        throw MPCDesignationError.invalidFormat("Invalid century code: \(century)")
    }

    let fullYear = "\(centuryValue)\(year)"
    let orderNum = try decodeCycleCount(orderEncoded)

    if orderNum == 0 {
        return "\(fullYear) \(halfMonth)\(secondLetter)"
    } else {
        return "\(fullYear) \(halfMonth)\(secondLetter)\(orderNum)"
    }
}

/// Pack a provisional asteroid designation
func packProvisional(_ unpacked: String) throws -> String {
    let u = unpacked.trimmingCharacters(in: .whitespaces)

    // Check for survey designations
    if Patterns.surveyUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) != nil {
        let parts = u.split(separator: " ")
        guard let number = Int(parts[0]) else {
            throw MPCDesignationError.invalidFormat("Invalid survey designation: \(unpacked)")
        }
        if number < 1 {
            throw MPCDesignationError.outOfRange("Survey number must be positive: \(number)")
        }
        let survey = String(parts[1])
        guard let packedSurvey = SURVEY_UNPACKED_TO_PACKED[survey] else {
            throw MPCDesignationError.invalidFormat("Invalid survey code: \(survey)")
        }
        return String(format: "%@%04d", packedSurvey, number)
    }

    // Check for old-style designation: "A908 CJ" or "B842 FA"
    if let match = Patterns.oldStyleUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) {
        let centuryDigit = String(u[Range(match.range(at: 1), in: u)!])
        let yearShort = String(u[Range(match.range(at: 2), in: u)!])
        let halfMonth = String(u[Range(match.range(at: 3), in: u)!])
        let secondLetter = String(u[Range(match.range(at: 4), in: u)!])

        let centuryCode: Character
        switch centuryDigit {
        case "8": centuryCode = "I"
        case "9": centuryCode = "J"
        case "0": centuryCode = "K"
        default:
            throw MPCDesignationError.invalidFormat("Invalid century digit in old-style designation: \(centuryDigit)")
        }

        return "\(centuryCode)\(yearShort)\(halfMonth)00\(secondLetter)"
    }

    // Match standard provisional: "1995 XA" or "1995 XA12"
    guard let match = Patterns.provisionalUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) else {
        throw MPCDesignationError.invalidFormat("Invalid unpacked provisional designation: \(unpacked)")
    }

    let year = String(u[Range(match.range(at: 1), in: u)!])
    let halfMonth = Character(String(u[Range(match.range(at: 2), in: u)!]))
    let secondLetter = Character(String(u[Range(match.range(at: 3), in: u)!]))
    let orderStr = match.range(at: 4).length > 0 ? String(u[Range(match.range(at: 4), in: u)!]) : ""

    // Validate half-month letter
    guard isValidHalfMonth(halfMonth) else {
        throw MPCDesignationError.invalidFormat("Invalid half-month letter: \(halfMonth)")
    }

    guard let century = Int(year.prefix(2)) else {
        throw MPCDesignationError.invalidFormat("Invalid year: \(year)")
    }
    let yearShort = String(year.suffix(2))

    guard let centuryCode = REVERSE_CENTURY_CODES[century] else {
        throw MPCDesignationError.invalidFormat("Invalid century in year: \(year)")
    }

    let orderNum: Int
    if orderStr.isEmpty {
        orderNum = 0
    } else {
        guard let parsed = Int(orderStr) else {
            throw MPCDesignationError.outOfRange("Invalid order number: \(orderStr)")
        }
        orderNum = parsed
    }

    // Check if we need extended format
    if needsExtendedFormat(orderNum) {
        guard let yearInt = Int(year) else {
            throw MPCDesignationError.invalidFormat("Invalid year: \(year)")
        }
        return try packExtendedProvisional(year: yearInt, halfMonth: halfMonth, secondLetter: secondLetter, cycle: orderNum)
    }

    let orderEncoded = try encodeCycleCount(orderNum)
    return "\(centuryCode)\(yearShort)\(halfMonth)\(orderEncoded)\(secondLetter)"
}

// MARK: - Comet Provisional Designations

/// Unpack a comet provisional designation
func unpackCometProvisional(_ packed: String) throws -> String {
    let p = packed.trimmingCharacters(in: .whitespaces)
    let chars = Array(p)
    let length = chars.count

    guard length == 7 || length == 8 else {
        throw MPCDesignationError.invalidFormat("Invalid packed comet provisional designation length: \(packed)")
    }

    let century = chars[0]
    let year = String(chars[1...2])
    let halfMonth = chars[3]
    let orderEncoded = String(chars[4...5])

    // Fragment: 1 char for 7-char format, 2 chars for 8-char format
    let fragment: String
    if length == 7 {
        fragment = String(chars[6])
    } else {
        fragment = String(chars[6...7])
    }

    guard let centuryValue = CENTURY_CODES[century] else {
        throw MPCDesignationError.invalidFormat("Invalid century code: \(century)")
    }

    let fullYear = "\(centuryValue)\(year)"
    let orderNum = try decodeCycleCount(orderEncoded)

    var result = "\(fullYear) \(halfMonth)\(orderNum)"

    // Add fragment suffix if present
    if fragment != "0" {
        result += "-\(fragment.uppercased())"
    }

    return result
}

/// Pack a comet provisional designation
func packCometProvisional(_ unpacked: String) throws -> String {
    let u = unpacked.trimmingCharacters(in: .whitespaces)

    // Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
    guard let match = Patterns.cometProvUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) else {
        throw MPCDesignationError.invalidFormat("Invalid unpacked comet provisional designation: \(unpacked)")
    }

    guard let yearRange = Range(match.range(at: 1), in: u),
          let halfMonthRange = Range(match.range(at: 2), in: u),
          let orderRange = Range(match.range(at: 3), in: u) else {
        throw MPCDesignationError.invalidFormat("Invalid unpacked comet provisional designation: \(unpacked)")
    }

    let year = String(u[yearRange])
    let halfMonth = String(u[halfMonthRange])
    guard let orderNum = Int(String(u[orderRange])) else {
        throw MPCDesignationError.invalidFormat("Invalid order number in comet designation: \(unpacked)")
    }
    let fragment: String? = match.range(at: 4).length > 0 ? (Range(match.range(at: 4), in: u).map { String(u[$0]) }) : nil

    // Comet order number must be positive
    guard orderNum >= 1 else {
        throw MPCDesignationError.outOfRange("Comet order number must be positive: \(orderNum)")
    }

    guard let century = Int(year.prefix(2)) else {
        throw MPCDesignationError.invalidFormat("Invalid year in comet designation: \(year)")
    }
    let yearShort = String(year.suffix(2))

    guard let centuryCode = REVERSE_CENTURY_CODES[century] else {
        throw MPCDesignationError.invalidFormat("Invalid century in year: \(year)")
    }

    let orderEncoded = try encodeCycleCount(orderNum)

    // Fragment encoding: 0 for none, lowercase letter(s) for fragment
    let fragmentCode = fragment?.lowercased() ?? "0"

    return "\(centuryCode)\(yearShort)\(halfMonth)\(orderEncoded)\(fragmentCode)"
}

// MARK: - Numbered Comet Designations

/// Unpack a numbered periodic comet designation
func unpackCometNumbered(_ packed: String) throws -> String {
    let p = packed.trimmingCharacters(in: .whitespaces)

    guard let match = Patterns.cometNumberedPacked.firstMatch(in: p, range: NSRange(p.startIndex..., in: p)) else {
        throw MPCDesignationError.invalidFormat("Invalid packed numbered comet designation: \(packed)")
    }

    let number = Int(String(p[Range(match.range(at: 1), in: p)!]))!
    let cometType = String(p[Range(match.range(at: 2), in: p)!])

    return "\(number)\(cometType)"
}

/// Pack a numbered periodic comet designation
func packCometNumbered(_ unpacked: String) throws -> String {
    let u = unpacked.trimmingCharacters(in: .whitespaces)

    // Match "1P" or "354P" or "1P/Halley"
    guard let match = Patterns.cometNumberedUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) else {
        throw MPCDesignationError.invalidFormat("Invalid unpacked numbered comet designation: \(unpacked)")
    }

    let number = Int(String(u[Range(match.range(at: 1), in: u)!]))!
    let cometType = String(u[Range(match.range(at: 2), in: u)!])

    guard number >= 1 && number <= 9999 else {
        throw MPCDesignationError.outOfRange("Comet number out of range (1-9999): \(number)")
    }

    return String(format: "%04d%@", number, cometType)
}

// MARK: - Natural Satellite Designations

/// Unpack a natural satellite provisional designation
func unpackSatellite(_ packed: String) throws -> String {
    let p = packed.trimmingCharacters(in: .whitespaces)
    let chars = Array(p)

    guard chars.count == 8 && chars[0] == "S" else {
        throw MPCDesignationError.invalidFormat("Invalid packed satellite designation: \(packed)")
    }

    let century = chars[1]
    let year = String(chars[2...3])
    let planet = chars[4]
    let numberEncoded = String(chars[5...6])

    guard let centuryValue = CENTURY_CODES[century] else {
        throw MPCDesignationError.invalidFormat("Invalid century code: \(century)")
    }

    guard SATELLITE_PLANETS.contains(planet) else {
        throw MPCDesignationError.invalidFormat("Invalid planet code: \(planet)")
    }

    let fullYear = "\(centuryValue)\(year)"
    let number = try decodeCycleCount(numberEncoded)

    return "S/\(fullYear) \(planet) \(number)"
}

/// Pack a natural satellite provisional designation
func packSatellite(_ unpacked: String) throws -> String {
    let u = unpacked.trimmingCharacters(in: .whitespaces)

    guard let match = Patterns.satelliteUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) else {
        throw MPCDesignationError.invalidFormat("Invalid unpacked satellite designation: \(unpacked)")
    }

    let year = String(u[Range(match.range(at: 1), in: u)!])
    let planet = String(u[Range(match.range(at: 2), in: u)!])
    let number = Int(String(u[Range(match.range(at: 3), in: u)!]))!

    // Satellite number must be positive
    guard number >= 1 else {
        throw MPCDesignationError.outOfRange("Satellite number must be positive: \(number)")
    }

    let century = Int(year.prefix(2))!
    let yearShort = String(year.suffix(2))

    guard let centuryCode = REVERSE_CENTURY_CODES[century] else {
        throw MPCDesignationError.invalidFormat("Invalid century in year: \(year)")
    }

    let numberEncoded = try encodeCycleCount(number)

    return "S\(centuryCode)\(yearShort)\(planet)\(numberEncoded)0"
}

// MARK: - BCE Year Encoding

/// Encode a BCE year for packed format
func encodeBCEYear(_ year: Int) throws -> (String, String) {
    guard year < 0 else {
        throw MPCDesignationError.invalidFormat("Not a BCE year: \(year)")
    }

    let absYear = abs(year)
    let code = 99 - (absYear % 100)

    if absYear < 100 {
        return ("/", String(format: "%02d", code))
    } else if absYear < 200 {
        return (".", String(format: "%02d", code))
    } else if absYear < 300 {
        return ("-", String(format: "%02d", code))
    } else {
        throw MPCDesignationError.outOfRange("BCE year out of supported range: \(year)")
    }
}

/// Decode a BCE year from packed format
func decodeBCEYear(prefix: String, code: String) throws -> Int {
    guard let codeNum = Int(code) else {
        throw MPCDesignationError.invalidFormat("Invalid BCE year code: \(code)")
    }
    let yearPart = 99 - codeNum

    switch prefix {
    case "/":
        return -yearPart
    case ".":
        return -(yearPart + 100)
    case "-":
        return -(yearPart + 200)
    default:
        throw MPCDesignationError.invalidFormat("Invalid BCE prefix: \(prefix)")
    }
}

/// Check if a year is ancient (< 1000) or BCE
func isAncientYear(_ year: Int) -> Bool {
    return year < 1000
}

// MARK: - Ancient/BCE Comet Designations

/// Pack an ancient or BCE comet provisional designation
func packAncientCometProvisional(cometType: Character, year: Int, halfMonth: Character, orderNum: Int, fragment: String = "") throws -> String {
    let orderEncoded = try encodeCycleCount(orderNum)
    let fragmentCode = fragment.isEmpty ? "0" : fragment.lowercased()

    if year < 0 {
        // BCE year
        let (prefix, code) = try encodeBCEYear(year)
        return "\(cometType)\(prefix)\(code)\(halfMonth)\(orderEncoded)\(fragmentCode)"
    } else {
        // Ancient (1-999)
        return String(format: "%c%03d%c%@%@", cometType.asciiValue!, year, halfMonth.asciiValue!, orderEncoded, fragmentCode)
    }
}

/// Unpack an ancient or BCE comet provisional designation
func unpackAncientCometProvisional(_ packed: String) throws -> String {
    let p = packed.trimmingCharacters(in: .whitespaces)
    let chars = Array(p)

    guard chars.count == 8 else {
        throw MPCDesignationError.invalidFormat("Invalid ancient comet designation length: \(packed)")
    }

    let cometType = chars[0]

    guard COMET_TYPES.contains(cometType) else {
        throw MPCDesignationError.invalidFormat("Invalid comet type: \(cometType)")
    }

    let year: Int
    let halfMonth: Character
    let orderEncoded: String
    let fragment: Character

    // Check for BCE prefix
    if "/.-".contains(chars[1]) {
        let prefix = String(chars[1])
        let yearCode = String(chars[2...3])
        year = try decodeBCEYear(prefix: prefix, code: yearCode)
        halfMonth = chars[4]
        orderEncoded = String(chars[5...6])
        fragment = chars[7]
    } else {
        // Ancient year (3 digits)
        year = Int(String(chars[1...3]))!
        halfMonth = chars[4]
        orderEncoded = String(chars[5...6])
        fragment = chars[7]
    }

    let orderNum = try decodeCycleCount(orderEncoded)
    var result = "\(cometType)/\(year) \(halfMonth)\(orderNum)"

    if fragment != "0" {
        result += "-\(String(fragment).uppercased())"
    }

    return result
}

// MARK: - Comet Format Detection Helpers

/// Check if a 7-char provisional uses asteroid-style (ends with uppercase letter)
func isAsteroidStylePacked(_ provisionalPart: String) -> Bool {
    guard provisionalPart.count == 7 else { return false }
    let lastChar = provisionalPart.last!
    return lastChar.isUppercase
}

/// Check if an unpacked provisional uses asteroid-style (letter after half-month)
func isAsteroidStyleUnpacked(_ provisional: String) -> Bool {
    guard let match = Patterns.asteroidStylePacked.firstMatch(in: provisional, range: NSRange(provisional.startIndex..., in: provisional)),
          match.range(at: 2).length > 0 else {
        return false
    }
    let secondChar = String(provisional[Range(match.range(at: 2), in: provisional)!])
    return secondChar.first?.isLetter ?? false
}

// MARK: - Full Comet Designations

/// Unpack a full comet designation (type + provisional or numbered)
func unpackCometFull(_ packed: String) throws -> String {
    var p = packed
    let length = p.count

    if length == 8 {
        // Compact 8-char format: type + 7-char provisional
        let cometType = p.first!
        let provisionalPart = String(p.dropFirst())

        guard COMET_TYPES.contains(cometType) else {
            throw MPCDesignationError.invalidFormat("Invalid comet type: \(cometType)")
        }

        let provisional: String
        if isAsteroidStylePacked(provisionalPart) {
            provisional = try unpackProvisional(provisionalPart)
        } else {
            provisional = try unpackCometProvisional(provisionalPart)
        }

        return "\(cometType)/\(provisional)"
    } else if length == 9 {
        // Compact 9-char format with 2-letter fragment
        let cometType = p.first!
        let provisionalPart = String(p.dropFirst())

        guard COMET_TYPES.contains(cometType) else {
            throw MPCDesignationError.invalidFormat("Invalid comet type: \(cometType)")
        }

        let provisional = try unpackCometProvisional(provisionalPart)
        return "\(cometType)/\(provisional)"
    } else if length == 12 || (length < 12 && p.first == " ") {
        // Full 12-char format or trimmed version
        while p.count < 12 {
            p = " " + p
        }
        let numPart = String(p.prefix(4))
        let cometType = p[p.index(p.startIndex, offsetBy: 4)]
        let provisionalPart = String(p.suffix(7))
        let numStr = numPart.trimmingCharacters(in: .whitespaces)

        guard COMET_TYPES.contains(cometType) else {
            throw MPCDesignationError.invalidFormat("Invalid comet type: \(cometType)")
        }

        let provisional: String
        if isAsteroidStylePacked(provisionalPart) {
            provisional = try unpackProvisional(provisionalPart)
        } else {
            provisional = try unpackCometProvisional(provisionalPart)
        }

        if numStr.isEmpty {
            return "\(cometType)/\(provisional)"
        } else {
            let number = Int(numStr)!
            return "\(number)\(cometType)/\(provisional)"
        }
    } else {
        throw MPCDesignationError.invalidFormat("Invalid packed full comet designation length: \(packed)")
    }
}

/// Pack a full comet designation
func packCometFull(_ unpacked: String) throws -> String {
    let u = unpacked.trimmingCharacters(in: .whitespaces)

    // Match: optional number, type, slash, year, provisional
    guard let match = Patterns.cometFullUnpacked.firstMatch(in: u, range: NSRange(u.startIndex..., in: u)) else {
        throw MPCDesignationError.invalidFormat("Invalid unpacked comet designation: \(unpacked)")
    }

    let number = String(u[Range(match.range(at: 1), in: u)!])
    let cometType = Character(String(u[Range(match.range(at: 2), in: u)!]))
    let year = Int(String(u[Range(match.range(at: 3), in: u)!]))!
    let provPart = String(u[Range(match.range(at: 4), in: u)!])

    guard COMET_TYPES.contains(cometType) else {
        throw MPCDesignationError.invalidFormat("Invalid comet type: \(cometType)")
    }

    // Check for ancient or BCE year
    if isAncientYear(year) {
        guard let ancientMatch = Patterns.ancientCometProv.firstMatch(in: provPart, range: NSRange(provPart.startIndex..., in: provPart)) else {
            throw MPCDesignationError.invalidFormat("Invalid ancient comet provisional: \(provPart)")
        }

        let halfMonth = Character(String(provPart[Range(ancientMatch.range(at: 1), in: provPart)!]))
        let orderNum = Int(String(provPart[Range(ancientMatch.range(at: 2), in: provPart)!]))!
        let fragment = ancientMatch.range(at: 3).length > 0 ? String(provPart[Range(ancientMatch.range(at: 3), in: provPart)!]) : ""

        return try packAncientCometProvisional(cometType: cometType, year: year, halfMonth: halfMonth, orderNum: orderNum, fragment: fragment)
    }

    // Modern comet - reconstruct provisional with year
    let provisional = "\(year) \(provPart)"

    let provisionalPacked: String
    if isAsteroidStyleUnpacked(provisional) {
        provisionalPacked = try packProvisional(provisional)
    } else {
        provisionalPacked = try packCometProvisional(provisional)
    }

    if number.isEmpty {
        // No periodic number - use compact format
        return "\(cometType)\(provisionalPacked)"
    } else {
        // Has periodic number - use full 12-char format
        guard let num = Int(number), num >= 1 && num <= 9999 else {
            throw MPCDesignationError.outOfRange("Comet number out of range (1-9999): \(number)")
        }
        return String(format: "%04d%c%@", num, cometType.asciiValue!, provisionalPacked)
    }
}

// MARK: - Format Detection

/// Result of format detection
struct FormatInfo {
    var format: String = ""
    var type: String = ""
    var subtype: String = ""
}

/// Detect if a designation is packed or unpacked and what type it is
func detectFormat(_ designation: String) throws -> FormatInfo {
    var result = FormatInfo()

    // Validate raw input BEFORE trimming
    try validateRawInput(designation)

    // Check for packed full comet designation BEFORE trimming (12 chars with spaces)
    if designation.count == 12 {
        if Patterns.packedCometFull12.firstMatch(in: designation, range: NSRange(designation.startIndex..., in: designation)) != nil {
            result.format = "packed"
            result.type = "comet_full"
            result.subtype = "comet with provisional designation (12-char)"
            return result
        }
    }

    // Check for packed comet designation (8 chars)
    if designation.count == 8 {
        if Patterns.packedCometFull8.firstMatch(in: designation, range: NSRange(designation.startIndex..., in: designation)) != nil {
            result.format = "packed"
            result.type = "comet_full"
            result.subtype = "comet with provisional designation (8-char)"
            return result
        }
    }

    // Check for packed comet with 2-letter fragment (9 chars)
    if designation.count == 9 {
        if Patterns.packedCometFull9.firstMatch(in: designation, range: NSRange(designation.startIndex..., in: designation)) != nil {
            result.format = "packed"
            result.type = "comet_full"
            result.subtype = "comet with provisional designation (9-char, 2-letter fragment)"
            return result
        }
    }

    // Check for packed ancient comet (8 chars)
    if designation.count == 8 {
        if Patterns.packedAncientComet.firstMatch(in: designation, range: NSRange(designation.startIndex..., in: designation)) != nil {
            result.format = "packed"
            result.type = "comet_ancient"
            result.subtype = "comet with ancient provisional (year < 1000)"
            return result
        }
    }

    // Check for packed BCE comet (8 chars)
    if designation.count == 8 {
        if Patterns.packedBCEComet.firstMatch(in: designation, range: NSRange(designation.startIndex..., in: designation)) != nil {
            result.format = "packed"
            result.type = "comet_bce"
            result.subtype = "comet with BCE provisional"
            return result
        }
    }

    let des = designation.trimmingCharacters(in: .whitespaces)

    // Validate whitespace
    try validateWhitespace(des)

    // Check for packed satellite designation (8 chars starting with S)
    if des.count == 8 && des.first == "S" {
        if Patterns.packedSatellite.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
            result.format = "packed"
            result.type = "satellite"
            let planet = des[des.index(des.startIndex, offsetBy: 4)]
            let planetName = SATELLITE_PLANET_NAMES[planet] ?? String(planet)
            result.subtype = "natural satellite (\(planetName))"
            return result
        }
    }

    // Check for packed permanent (numbered) asteroid
    if des.count == 5 {
        if des.first == "~" {
            if Patterns.packedTilde.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
                result.format = "packed"
                result.type = "permanent"
                result.subtype = "permanent numbered (tilde/base-62, >= 620000)"
                return result
            }
        } else if des.allSatisfy({ $0.isNumber }) {
            result.format = "packed"
            result.type = "permanent"
            result.subtype = "permanent numbered (5-digit, < 100000)"
            return result
        } else {
            if Patterns.packedLetterPrefix.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
                result.format = "packed"
                result.type = "permanent"
                if des.first!.isUppercase {
                    result.subtype = "permanent numbered (letter-prefix, 100000-359999)"
                } else {
                    result.subtype = "permanent numbered (letter-prefix, 360000-619999)"
                }
                return result
            }
        }
    }

    // Check for packed provisional asteroid (7 chars)
    if des.count == 7 {
        // Extended format with underscore
        if des.first == "_" {
            if Patterns.packedExtended.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
                result.format = "packed"
                result.type = "provisional_extended"
                result.subtype = "provisional (extended format, cycle >=620)"
                return result
            }
        }
        // Standard provisional
        if Patterns.packedProvisional.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
            result.format = "packed"
            result.type = "provisional"
            result.subtype = "provisional"
            return result
        }
        // Survey designations
        if des.hasPrefix("PLS") && des.dropFirst(3).allSatisfy({ $0.isNumber }) {
            result.format = "packed"
            result.type = "survey"
            result.subtype = "survey (Palomar-Leiden)"
            return result
        }
        if Patterns.packedSurveyT.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
            result.format = "packed"
            result.type = "survey"
            result.subtype = "survey (Trojan T-\(des[des.index(des.startIndex, offsetBy: 1)]))"
            return result
        }
    }

    // Check for packed numbered comet (5 chars ending in P or D)
    if des.count == 5 {
        if Patterns.packedCometNumbered.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
            result.format = "packed"
            result.type = "comet_numbered"
            let cometType = des.last!
            let typeDesc = COMET_TYPE_DESCRIPTIONS[cometType] ?? String(cometType)
            result.subtype = "comet numbered \(typeDesc)"
            return result
        }
    }

    // Check for packed comet provisional (7 chars)
    if des.count == 7 {
        if Patterns.packedCometProv.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
            result.format = "packed"
            result.type = "comet_provisional"
            result.subtype = "comet provisional"
            return result
        }
    }

    // --- UNPACKED FORMATS ---

    // Check for unpacked satellite
    if let match = Patterns.unpackedSatellite.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) {
        result.format = "unpacked"
        result.type = "satellite"
        let planet = Character(String(des[Range(match.range(at: 1), in: des)!]))
        let planetName = SATELLITE_PLANET_NAMES[planet] ?? String(planet)
        result.subtype = "natural satellite (\(planetName))"
        return result
    }

    // Check for unpacked permanent (numbered) asteroid
    if des.allSatisfy({ $0.isNumber }) && !des.isEmpty {
        result.format = "unpacked"
        result.type = "permanent"
        result.subtype = "permanent numbered"
        return result
    }

    // Check for unpacked survey designation
    if let match = Patterns.unpackedSurvey.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) {
        result.format = "unpacked"
        result.type = "survey"
        let survey = String(des[Range(match.range(at: 1), in: des)!])
        if survey == "P-L" {
            result.subtype = "survey (Palomar-Leiden)"
        } else {
            result.subtype = "survey (Trojan \(survey))"
        }
        return result
    }

    // Check for old-style asteroid designation
    if Patterns.unpackedOldStyle.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
        result.format = "unpacked"
        result.type = "provisional"
        result.subtype = "provisional (old-style pre-1925)"
        return result
    }

    // Check for unpacked provisional asteroid
    if Patterns.unpackedProvisional.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) != nil {
        result.format = "unpacked"
        result.type = "provisional"
        result.subtype = "provisional"
        return result
    }

    // Check for unpacked comet with type prefix
    if let match = Patterns.unpackedCometFull.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) {
        let num = String(des[Range(match.range(at: 1), in: des)!])
        let ctype = Character(String(des[Range(match.range(at: 2), in: des)!]))
        let year = Int(String(des[Range(match.range(at: 3), in: des)!]))!

        let yearDesc: String
        if year < 0 {
            yearDesc = "BCE"
        } else if year < 1000 {
            yearDesc = "ancient"
        } else {
            yearDesc = ""
        }

        result.format = "unpacked"
        result.type = "comet_full"

        let typeDesc = COMET_TYPE_DESCRIPTIONS[ctype] ?? String(ctype)
        if !num.isEmpty {
            if !yearDesc.isEmpty {
                result.subtype = "comet numbered with \(yearDesc) provisional (\(typeDesc))"
            } else {
                result.subtype = "comet numbered with provisional (\(typeDesc))"
            }
        } else {
            if !yearDesc.isEmpty {
                result.subtype = "comet \(yearDesc) provisional (\(typeDesc))"
            } else {
                result.subtype = "comet provisional (\(typeDesc))"
            }
        }
        return result
    }

    // Check for unpacked numbered periodic comet
    if let match = Patterns.unpackedCometNumbered.firstMatch(in: des, range: NSRange(des.startIndex..., in: des)) {
        result.format = "unpacked"
        result.type = "comet_numbered"
        let cometType = Character(String(des[Range(match.range(at: 2), in: des)!]))
        let typeDesc = COMET_TYPE_DESCRIPTIONS[cometType] ?? String(cometType)
        result.subtype = "comet numbered \(typeDesc)"
        return result
    }

    throw MPCDesignationError.invalidFormat("Unable to detect designation format: \(designation)")
}

// MARK: - Main Conversion Functions

/// Result of a conversion
struct ConversionResult {
    let input: String
    let output: String
    let info: FormatInfo
}

/// Convert a designation between packed and unpacked formats
func convert(_ designation: String) throws -> ConversionResult {
    let info = try detectFormat(designation)
    var result = ""

    if info.format == "packed" {
        switch info.type {
        case "permanent":
            result = String(try unpackPermanent(designation))
        case "provisional", "survey":
            result = try unpackProvisional(designation)
        case "provisional_extended":
            result = try unpackExtendedProvisional(designation)
        case "comet_numbered":
            result = try unpackCometNumbered(designation)
        case "comet_provisional":
            result = try unpackCometProvisional(designation)
        case "comet_full":
            result = try unpackCometFull(designation)
        case "comet_ancient", "comet_bce":
            result = try unpackAncientCometProvisional(designation)
        case "satellite":
            result = try unpackSatellite(designation)
        default:
            throw MPCDesignationError.invalidFormat("Unknown type: \(info.type)")
        }
    } else {
        switch info.type {
        case "permanent":
            guard let num = Int(designation.trimmingCharacters(in: .whitespaces)) else {
                throw MPCDesignationError.invalidFormat("Invalid number: \(designation)")
            }
            result = try packPermanent(num)
        case "provisional", "survey":
            result = try packProvisional(designation)
        case "comet_numbered":
            result = try packCometNumbered(designation)
        case "comet_full":
            result = try packCometFull(designation)
        case "satellite":
            result = try packSatellite(designation)
        default:
            throw MPCDesignationError.invalidFormat("Unknown type: \(info.type)")
        }
    }

    return ConversionResult(input: designation, output: result, info: info)
}

/// Convert a designation between packed and unpacked formats (simple version)
func convertSimple(_ designation: String) throws -> String {
    return try convert(designation).output
}

// MARK: - High-Level Pack/Unpack Functions

/// Ensure a designation is in packed format
func pack(_ designation: String) throws -> String {
    let info = try detectFormat(designation)

    if info.format == "packed" {
        return designation.trimmingCharacters(in: .whitespaces)
    }

    return try convert(designation).output
}

/// Ensure a designation is in unpacked (human-readable) format
func unpack(_ designation: String) throws -> String {
    let info = try detectFormat(designation)

    if info.format == "unpacked" {
        return designation.trimmingCharacters(in: .whitespaces)
    }

    return try convert(designation).output
}

// MARK: - Command-Line Interface

func printUsage() {
    let usage = """
    Usage: MPCDesignation [-v|--verbose] <designation> [designation ...]

    Convert between packed and unpacked MPC designations.
    Auto-detects the input format and converts to the other.

    Options:
      -v, --verbose   Show detailed information about the conversion
      --version       Show version information

    Examples:
      MPCDesignation 00001             -> 1
      MPCDesignation 1                 -> 00001
      MPCDesignation J95X00A           -> 1995 XA
      MPCDesignation '1995 XA'         -> J95X00A
      MPCDesignation 'C/1995 O1'       -> CJ95O010
      MPCDesignation 1P                -> 0001P
    """
    fputs(usage + "\n", stderr)
}

func main() {
    let args = Array(CommandLine.arguments.dropFirst())

    if args.isEmpty {
        printUsage()
        exit(1)
    }

    var verbose = false
    var designations: [String] = []

    for arg in args {
        switch arg {
        case "-v", "--verbose":
            verbose = true
        case "-h", "--help":
            printUsage()
            exit(0)
        case "--version":
            print("mpc_designation \(MPC_VERSION)")
            exit(0)
        default:
            designations.append(arg)
        }
    }

    if designations.isEmpty {
        printUsage()
        exit(1)
    }

    let multiple = designations.count > 1

    for des in designations {
        do {
            let result = try convert(des)
            let info = result.info
            let output = result.output

            if verbose {
                print("  Input:    \(des)")
                print("  Detected: \(info.format) format, \(info.subtype)")
                let action = info.format == "packed" ? "unpacking to human-readable form" : "packing to MPC compact form"
                print("  Action:   \(action)")
                print("  Output:   \(output)")
                if multiple {
                    print()
                }
            } else if multiple {
                print("\(des) -> \(output)")
            } else {
                print(output)
            }
        } catch let error as MPCDesignationError {
            fputs("Error: \(error.description)\n", stderr)
            exit(1)
        } catch {
            fputs("Error: \(error.localizedDescription)\n", stderr)
            exit(1)
        }
    }
}

// Note: For standalone execution, compile with main.swift or use as library
