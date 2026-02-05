/**
 * mpc_designation.js - Convert between packed and unpacked MPC designations
 *
 * Based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 *
 * Supports asteroids, comets, and natural satellites.
 */

'use strict';

// Base-62 character set: 0-9, A-Z, a-z
const BASE62_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

// Century codes for provisional designations
const CENTURY_CODES = {
    'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15,
    'G': 16, 'H': 17, 'I': 18, 'J': 19, 'K': 20, 'L': 21
};
const REVERSE_CENTURY_CODES = {};
for (const [k, v] of Object.entries(CENTURY_CODES)) {
    REVERSE_CENTURY_CODES[v] = k;
}

// Survey codes mapping
const SURVEY_PACKED_TO_UNPACKED = {
    'PLS': 'P-L',
    'T1S': 'T-1',
    'T2S': 'T-2',
    'T3S': 'T-3'
};
const SURVEY_UNPACKED_TO_PACKED = {};
for (const [k, v] of Object.entries(SURVEY_PACKED_TO_UNPACKED)) {
    SURVEY_UNPACKED_TO_PACKED[v] = k;
}

// Valid comet type prefixes
const COMET_TYPES = new Set(['P', 'C', 'D', 'X', 'A', 'I']);

// Planet codes for natural satellite designations
const SATELLITE_PLANETS = new Set(['J', 'S', 'U', 'N']);
const SATELLITE_PLANET_NAMES = {
    'J': 'Jupiter',
    'S': 'Saturn',
    'U': 'Uranus',
    'N': 'Neptune'
};

// Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
const MAX_ASTEROID_NUMBER = 15396335;

/**
 * Error class for MPC designation errors.
 */
class MPCDesignationError extends Error {
    constructor(message) {
        super(message);
        this.name = 'MPCDesignationError';
    }
}

// =============================================================================
// Input validation
// =============================================================================

/**
 * Validate raw input string BEFORE trimming.
 */
function validateRawInput(s) {
    for (let i = 0; i < s.length; i++) {
        const code = s.charCodeAt(i);
        if (code < 32 || code > 126) {
            throw new MPCDesignationError(`Invalid character in designation: ${JSON.stringify(s[i])}`);
        }
    }
}

/**
 * Validate whitespace in a trimmed designation string.
 */
function validateWhitespace(s) {
    let prevSpace = false;
    for (let i = 0; i < s.length; i++) {
        const code = s.charCodeAt(i);
        if (code < 32 || code > 126) {
            throw new MPCDesignationError(`Invalid character in designation: ${JSON.stringify(s[i])}`);
        }
        if (s[i] === ' ') {
            if (prevSpace) {
                throw new MPCDesignationError('Consecutive spaces in designation');
            }
            prevSpace = true;
        } else {
            prevSpace = false;
        }
    }
}

/**
 * Check if a letter is a valid half-month letter (A-Y excluding I).
 */
function isValidHalfMonth(letter) {
    return /^[A-HJ-Y]$/.test(letter);
}

/**
 * Sanitize a designation string for processing.
 */
function sanitize(designation) {
    if (typeof designation !== 'string') {
        throw new MPCDesignationError(`Designation must be a string, got ${typeof designation}`);
    }
    validateRawInput(designation);
    const result = designation.trim();
    if (!result) {
        throw new MPCDesignationError('Empty designation');
    }
    return result;
}

// =============================================================================
// Base-62 encoding utilities
// =============================================================================

/**
 * Convert a single base-62 character to its numeric value.
 */
function base62ToNum(char) {
    const idx = BASE62_CHARS.indexOf(char);
    if (idx < 0) {
        throw new MPCDesignationError(`Invalid base-62 character: ${char}`);
    }
    return idx;
}

/**
 * Convert a numeric value (0-61) to its base-62 character.
 */
function numToBase62(num) {
    if (num < 0 || num > 61) {
        throw new MPCDesignationError(`Number out of base-62 range: ${num}`);
    }
    return BASE62_CHARS[num];
}

/**
 * Convert a base-62 string to a number.
 */
function base62StringToNum(s) {
    let result = 0;
    for (let i = 0; i < s.length; i++) {
        result = result * 62 + base62ToNum(s[i]);
    }
    return result;
}

/**
 * Convert a number to a base-62 string of specified width.
 */
function numToBase62String(num, width = 4) {
    const result = [];
    for (let i = 0; i < width; i++) {
        result.push(numToBase62(num % 62));
        num = Math.floor(num / 62);
    }
    return result.reverse().join('');
}

// =============================================================================
// Permanent (numbered) asteroid designations
// =============================================================================

/**
 * Unpack a permanent (numbered) asteroid designation.
 */
function unpackPermanent(packed) {
    packed = packed.trim();
    const length = packed.length;
    const first = packed[0];

    // Check for tilde format first (>= 620,000)
    if (first === '~' && length === 5) {
        const base62Part = packed.substring(1, 5);
        return 620000 + base62StringToNum(base62Part);
    }

    if (length !== 5) {
        throw new MPCDesignationError(`Invalid packed permanent designation length: ${packed}`);
    }

    const rest = packed.substring(1, 5);

    if (/^\d$/.test(first)) {
        return parseInt(packed, 10);
    } else if (first >= 'A' && first <= 'Z') {
        const val = first.charCodeAt(0) - 55; // A=10, B=11, etc.
        return val * 10000 + parseInt(rest, 10);
    } else if (first >= 'a' && first <= 'z') {
        const val = first.charCodeAt(0) - 61; // a=36, b=37, etc.
        return val * 10000 + parseInt(rest, 10);
    } else {
        throw new MPCDesignationError(`Invalid packed permanent designation: ${packed}`);
    }
}

/**
 * Pack a permanent (numbered) asteroid designation.
 */
function packPermanent(number) {
    if (!Number.isInteger(number) || number < 1 || number > MAX_ASTEROID_NUMBER) {
        throw new MPCDesignationError(`Invalid asteroid number: ${number}`);
    }

    if (number < 100000) {
        return number.toString().padStart(5, '0');
    } else if (number < 620000) {
        const div = Math.floor(number / 10000);
        const mod = number % 10000;
        let letter;
        if (div < 36) {
            letter = String.fromCharCode(div + 55);
        } else {
            letter = String.fromCharCode(div + 61);
        }
        return letter + mod.toString().padStart(4, '0');
    } else {
        const offset = number - 620000;
        return '~' + numToBase62String(offset);
    }
}

// =============================================================================
// Cycle count encoding for provisional designations
// =============================================================================

/**
 * Decode the cycle count from packed provisional format.
 */
function decodeCycleCount(encoded) {
    const first = encoded[0];
    const second = encoded[1];

    let tens;
    if (/^\d$/.test(first)) {
        tens = parseInt(first, 10);
    } else if (first >= 'A' && first <= 'Z') {
        tens = first.charCodeAt(0) - 55;
    } else if (first >= 'a' && first <= 'z') {
        tens = first.charCodeAt(0) - 61;
    } else {
        throw new MPCDesignationError(`Invalid cycle count encoding: ${encoded}`);
    }

    if (!/^\d$/.test(second)) {
        throw new MPCDesignationError(`Invalid cycle count encoding: ${encoded}`);
    }

    return tens * 10 + parseInt(second, 10);
}

/**
 * Encode a cycle count for packed provisional format.
 */
function encodeCycleCount(count) {
    if (count < 0 || count >= 620) {
        throw new MPCDesignationError(`Cycle count out of range (0-619): ${count}`);
    }

    const tens = Math.floor(count / 10);
    const ones = count % 10;

    let first;
    if (tens < 10) {
        first = tens.toString();
    } else if (tens < 36) {
        first = String.fromCharCode(tens + 55);
    } else {
        first = String.fromCharCode(tens + 61);
    }

    return first + ones.toString();
}

// =============================================================================
// Letter/position utilities for provisional designations
// =============================================================================

/**
 * Convert a half-month letter to its position (A=1, B=2, ..., skipping I).
 */
function letterToPosition(letter) {
    if (!letter || letter.length !== 1) {
        throw new MPCDesignationError(`Invalid half-month letter: ${letter}`);
    }

    let pos = letter.toUpperCase().charCodeAt(0) - 'A'.charCodeAt(0) + 1;
    if (letter.toUpperCase() > 'I') {
        pos -= 1; // Skip I
    }
    return pos;
}

/**
 * Convert a position to letter (1=A, 2=B, ..., skipping I).
 * Second letters A-Z excluding I = 25 positions (1-25).
 */
function positionToLetter(pos) {
    if (pos < 1 || pos > 25) {
        throw new MPCDesignationError(`Invalid letter position: ${pos}`);
    }

    if (pos >= 9) {
        pos += 1; // Skip I
    }
    return String.fromCharCode('A'.charCodeAt(0) + pos - 1);
}

/**
 * Convert cycle count and second letter to sequence number.
 */
function cycleLetterToSequence(cycle, letter) {
    return cycle * 25 + letterToPosition(letter);
}

/**
 * Convert sequence number to cycle count and second letter.
 */
function sequenceToCycleLetter(sequence) {
    const cycle = Math.floor((sequence - 1) / 25);
    const pos = ((sequence - 1) % 25) + 1;
    return [cycle, positionToLetter(pos)];
}

// =============================================================================
// Extended provisional format (cycle >= 620)
// =============================================================================

/**
 * Check if a cycle count requires the extended underscore format.
 */
function needsExtendedFormat(cycle) {
    return cycle >= 620;
}

/**
 * Pack a provisional designation using extended format (underscore prefix).
 */
function packExtendedProvisional(year, halfMonth, secondLetter, cycle) {
    const yearShort = year % 100;
    const baseSequence = (cycle - 620) * 25 + letterToPosition(secondLetter) - 1;
    const seqEncoded = numToBase62String(baseSequence, 4);
    return '_' + numToBase62(yearShort) + halfMonth + seqEncoded;
}

/**
 * Unpack an extended provisional designation (underscore format).
 */
function unpackExtendedProvisional(packed) {
    packed = packed.trim();
    if (packed.length !== 7 || packed[0] !== '_') {
        throw new MPCDesignationError(`Invalid extended packed provisional: ${packed}`);
    }

    const yearDigit = packed[1];
    const halfMonth = packed[2];
    const seqEncoded = packed.substring(3, 7);

    const baseSequence = base62StringToNum(seqEncoded);
    const cycle = 620 + Math.floor(baseSequence / 25);
    const letterPos = (baseSequence % 25) + 1;
    const secondLetter = positionToLetter(letterPos);

    // Year code is base-62: digit (0-9 for 2000-2009) or letter (A=10 for 2010, etc.)
    const year = 2000 + base62ToNum(yearDigit);

    return `${year} ${halfMonth}${secondLetter}${cycle}`;
}

// =============================================================================
// Standard provisional asteroid designations
// =============================================================================

/**
 * Unpack a provisional asteroid designation.
 */
function unpackProvisional(packed) {
    packed = packed.trim();

    // Check for survey designations first
    if (packed.length === 7 && SURVEY_PACKED_TO_UNPACKED[packed.substring(0, 3)]) {
        const survey = SURVEY_PACKED_TO_UNPACKED[packed.substring(0, 3)];
        const number = packed.substring(3, 7);
        return `${parseInt(number, 10)} ${survey}`;
    }

    if (packed.length !== 7) {
        throw new MPCDesignationError(`Invalid packed provisional designation length: ${packed}`);
    }

    const century = packed[0];
    const year = packed.substring(1, 3);
    const halfMonth = packed[3];
    const orderEncoded = packed.substring(4, 6);
    const secondLetter = packed[6];

    // Asteroid provisionals: only I-L valid (1800-2199)
    if (century !== 'I' && century !== 'J' && century !== 'K' && century !== 'L') {
        throw new MPCDesignationError(
            `Invalid century code for asteroid provisional: ${century} (must be I-L for years 1800-2199)`
        );
    }

    if (!CENTURY_CODES[century]) {
        throw new MPCDesignationError(`Invalid century code: ${century}`);
    }

    const fullYear = `${CENTURY_CODES[century]}${year}`;
    const yearNum = parseInt(fullYear, 10);
    const orderNum = decodeCycleCount(orderEncoded);

    // For pre-1925 designations, use A-prefix format (MPC canonical)
    if (yearNum < 1925) {
        const firstDigit = fullYear[0];
        const restOfYear = fullYear.substring(1);
        let prefix = '';
        if (firstDigit === '1') {
            prefix = 'A';
        } else if (firstDigit === '2') {
            prefix = 'B';
        }
        if (prefix) {
            if (orderNum === 0) {
                return `${prefix}${restOfYear} ${halfMonth}${secondLetter}`;
            }
            return `${prefix}${restOfYear} ${halfMonth}${secondLetter}${orderNum}`;
        }
    }

    if (orderNum === 0) {
        return `${fullYear} ${halfMonth}${secondLetter}`;
    } else {
        return `${fullYear} ${halfMonth}${secondLetter}${orderNum}`;
    }
}

/**
 * Pack a provisional asteroid designation.
 */
function packProvisional(unpacked) {
    unpacked = unpacked.trim();

    // Check for survey designations
    let match = unpacked.match(/^(\d+) (P-L|T-[123])$/);
    if (match) {
        const number = parseInt(match[1], 10);
        const survey = match[2];
        if (number < 1) {
            throw new MPCDesignationError(`Survey number must be positive: ${number}`);
        }
        return SURVEY_UNPACKED_TO_PACKED[survey] + number.toString().padStart(4, '0');
    }

    // Check for old-style designation: "A908 CJ" or "B842 FA"
    match = unpacked.match(/^([AB])(\d)(\d{2}) ([A-Z])([A-Z])$/);
    if (match) {
        const centuryDigit = match[2];
        const yearShort = match[3];
        const halfMonth = match[4];
        const secondLetter = match[5];

        let centuryCode;
        if (centuryDigit === '8') {
            centuryCode = 'I';
        } else if (centuryDigit === '9') {
            centuryCode = 'J';
        } else if (centuryDigit === '0') {
            centuryCode = 'K';
        } else {
            throw new MPCDesignationError(`Invalid century digit in old-style designation: ${centuryDigit}`);
        }

        return `${centuryCode}${yearShort}${halfMonth}00${secondLetter}`;
    }

    // Match standard provisional: "1995 XA" or "1995 XA12"
    match = unpacked.match(/^(\d{4}) ([A-Z])([A-Z])(\d*)$/);
    if (!match) {
        throw new MPCDesignationError(`Invalid unpacked provisional designation: ${unpacked}`);
    }

    const year = match[1];
    const halfMonth = match[2];
    const secondLetter = match[3];
    const orderStr = match[4];

    if (!isValidHalfMonth(halfMonth)) {
        throw new MPCDesignationError(`Invalid half-month letter: ${halfMonth}`);
    }

    // Asteroid provisionals: only years 1800-2199 valid
    const yearInt = parseInt(year, 10);
    if (yearInt < 1800 || yearInt > 2199) {
        throw new MPCDesignationError(
            `Year out of range for asteroid provisional: ${year} (must be 1800-2199)`
        );
    }

    const century = year.substring(0, 2);
    const yearShort = year.substring(2, 4);

    if (!REVERSE_CENTURY_CODES[parseInt(century, 10)]) {
        throw new MPCDesignationError(`Invalid century in year: ${year}`);
    }

    const centuryCode = REVERSE_CENTURY_CODES[parseInt(century, 10)];
    const orderNum = orderStr ? parseInt(orderStr, 10) : 0;

    if (needsExtendedFormat(orderNum)) {
        return packExtendedProvisional(parseInt(year, 10), halfMonth, secondLetter, orderNum);
    }

    const orderEncoded = encodeCycleCount(orderNum);
    return `${centuryCode}${yearShort}${halfMonth}${orderEncoded}${secondLetter}`;
}

// =============================================================================
// Comet provisional designations
// =============================================================================

/**
 * Unpack a comet provisional designation.
 */
function unpackCometProvisional(packed) {
    packed = packed.trim();
    const length = packed.length;

    if (length !== 7 && length !== 8) {
        throw new MPCDesignationError(`Invalid packed comet provisional designation length: ${packed}`);
    }

    const century = packed[0];
    const year = packed.substring(1, 3);
    const halfMonth = packed[3];
    const orderEncoded = packed.substring(4, 6);

    let fragment;
    if (length === 7) {
        fragment = packed[6];
    } else {
        fragment = packed.substring(6, 8);
    }

    if (!CENTURY_CODES[century]) {
        throw new MPCDesignationError(`Invalid century code: ${century}`);
    }

    const fullYear = `${CENTURY_CODES[century]}${year}`;
    const orderNum = decodeCycleCount(orderEncoded);

    let result = `${fullYear} ${halfMonth}${orderNum}`;

    if (fragment !== '0') {
        const fragmentLetter = fragment.toUpperCase();
        result += `-${fragmentLetter}`;
    }

    return result;
}

/**
 * Pack a comet provisional designation.
 */
function packCometProvisional(unpacked) {
    unpacked = unpacked.trim();

    const match = unpacked.match(/^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$/);
    if (!match) {
        throw new MPCDesignationError(`Invalid unpacked comet provisional designation: ${unpacked}`);
    }

    const year = match[1];
    const halfMonth = match[2];
    const orderNum = parseInt(match[3], 10);
    const fragment = match[4];

    if (orderNum < 1) {
        throw new MPCDesignationError(`Comet order number must be positive: ${orderNum}`);
    }

    const century = year.substring(0, 2);
    const yearShort = year.substring(2, 4);

    if (!REVERSE_CENTURY_CODES[parseInt(century, 10)]) {
        throw new MPCDesignationError(`Invalid century in year: ${year}`);
    }

    const centuryCode = REVERSE_CENTURY_CODES[parseInt(century, 10)];
    const orderEncoded = encodeCycleCount(orderNum);

    const fragmentCode = fragment ? fragment.toLowerCase() : '0';

    return `${centuryCode}${yearShort}${halfMonth}${orderEncoded}${fragmentCode}`;
}

// =============================================================================
// Numbered comet designations
// =============================================================================

/**
 * Unpack a numbered periodic comet designation.
 * Supports fragments: 0073Pa -> 73P-A, 0073Paa -> 73P-AA
 */
function unpackCometNumbered(packed) {
    packed = packed.trim();

    // Match with optional lowercase fragment: 0073P, 0073Pa, or 0073Paa
    const match = packed.match(/^(\d{4})([PD])([a-z]{1,2})?$/);
    if (!match) {
        throw new MPCDesignationError(`Invalid packed numbered comet designation: ${packed}`);
    }

    const number = parseInt(match[1], 10);
    const cometType = match[2];
    const fragment = match[3] || '';

    let result = `${number}${cometType}`;
    if (fragment) {
        result += `-${fragment.toUpperCase()}`;
    }
    return result;
}

/**
 * Pack a numbered periodic comet designation.
 * Supports fragments: 73P-A -> 0073Pa, 73P-AA -> 0073Paa
 */
function packCometNumbered(unpacked) {
    unpacked = unpacked.trim();

    // Match "1P" or "354P" or "73P-A" or "73P-AA" or "1P/Halley" (with optional name after slash)
    const match = unpacked.match(/^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:\/[A-Za-z].*)?$/);
    if (!match) {
        throw new MPCDesignationError(`Invalid unpacked numbered comet designation: ${unpacked}`);
    }

    const number = parseInt(match[1], 10);
    const cometType = match[2];
    const fragment = match[3] || '';

    if (number < 1 || number > 9999) {
        throw new MPCDesignationError(`Comet number out of range (1-9999): ${number}`);
    }

    let result = number.toString().padStart(4, '0') + cometType;
    if (fragment) {
        result += fragment.toLowerCase();
    }
    return result;
}

// =============================================================================
// Natural satellite designations
// =============================================================================

/**
 * Unpack a natural satellite provisional designation.
 */
function unpackSatellite(packed) {
    packed = packed.trim();

    if (packed.length !== 8 || packed[0] !== 'S') {
        throw new MPCDesignationError(`Invalid packed satellite designation: ${packed}`);
    }

    const century = packed[1];
    const year = packed.substring(2, 4);
    const planet = packed[4];
    const numberEncoded = packed.substring(5, 7);

    if (!CENTURY_CODES[century]) {
        throw new MPCDesignationError(`Invalid century code: ${century}`);
    }

    if (!SATELLITE_PLANETS.has(planet)) {
        throw new MPCDesignationError(`Invalid planet code: ${planet}`);
    }

    const fullYear = `${CENTURY_CODES[century]}${year}`;
    const number = decodeCycleCount(numberEncoded);

    return `S/${fullYear} ${planet} ${number}`;
}

/**
 * Pack a natural satellite provisional designation.
 */
function packSatellite(unpacked) {
    unpacked = unpacked.trim();

    const match = unpacked.match(/^S\/(\d{4}) ([JSUN]) (\d+)$/);
    if (!match) {
        throw new MPCDesignationError(`Invalid unpacked satellite designation: ${unpacked}`);
    }

    const year = match[1];
    const planet = match[2];
    const number = parseInt(match[3], 10);

    if (number < 1) {
        throw new MPCDesignationError(`Satellite number must be positive: ${number}`);
    }

    const century = year.substring(0, 2);
    const yearShort = year.substring(2, 4);

    if (!REVERSE_CENTURY_CODES[parseInt(century, 10)]) {
        throw new MPCDesignationError(`Invalid century in year: ${year}`);
    }

    const centuryCode = REVERSE_CENTURY_CODES[parseInt(century, 10)];
    const numberEncoded = encodeCycleCount(number);

    return `S${centuryCode}${yearShort}${planet}${numberEncoded}0`;
}

// =============================================================================
// BCE year encoding for ancient comets
// =============================================================================

/**
 * Encode a BCE year for packed format.
 */
function encodeBceYear(year) {
    if (year >= 0) {
        throw new MPCDesignationError(`Not a BCE year: ${year}`);
    }

    const absYear = Math.abs(year);
    const code = 99 - (absYear % 100);

    if (absYear < 100) {
        return ['/', code.toString().padStart(2, '0')];
    } else if (absYear < 200) {
        return ['.', code.toString().padStart(2, '0')];
    } else if (absYear < 300) {
        return ['-', code.toString().padStart(2, '0')];
    } else {
        throw new MPCDesignationError(`BCE year out of supported range: ${year}`);
    }
}

/**
 * Decode a BCE year from packed format.
 */
function decodeBceYear(prefix, code) {
    const codeNum = parseInt(code, 10);
    const yearPart = 99 - codeNum;

    if (prefix === '/') {
        return -yearPart;
    } else if (prefix === '.') {
        return -(yearPart + 100);
    } else if (prefix === '-') {
        return -(yearPart + 200);
    } else {
        throw new MPCDesignationError(`Invalid BCE prefix: ${prefix}`);
    }
}

/**
 * Check if a year is ancient (< 1000) or BCE.
 */
function isAncientYear(year) {
    return year < 1000;
}

// =============================================================================
// Ancient/BCE comet provisional designations
// =============================================================================

/**
 * Pack an ancient or BCE comet provisional designation.
 */
function packAncientCometProvisional(cometType, year, halfMonth, orderNum, fragment = '') {
    const orderEncoded = encodeCycleCount(orderNum);
    const fragmentCode = fragment ? fragment.toLowerCase() : '0';

    if (year < 0) {
        const [prefix, code] = encodeBceYear(year);
        return `${cometType}${prefix}${code}${halfMonth}${orderEncoded}${fragmentCode}`;
    } else {
        return `${cometType}${year.toString().padStart(3, '0')}${halfMonth}${orderEncoded}${fragmentCode}`;
    }
}

/**
 * Unpack an ancient or BCE comet provisional designation.
 */
function unpackAncientCometProvisional(packed) {
    packed = packed.trim();

    if (packed.length !== 8) {
        throw new MPCDesignationError(`Invalid ancient comet designation length: ${packed}`);
    }

    const cometType = packed[0];

    if (!COMET_TYPES.has(cometType)) {
        throw new MPCDesignationError(`Invalid comet type: ${cometType}`);
    }

    let year, halfMonth, orderEncoded, fragment;

    if ('/.-'.includes(packed[1])) {
        const prefix = packed[1];
        const yearCode = packed.substring(2, 4);
        year = decodeBceYear(prefix, yearCode);
        halfMonth = packed[4];
        orderEncoded = packed.substring(5, 7);
        fragment = packed[7];
    } else {
        year = parseInt(packed.substring(1, 4), 10);
        halfMonth = packed[4];
        orderEncoded = packed.substring(5, 7);
        fragment = packed[7];
    }

    const orderNum = decodeCycleCount(orderEncoded);
    let result = `${cometType}/${year} ${halfMonth}${orderNum}`;

    if (fragment !== '0') {
        result += `-${fragment.toUpperCase()}`;
    }

    return result;
}

// =============================================================================
// Helper functions for comet format detection
// =============================================================================

/**
 * Check if a 7-char provisional uses asteroid-style (ends with uppercase letter).
 */
function isAsteroidStylePacked(provisionalPart) {
    if (provisionalPart.length !== 7) {
        return false;
    }
    const lastChar = provisionalPart[6];
    return /^[A-Z]$/.test(lastChar);
}

/**
 * Check if an unpacked provisional uses asteroid-style (letter after half-month).
 */
function isAsteroidStyleUnpacked(provisional) {
    const match = provisional.match(/^\d{4} ([A-Z])(.)/);
    if (match) {
        const secondChar = match[2];
        return /^[A-Za-z]$/.test(secondChar);
    }
    return false;
}

// =============================================================================
// Full comet designations (with type prefix)
// =============================================================================

/**
 * Unpack a full comet designation (type + provisional or numbered).
 */
function unpackCometFull(packed) {
    let length = packed.length;

    let cometType, provisionalPart, numStr;

    if (length === 8) {
        cometType = packed[0];
        provisionalPart = packed.substring(1, 8);
        numStr = '';
    } else if (length === 9) {
        cometType = packed[0];
        provisionalPart = packed.substring(1, 9);
        numStr = '';
    } else if (length === 12 || (length < 12 && packed[0] === ' ')) {
        while (packed.length < 12) {
            packed = ' ' + packed;
        }
        const numPart = packed.substring(0, 4);
        cometType = packed[4];
        provisionalPart = packed.substring(5, 12);
        numStr = numPart.trim();
    } else {
        throw new MPCDesignationError(`Invalid packed full comet designation length: ${packed}`);
    }

    if (!COMET_TYPES.has(cometType)) {
        throw new MPCDesignationError(`Invalid comet type: ${cometType}`);
    }

    let provisional;
    if (isAsteroidStylePacked(provisionalPart)) {
        provisional = unpackProvisional(provisionalPart);
    } else {
        provisional = unpackCometProvisional(provisionalPart);
    }

    if (numStr === '') {
        return `${cometType}/${provisional}`;
    } else {
        const number = parseInt(numStr, 10);
        return `${number}${cometType}/${provisional}`;
    }
}

/**
 * Pack a full comet designation.
 */
function packCometFull(unpacked) {
    unpacked = unpacked.trim();

    const match = unpacked.match(/^(\d*)([PCDXAI])\/(-?\d+) (.+)$/);
    if (!match) {
        throw new MPCDesignationError(`Invalid unpacked comet designation: ${unpacked}`);
    }

    const number = match[1];
    const cometType = match[2];
    const year = parseInt(match[3], 10);
    const provPart = match[4];

    if (!COMET_TYPES.has(cometType)) {
        throw new MPCDesignationError(`Invalid comet type: ${cometType}`);
    }

    if (isAncientYear(year)) {
        const ancientMatch = provPart.match(/^([A-Z])(\d+)(?:-([A-Z]))?$/);
        if (ancientMatch) {
            const halfMonth = ancientMatch[1];
            const orderNum = parseInt(ancientMatch[2], 10);
            const fragment = ancientMatch[3] || '';
            return packAncientCometProvisional(cometType, year, halfMonth, orderNum, fragment);
        } else {
            throw new MPCDesignationError(`Invalid ancient comet provisional: ${provPart}`);
        }
    }

    const provisional = `${year} ${provPart}`;

    let provisionalPacked;
    if (isAsteroidStyleUnpacked(provisional)) {
        provisionalPacked = packProvisional(provisional);
    } else {
        provisionalPacked = packCometProvisional(provisional);
    }

    if (number === '') {
        return `${cometType}${provisionalPacked}`;
    } else {
        const num = parseInt(number, 10);
        if (num < 1 || num > 9999) {
            throw new MPCDesignationError(`Comet number out of range (1-9999): ${num}`);
        }
        return num.toString().padStart(4, '0') + cometType + provisionalPacked;
    }
}

// =============================================================================
// Format detection
// =============================================================================

/**
 * Detect if a designation is packed or unpacked and what type it is.
 */
function detectFormat(designation) {
    const result = { format: '', type: '', subtype: '' };

    validateRawInput(designation);

    // Check for packed full comet designation BEFORE trimming (12 chars with spaces)
    if (designation.length === 12) {
        if (/^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$/.test(designation)) {
            result.format = 'packed';
            result.type = 'comet_full';
            result.subtype = 'comet with provisional designation (12-char)';
            return result;
        }
    }

    // Check for packed comet designation (8 chars: type + 7 char provisional)
    if (designation.length === 8) {
        if (/^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$/.test(designation)) {
            result.format = 'packed';
            result.type = 'comet_full';
            result.subtype = 'comet with provisional designation (8-char)';
            return result;
        }
    }

    // Check for packed comet with 2-letter fragment (9 chars)
    if (designation.length === 9) {
        if (/^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$/.test(designation)) {
            result.format = 'packed';
            result.type = 'comet_full';
            result.subtype = 'comet with provisional designation (9-char, 2-letter fragment)';
            return result;
        }
    }

    // Check for packed ancient comet (8 chars: type + 3-digit year + provisional)
    if (designation.length === 8) {
        if (/^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/.test(designation)) {
            result.format = 'packed';
            result.type = 'comet_ancient';
            result.subtype = 'comet with ancient provisional (year < 1000)';
            return result;
        }
    }

    // Check for packed BCE comet (8 chars: type + BCE prefix + code + provisional)
    if (designation.length === 8) {
        if (/^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/.test(designation)) {
            result.format = 'packed';
            result.type = 'comet_bce';
            result.subtype = 'comet with BCE provisional';
            return result;
        }
    }

    const des = designation.trim();
    validateWhitespace(des);

    // Check for packed satellite designation (8 chars starting with S)
    if (des.length === 8 && des[0] === 'S') {
        if (/^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$/.test(des)) {
            result.format = 'packed';
            result.type = 'satellite';
            const planet = des[4];
            const planetName = SATELLITE_PLANET_NAMES[planet] || planet;
            result.subtype = `natural satellite (${planetName})`;
            return result;
        }
    }

    // Check for packed permanent (numbered) asteroid
    if (des.length === 5) {
        if (des[0] === '~') {
            if (/^~[0-9A-Za-z]{4}$/.test(des)) {
                result.format = 'packed';
                result.type = 'permanent';
                result.subtype = 'permanent numbered (tilde/base-62, >= 620000)';
                return result;
            }
        } else if (/^\d{5}$/.test(des)) {
            result.format = 'packed';
            result.type = 'permanent';
            result.subtype = 'permanent numbered (5-digit, < 100000)';
            return result;
        } else if (/^[A-Za-z][0-9]{4}$/.test(des)) {
            result.format = 'packed';
            result.type = 'permanent';
            if (/^[A-Z]/.test(des[0])) {
                result.subtype = 'permanent numbered (letter-prefix, 100000-359999)';
            } else {
                result.subtype = 'permanent numbered (letter-prefix, 360000-619999)';
            }
            return result;
        }
    }

    // Check for packed provisional asteroid (7 chars)
    if (des.length === 7) {
        // Extended format with underscore
        if (des[0] === '_') {
            if (/^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$/.test(des)) {
                result.format = 'packed';
                result.type = 'provisional_extended';
                result.subtype = 'provisional (extended format, cycle >=620)';
                return result;
            }
        }
        // Standard provisional or survey
        if (/^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$/.test(des)) {
            result.format = 'packed';
            result.type = 'provisional';
            result.subtype = 'provisional';
            return result;
        }
        if (des.startsWith('PLS') && /^\d{4}$/.test(des.substring(3))) {
            result.format = 'packed';
            result.type = 'survey';
            result.subtype = 'survey (Palomar-Leiden)';
            return result;
        }
        if (/^T[123]S\d{4}$/.test(des)) {
            result.format = 'packed';
            result.type = 'survey';
            result.subtype = `survey (Trojan T-${des[1]})`;
            return result;
        }
    }

    // Check for packed numbered comet (5 chars ending in P or D)
    if (des.length === 5) {
        if (/^[0-9]{4}[PD]$/.test(des)) {
            result.format = 'packed';
            result.type = 'comet_numbered';
            result.subtype = `comet numbered`;
            return result;
        }
    }

    // Check for packed numbered comet with fragment (6-7 chars: ####Pa or ####Paa)
    if (des.length === 6 || des.length === 7) {
        if (/^[0-9]{4}[PD][a-z]{1,2}$/.test(des)) {
            result.format = 'packed';
            result.type = 'comet_numbered';
            const fragLen = des.length - 5;
            if (fragLen === 1) {
                result.subtype = 'comet numbered with fragment';
            } else {
                result.subtype = 'comet numbered with 2-letter fragment';
            }
            return result;
        }
    }

    // Check for packed comet provisional (7 chars starting with century code)
    if (des.length === 7) {
        if (/^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$/.test(des)) {
            result.format = 'packed';
            result.type = 'comet_provisional';
            result.subtype = 'comet provisional';
            return result;
        }
    }

    // --- UNPACKED FORMATS ---

    // Check for unpacked satellite: "S/2019 S 22"
    let match = des.match(/^S\/\d{4} ([JSUN]) \d+$/);
    if (match) {
        result.format = 'unpacked';
        result.type = 'satellite';
        const planet = match[1];
        const planetName = SATELLITE_PLANET_NAMES[planet] || planet;
        result.subtype = `natural satellite (${planetName})`;
        return result;
    }

    // Check for unpacked permanent (numbered) asteroid
    if (/^\d+$/.test(des)) {
        result.format = 'unpacked';
        result.type = 'permanent';
        result.subtype = 'permanent numbered';
        return result;
    }

    // Check for unpacked survey designation: "2040 P-L" or "3138 T-1"
    match = des.match(/^\d+ (P-L|T-[123])$/);
    if (match) {
        result.format = 'unpacked';
        result.type = 'survey';
        const survey = match[1];
        if (survey === 'P-L') {
            result.subtype = 'survey (Palomar-Leiden)';
        } else {
            result.subtype = `survey (Trojan ${survey})`;
        }
        return result;
    }

    // Check for old-style asteroid designation: "A908 CJ"
    if (/^[AB]\d{3} [A-Z][A-Z]$/.test(des)) {
        result.format = 'unpacked';
        result.type = 'provisional';
        result.subtype = 'provisional (old-style pre-1925)';
        return result;
    }

    // Check for unpacked provisional asteroid: "1995 XA" or "2024 AB12"
    if (/^\d{4} [A-Z][A-Z]\d*$/.test(des)) {
        result.format = 'unpacked';
        result.type = 'provisional';
        result.subtype = 'provisional';
        return result;
    }

    // Check for unpacked comet with type prefix
    match = des.match(/^(\d*)([PCDXAI])\/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$/);
    if (match) {
        result.format = 'unpacked';
        result.type = 'comet_full';
        result.subtype = 'comet provisional';
        return result;
    }

    // Check for unpacked numbered periodic comet "1P" or "354P" or "73P-A"
    match = des.match(/^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:\/[A-Za-z].*)?$/);
    if (match) {
        result.format = 'unpacked';
        result.type = 'comet_numbered';
        const fragment = match[3] || '';
        if (fragment) {
            if (fragment.length === 1) {
                result.subtype = 'comet numbered with fragment';
            } else {
                result.subtype = 'comet numbered with 2-letter fragment';
            }
        } else {
            result.subtype = 'comet numbered';
        }
        return result;
    }

    throw new MPCDesignationError(`Unable to detect designation format: ${designation}`);
}

// =============================================================================
// Main conversion functions
// =============================================================================

/**
 * Convert a designation between packed and unpacked formats.
 */
function convert(designation) {
    const info = detectFormat(designation);
    const fmt = info.format;
    const dtype = info.type;

    let result = '';

    if (fmt === 'packed') {
        if (dtype === 'permanent') {
            result = String(unpackPermanent(designation));
        } else if (dtype === 'provisional' || dtype === 'survey') {
            result = unpackProvisional(designation);
        } else if (dtype === 'provisional_extended') {
            result = unpackExtendedProvisional(designation);
        } else if (dtype === 'comet_numbered') {
            result = unpackCometNumbered(designation);
        } else if (dtype === 'comet_provisional') {
            result = unpackCometProvisional(designation);
        } else if (dtype === 'comet_full') {
            result = unpackCometFull(designation);
        } else if (dtype === 'comet_ancient' || dtype === 'comet_bce') {
            result = unpackAncientCometProvisional(designation);
        } else if (dtype === 'satellite') {
            result = unpackSatellite(designation);
        }
    } else {
        if (dtype === 'permanent') {
            result = packPermanent(parseInt(designation, 10));
        } else if (dtype === 'provisional' || dtype === 'survey') {
            result = packProvisional(designation);
        } else if (dtype === 'comet_numbered') {
            result = packCometNumbered(designation);
        } else if (dtype === 'comet_full') {
            result = packCometFull(designation);
        } else if (dtype === 'satellite') {
            result = packSatellite(designation);
        }
    }

    return {
        input: designation,
        output: result,
        info: info
    };
}

/**
 * Convert a designation between packed and unpacked formats.
 * Returns just the converted string.
 */
function convertSimple(designation) {
    return convert(designation).output;
}

/**
 * Ensure a designation is in packed format.
 */
function pack(designation) {
    const info = detectFormat(designation);

    if (info.format === 'packed') {
        return designation.trim();
    }

    return convert(designation).output;
}

/**
 * Ensure a designation is in unpacked (human-readable) format.
 */
function unpack(designation) {
    const info = detectFormat(designation);

    if (info.format === 'unpacked') {
        return designation.trim();
    }

    return convert(designation).output;
}

/**
 * Check if a string is a valid MPC designation.
 */
function isValidDesignation(designation) {
    if (!designation || typeof designation !== 'string') {
        return false;
    }
    try {
        detectFormat(designation);
        return true;
    } catch (e) {
        return false;
    }
}

// =============================================================================
// Helper Functions for Format Conversion and Fragment Handling
// =============================================================================

/**
 * Convert minimal packed format to 12-character MPC report format.
 * The 12-character format is used in MPC observation records (columns 1-12).
 * For numbered comets with fragments, the fragment letter(s) go in columns 11-12.
 *
 * Examples:
 *   "0073Pa"   -> "0073P      a" (numbered comet with single fragment)
 *   "0073Paa"  -> "0073P     aa" (numbered comet with double fragment)
 *   "00001"    -> "       00001" (numbered asteroid)
 *   "J95X00A"  -> "     J95X00A" (provisional asteroid)
 *   "CJ95O010" -> "    CJ95O010" (provisional comet)
 */
function toReportFormat(minimal) {
    minimal = minimal.trim();
    const length = minimal.length;

    const info = detectFormat(minimal);

    if (info.format !== 'packed') {
        throw new MPCDesignationError(`toReportFormat requires packed format input: ${minimal}`);
    }

    // Initialize 12-char output with spaces
    let report = '            '.split('');

    switch (info.type) {
        case 'permanent':
            // Right-align 5-char designation
            for (let i = 0; i < length; i++) {
                report[12 - length + i] = minimal[i];
            }
            break;

        case 'provisional':
        case 'provisional_extended':
        case 'survey':
            // Right-align 7-char designation
            for (let i = 0; i < length; i++) {
                report[12 - length + i] = minimal[i];
            }
            break;

        case 'comet_numbered':
            // Numbered comet: first 5 chars (####P), fragment in cols 11-12
            if (length === 5) {
                // No fragment
                for (let i = 0; i < 5; i++) {
                    report[i] = minimal[i];
                }
            } else if (length === 6) {
                // Single-letter fragment
                for (let i = 0; i < 5; i++) {
                    report[i] = minimal[i];
                }
                report[11] = minimal[5];
            } else if (length === 7) {
                // Two-letter fragment
                for (let i = 0; i < 5; i++) {
                    report[i] = minimal[i];
                }
                report[10] = minimal[5];
                report[11] = minimal[6];
            }
            break;

        case 'comet_provisional':
        case 'comet_full':
        case 'comet_ancient':
        case 'comet_bce':
            // Right-align in 12-char field
            for (let i = 0; i < length; i++) {
                report[12 - length + i] = minimal[i];
            }
            break;

        case 'satellite':
            // Right-align 8-char designation
            for (let i = 0; i < length; i++) {
                report[12 - length + i] = minimal[i];
            }
            break;

        default:
            throw new MPCDesignationError(`Unsupported type for report format: ${info.type}`);
    }

    return report.join('');
}

/**
 * Convert 12-character MPC report format to minimal packed format.
 */
function fromReportFormat(report) {
    if (report.length > 12) {
        throw new MPCDesignationError(`Report format too long: ${report}`);
    }

    // Pad to 12 chars if shorter
    while (report.length < 12) {
        report = ' ' + report;
    }

    // Check for numbered comet with fragment (fragment in cols 11-12)
    // Pattern: ####P or ####D in cols 1-5, spaces in cols 6-10, lowercase in cols 11-12
    const first5 = report.substring(0, 5);
    const middle = report.substring(5, 10);
    const frag1 = report[10];
    const frag2 = report[11];

    // Check if this is a numbered comet format
    if (/^[0-9]{4}[PD]$/.test(first5) && middle.trim() === '') {
        let result = first5;
        if (frag1 >= 'a' && frag1 <= 'z') {
            result += frag1;
        }
        if (frag2 >= 'a' && frag2 <= 'z') {
            result += frag2;
        }
        return result;
    }

    // Standard case: just trim spaces
    return report.trim();
}

/**
 * Check if a designation has a comet fragment suffix.
 * Works with both packed and unpacked formats.
 */
function hasFragment(desig) {
    let info;
    try {
        info = detectFormat(desig);
    } catch (e) {
        return false;
    }

    const dtype = info.type;

    // Only comets can have fragments
    if (dtype !== 'comet_numbered' && dtype !== 'comet_provisional' && dtype !== 'comet_full') {
        return false;
    }

    desig = desig.trim();
    const length = desig.length;

    if (info.format === 'unpacked') {
        // Look for "-X" or "-XX" at end
        return /-[A-Z]{1,2}$/.test(desig);
    } else {
        // Packed format
        if (dtype === 'comet_numbered') {
            // Check for lowercase after P/D (position 5+)
            if (length > 5) {
                const c = desig[5];
                return c >= 'a' && c <= 'z';
            }
        } else if (dtype === 'comet_provisional') {
            // 7-char: last char lowercase and not '0'
            const lastChar = desig[length - 1];
            return lastChar >= 'a' && lastChar <= 'z' && lastChar !== '0';
        } else if (dtype === 'comet_full') {
            const lastChar = desig[length - 1];
            return lastChar >= 'a' && lastChar <= 'z' && lastChar !== '0';
        }
    }
    return false;
}

/**
 * Extract the fragment suffix from a comet designation.
 * Works with both packed and unpacked formats.
 * Fragment is returned in uppercase (e.g., "A", "AA").
 * Returns empty string if no fragment.
 */
function getFragment(desig) {
    const info = detectFormat(desig);
    const dtype = info.type;

    // Only comets can have fragments
    if (dtype !== 'comet_numbered' && dtype !== 'comet_provisional' && dtype !== 'comet_full') {
        return '';
    }

    desig = desig.trim();
    const length = desig.length;

    if (info.format === 'unpacked') {
        // Look for "-X" or "-XX" at end
        const match = desig.match(/-([A-Z]{1,2})$/);
        if (match) {
            return match[1];
        }
    } else {
        // Packed format
        if (dtype === 'comet_numbered') {
            // Fragment is lowercase after P/D
            if (length === 6) {
                return desig[5].toUpperCase();
            } else if (length === 7) {
                return desig.substring(5, 7).toUpperCase();
            }
        } else if (dtype === 'comet_provisional') {
            // 7-char: position 6 if lowercase and not '0'
            // 8-char: positions 6-7 if lowercase
            if (length === 7) {
                const lastChar = desig[6];
                if (lastChar >= 'a' && lastChar <= 'z' && lastChar !== '0') {
                    return lastChar.toUpperCase();
                }
            } else if (length === 8) {
                const frag = desig.substring(6, 8);
                if (frag[0] >= 'a' && frag[0] <= 'z' && frag[1] >= 'a' && frag[1] <= 'z') {
                    return frag.toUpperCase();
                }
            }
        } else if (dtype === 'comet_full') {
            // 8-char: position 7 if lowercase and not '0'
            // 9-char: positions 7-8 if lowercase
            if (length === 8) {
                const lastChar = desig[7];
                if (lastChar >= 'a' && lastChar <= 'z' && lastChar !== '0') {
                    return lastChar.toUpperCase();
                }
            } else if (length === 9) {
                const frag = desig.substring(7, 9);
                if (frag[0] >= 'a' && frag[0] <= 'z' && frag[1] >= 'a' && frag[1] <= 'z') {
                    return frag.toUpperCase();
                }
            }
        }
    }

    return '';
}

/**
 * Get the parent comet designation (without fragment suffix).
 * Works with both packed and unpacked formats.
 * Returns the designation in the same format (packed or unpacked) as input.
 */
function getParent(desig) {
    const info = detectFormat(desig);
    const dtype = info.type;

    // Non-comets: return as-is
    if (dtype !== 'comet_numbered' && dtype !== 'comet_provisional' && dtype !== 'comet_full') {
        return desig.trim();
    }

    desig = desig.trim();
    const length = desig.length;

    if (info.format === 'unpacked') {
        // Remove "-X" or "-XX" suffix if present
        return desig.replace(/-[A-Z]{1,2}$/, '');
    } else {
        // Packed format
        if (dtype === 'comet_numbered') {
            // Remove lowercase fragment letters after P/D
            if (length > 5) {
                const c = desig[5];
                if (c >= 'a' && c <= 'z') {
                    return desig.substring(0, 5);
                }
            }
        } else if (dtype === 'comet_provisional') {
            // 7-char: replace lowercase fragment with '0'
            // 8-char: replace 2 lowercase with '0', truncate
            if (length === 7) {
                const lastChar = desig[6];
                if (lastChar >= 'a' && lastChar <= 'z' && lastChar !== '0') {
                    return desig.substring(0, 6) + '0';
                }
            } else if (length === 8) {
                const c = desig[6];
                if (c >= 'a' && c <= 'z') {
                    return desig.substring(0, 6) + '0';
                }
            }
        } else if (dtype === 'comet_full') {
            // 8-char: replace fragment with '0'
            // 9-char: replace fragment with '0', truncate
            if (length === 8) {
                const lastChar = desig[7];
                if (lastChar >= 'a' && lastChar <= 'z' && lastChar !== '0') {
                    return desig.substring(0, 7) + '0';
                }
            } else if (length === 9) {
                const c = desig[7];
                if (c >= 'a' && c <= 'z') {
                    return desig.substring(0, 7) + '0';
                }
            }
        }
    }

    return desig;
}

/**
 * Check if two designations refer to the same object.
 * This function normalizes both designations to packed format and compares them,
 * handling different formats (packed/unpacked).
 */
function designationsEqual(desig1, desig2) {
    try {
        const packed1 = pack(desig1);
        const packed2 = pack(desig2);
        return packed1 === packed2;
    } catch (e) {
        return false;
    }
}

// =============================================================================
// Module exports
// =============================================================================

module.exports = {
    MPCDesignationError,
    convert,
    convertSimple,
    pack,
    unpack,
    detectFormat,
    isValidDesignation,
    packPermanent,
    unpackPermanent,
    packProvisional,
    unpackProvisional,
    packCometProvisional,
    unpackCometProvisional,
    packCometNumbered,
    unpackCometNumbered,
    packCometFull,
    unpackCometFull,
    packSatellite,
    unpackSatellite,
    // Helper functions
    toReportFormat,
    fromReportFormat,
    hasFragment,
    getFragment,
    getParent,
    designationsEqual,
    MAX_ASTEROID_NUMBER
};
