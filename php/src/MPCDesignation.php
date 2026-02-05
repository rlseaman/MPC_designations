<?php
/**
 * MPCDesignation - Convert between packed and unpacked MPC designations
 *
 * Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
 */

declare(strict_types=1);

namespace MPC;

class MPCDesignationException extends \Exception {}

class FormatInfo {
    public string $format;  // 'packed' or 'unpacked'
    public string $type;
    public string $subtype;

    public function __construct(string $format, string $type, string $subtype) {
        $this->format = $format;
        $this->type = $type;
        $this->subtype = $subtype;
    }
}

class ConversionResult {
    public string $input;
    public string $output;
    public FormatInfo $info;

    public function __construct(string $input, string $output, FormatInfo $info) {
        $this->input = $input;
        $this->output = $output;
        $this->info = $info;
    }
}

class MPCDesignation {
    public const VERSION = '1.0.0';
    public const MAX_ASTEROID_NUMBER = 15396335;

    private const BASE62_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

    private const CENTURY_CODES = [
        'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
        'G' => 16, 'H' => 17, 'I' => 18, 'J' => 19, 'K' => 20, 'L' => 21
    ];

    private const SURVEY_PACKED_TO_UNPACKED = [
        'PLS' => 'P-L', 'T1S' => 'T-1', 'T2S' => 'T-2', 'T3S' => 'T-3'
    ];

    private const COMET_TYPES = ['P', 'C', 'D', 'X', 'A', 'I'];

    private const COMET_TYPE_DESCRIPTIONS = [
        'P' => 'periodic', 'C' => 'non-periodic', 'D' => 'defunct',
        'X' => 'uncertain orbit', 'A' => 'asteroid with comet designation', 'I' => 'interstellar'
    ];

    private const SATELLITE_PLANETS = ['J', 'S', 'U', 'N'];

    private const SATELLITE_PLANET_NAMES = [
        'J' => 'Jupiter', 'S' => 'Saturn', 'U' => 'Uranus', 'N' => 'Neptune'
    ];

    // =========================================================================
    // Input validation
    // =========================================================================

    private static function validateRawInput(string $s): void {
        for ($i = 0; $i < strlen($s); $i++) {
            $code = ord($s[$i]);
            if ($code < 32 || $code > 126) {
                throw new MPCDesignationException(
                    sprintf("Invalid character in designation: '\\x%02x'", $code)
                );
            }
        }
    }

    private static function validateWhitespace(string $s): void {
        $prevSpace = false;
        for ($i = 0; $i < strlen($s); $i++) {
            $c = $s[$i];
            $code = ord($c);
            if ($code < 32 || $code > 126) {
                throw new MPCDesignationException("Invalid character in designation");
            }
            if ($c === ' ') {
                if ($prevSpace) {
                    throw new MPCDesignationException("Consecutive spaces in designation");
                }
                $prevSpace = true;
            } else {
                $prevSpace = false;
            }
        }
    }

    private static function sanitize(string $designation): string {
        self::validateRawInput($designation);
        $result = trim($designation);
        if ($result === '') {
            throw new MPCDesignationException("Empty designation");
        }
        return $result;
    }

    private static function isValidHalfMonth(string $c): bool {
        return $c >= 'A' && $c <= 'Y' && $c !== 'I';
    }

    // =========================================================================
    // Base-62 encoding utilities
    // =========================================================================

    private static function base62ToNum(string $c): int {
        $idx = strpos(self::BASE62_CHARS, $c);
        if ($idx === false) {
            throw new MPCDesignationException("Invalid base-62 character: $c");
        }
        return $idx;
    }

    private static function numToBase62(int $n): string {
        if ($n < 0 || $n > 61) {
            throw new MPCDesignationException("Number out of base-62 range: $n");
        }
        return self::BASE62_CHARS[$n];
    }

    private static function base62StringToNum(string $s): int {
        $result = 0;
        for ($i = 0; $i < strlen($s); $i++) {
            $result = $result * 62 + self::base62ToNum($s[$i]);
        }
        return $result;
    }

    private static function numToBase62String(int $n, int $width): string {
        $result = '';
        for ($i = 0; $i < $width; $i++) {
            $result = self::numToBase62($n % 62) . $result;
            $n = intdiv($n, 62);
        }
        return $result;
    }

    // =========================================================================
    // Cycle count encoding for provisional designations
    // =========================================================================

    private static function decodeCycleCount(string $encoded): int {
        if (strlen($encoded) < 2) {
            throw new MPCDesignationException("Invalid cycle count encoding");
        }

        $first = $encoded[0];
        $second = $encoded[1];

        if ($first >= '0' && $first <= '9') {
            $tens = ord($first) - ord('0');
        } elseif ($first >= 'A' && $first <= 'Z') {
            $tens = ord($first) - ord('A') + 10;
        } elseif ($first >= 'a' && $first <= 'z') {
            $tens = ord($first) - ord('a') + 36;
        } else {
            throw new MPCDesignationException("Invalid cycle count encoding");
        }

        if ($second < '0' || $second > '9') {
            throw new MPCDesignationException("Invalid cycle count encoding");
        }

        return $tens * 10 + (ord($second) - ord('0'));
    }

    private static function encodeCycleCount(int $count): string {
        if ($count < 0 || $count >= 620) {
            throw new MPCDesignationException("Cycle count out of range (0-619): $count");
        }

        $tens = intdiv($count, 10);
        $ones = $count % 10;

        if ($tens < 10) {
            $first = chr(ord('0') + $tens);
        } elseif ($tens < 36) {
            $first = chr(ord('A') + $tens - 10);
        } else {
            $first = chr(ord('a') + $tens - 36);
        }

        return $first . chr(ord('0') + $ones);
    }

    // =========================================================================
    // Letter/position utilities for provisional designations
    // =========================================================================

    private static function letterToPosition(string $letter): int {
        if ($letter < 'A' || $letter > 'Z') {
            throw new MPCDesignationException("Invalid half-month letter: $letter");
        }
        $pos = ord($letter) - ord('A') + 1;
        if ($letter > 'I') {
            $pos--;
        }
        return $pos;
    }

    private static function positionToLetter(int $pos): string {
        if ($pos < 1 || $pos > 25) {
            throw new MPCDesignationException("Invalid letter position: $pos");
        }
        $p = $pos;
        if ($p >= 9) {
            $p++;
        }
        return chr(ord('A') + $p - 1);
    }

    // =========================================================================
    // Permanent (numbered) asteroid designations
    // =========================================================================

    public static function unpackPermanent(string $packed): int {
        $p = trim($packed);
        if (strlen($p) !== 5) {
            throw new MPCDesignationException("Invalid packed permanent designation length");
        }

        $first = $p[0];

        // Tilde format (>= 620,000)
        if ($first === '~') {
            return 620000 + self::base62StringToNum(substr($p, 1, 4));
        }

        // Simple numeric format (< 100,000)
        if ($first >= '0' && $first <= '9') {
            return (int)$p;
        }

        // Extended format with uppercase letter (100,000 - 359,999)
        if ($first >= 'A' && $first <= 'Z') {
            $val = ord($first) - 55;
            $rest = (int)substr($p, 1, 4);
            return $val * 10000 + $rest;
        }

        // Extended format with lowercase letter (360,000 - 619,999)
        if ($first >= 'a' && $first <= 'z') {
            $val = ord($first) - 61;
            $rest = (int)substr($p, 1, 4);
            return $val * 10000 + $rest;
        }

        throw new MPCDesignationException("Invalid packed permanent designation");
    }

    public static function packPermanent(int $number): string {
        if ($number < 1 || $number > self::MAX_ASTEROID_NUMBER) {
            throw new MPCDesignationException("Invalid asteroid number: $number");
        }

        if ($number < 100000) {
            return str_pad((string)$number, 5, '0', STR_PAD_LEFT);
        }

        if ($number < 620000) {
            $div = intdiv($number, 10000);
            $mod = $number % 10000;
            if ($div < 36) {
                $letter = chr($div + 55);
            } else {
                $letter = chr($div + 61);
            }
            return $letter . str_pad((string)$mod, 4, '0', STR_PAD_LEFT);
        }

        // Tilde + base-62 format
        $offset = $number - 620000;
        return '~' . self::numToBase62String($offset, 4);
    }

    // =========================================================================
    // Standard provisional asteroid designations
    // =========================================================================

    public static function unpackProvisional(string $packed): string {
        $p = trim($packed);

        // Check for survey designations first
        if (strlen($p) === 7 && isset(self::SURVEY_PACKED_TO_UNPACKED[substr($p, 0, 3)])) {
            $survey = self::SURVEY_PACKED_TO_UNPACKED[substr($p, 0, 3)];
            $num = (int)substr($p, 3, 4);
            return "$num $survey";
        }

        if (strlen($p) !== 7) {
            throw new MPCDesignationException("Invalid packed provisional designation length");
        }

        $century = $p[0];
        $year = substr($p, 1, 2);
        $halfMonth = $p[3];
        $orderEncoded = substr($p, 4, 2);
        $secondLetter = $p[6];

        if (!isset(self::CENTURY_CODES[$century])) {
            throw new MPCDesignationException("Invalid century code: $century");
        }

        // For asteroid provisionals, only I-L (1800-2199) are valid century codes
        if ($century !== 'I' && $century !== 'J' && $century !== 'K' && $century !== 'L') {
            throw new MPCDesignationException(
                "Invalid century code for asteroid provisional: $century (must be I-L for years 1800-2199)"
            );
        }

        $fullYear = self::CENTURY_CODES[$century] . $year;
        $orderNum = self::decodeCycleCount($orderEncoded);

        // Pre-1925 designations use A-prefix format (e.g., "A908 CJ" for J08C00J)
        $fullYearInt = (int)$fullYear;
        if ($fullYearInt < 1925) {
            // Determine prefix: A for 1xxx, B for 2xxx (would be B0xx etc, but 2xxx years < 1925 don't exist)
            $prefix = $fullYearInt < 2000 ? 'A' : 'B';
            $shortYear = substr($fullYear, 1, 3); // Last 3 digits of year
            if ($orderNum === 0) {
                return "$prefix$shortYear $halfMonth$secondLetter";
            }
            return "$prefix$shortYear $halfMonth$secondLetter$orderNum";
        }

        if ($orderNum === 0) {
            return "$fullYear $halfMonth$secondLetter";
        }
        return "$fullYear $halfMonth$secondLetter$orderNum";
    }

    public static function packProvisional(string $unpacked): string {
        $u = trim($unpacked);

        // Check for survey designations
        if (preg_match('/^(\d+) (P-L|T-[123])$/', $u, $matches)) {
            $number = (int)$matches[1];
            $survey = $matches[2];
            if ($number < 1) {
                throw new MPCDesignationException("Survey number must be positive");
            }
            $surveyUnpackedToPacked = array_flip(self::SURVEY_PACKED_TO_UNPACKED);
            return $surveyUnpackedToPacked[$survey] . str_pad((string)$number, 4, '0', STR_PAD_LEFT);
        }

        // Check for old-style designation: "A908 CJ" or "B842 FA"
        if (preg_match('/^[AB](\d)(\d{2}) ([A-Z])([A-Z])$/', $u, $matches)) {
            $centuryDigit = $matches[1];
            $yearShort = $matches[2];
            $halfMonth = $matches[3];
            $secondLetter = $matches[4];

            $centuryCode = match($centuryDigit) {
                '8' => 'I',
                '9' => 'J',
                '0' => 'K',
                default => throw new MPCDesignationException("Invalid century digit in old-style designation")
            };

            return "$centuryCode{$yearShort}{$halfMonth}00$secondLetter";
        }

        // Match standard provisional: "1995 XA" or "1995 XA12"
        if (!preg_match('/^(\d{4}) ([A-Z])([A-Z])(\d*)$/', $u, $matches)) {
            throw new MPCDesignationException("Invalid unpacked provisional designation: $u");
        }

        $year = $matches[1];
        $halfMonth = $matches[2];
        $secondLetter = $matches[3];
        $orderStr = $matches[4];

        if (!self::isValidHalfMonth($halfMonth)) {
            throw new MPCDesignationException("Invalid half-month letter: $halfMonth");
        }

        $century = (int)substr($year, 0, 2);
        $yearShort = substr($year, 2, 2);

        $reverseCenturyCodes = array_flip(self::CENTURY_CODES);
        if (!isset($reverseCenturyCodes[$century])) {
            throw new MPCDesignationException("Invalid century in year: $year");
        }

        $centuryCode = $reverseCenturyCodes[$century];

        // Validate year range for asteroid provisionals (1800-2199)
        $yearInt = (int)$year;
        if ($yearInt < 1800 || $yearInt > 2199) {
            throw new MPCDesignationException(
                "Year out of range for asteroid provisional: $year (must be 1800-2199)"
            );
        }

        if ($orderStr === '') {
            $orderNum = 0;
        } else {
            if (!is_numeric($orderStr) || bccomp($orderStr, (string)PHP_INT_MAX) > 0) {
                throw new MPCDesignationException("Cycle count out of range (overflow): $orderStr");
            }
            $orderNum = (int)$orderStr;
        }

        // Check if we need extended format
        if ($orderNum >= 620) {
            return self::packExtendedProvisional((int)$year, $halfMonth, $secondLetter, $orderNum);
        }

        $orderEncoded = self::encodeCycleCount($orderNum);
        return "$centuryCode$yearShort$halfMonth$orderEncoded$secondLetter";
    }

    // =========================================================================
    // Extended provisional format (cycle >= 620)
    // =========================================================================

    private static function packExtendedProvisional(int $year, string $halfMonth, string $secondLetter, int $cycle): string {
        $yearShort = $year % 100;
        $letterPos = self::letterToPosition($secondLetter);
        $baseSequence = ($cycle - 620) * 25 + $letterPos - 1;
        $seqEncoded = self::numToBase62String($baseSequence, 4);
        $yearChar = self::numToBase62($yearShort);
        return "_$yearChar$halfMonth$seqEncoded";
    }

    public static function unpackExtendedProvisional(string $packed): string {
        $p = trim($packed);
        if (strlen($p) !== 7 || $p[0] !== '_') {
            throw new MPCDesignationException("Invalid extended packed provisional");
        }

        $yearDigit = $p[1];
        $halfMonth = $p[2];
        $seqEncoded = substr($p, 3, 4);

        $baseSequence = self::base62StringToNum($seqEncoded);
        $cycle = 620 + intdiv($baseSequence, 25);
        $letterPos = ($baseSequence % 25) + 1;
        $secondLetter = self::positionToLetter($letterPos);

        $yearVal = self::base62ToNum($yearDigit);
        $year = 2000 + $yearVal;

        return "$year $halfMonth$secondLetter$cycle";
    }

    // =========================================================================
    // Comet provisional designations
    // =========================================================================

    public static function unpackCometProvisional(string $packed): string {
        $p = trim($packed);
        $len = strlen($p);

        if ($len !== 7 && $len !== 8) {
            throw new MPCDesignationException("Invalid packed comet provisional designation length");
        }

        $century = $p[0];
        $year = substr($p, 1, 2);
        $halfMonth = $p[3];
        $orderEncoded = substr($p, 4, 2);
        $fragment = $len === 7 ? $p[6] : substr($p, 6, 2);

        if (!isset(self::CENTURY_CODES[$century])) {
            throw new MPCDesignationException("Invalid century code: $century");
        }

        $fullYear = self::CENTURY_CODES[$century] . $year;
        $orderNum = self::decodeCycleCount($orderEncoded);

        $result = "$fullYear $halfMonth$orderNum";
        if ($fragment !== '0') {
            $result .= '-' . strtoupper($fragment);
        }

        return $result;
    }

    public static function packCometProvisional(string $unpacked): string {
        $u = trim($unpacked);

        if (!preg_match('/^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$/', $u, $matches)) {
            throw new MPCDesignationException("Invalid unpacked comet provisional designation: $u");
        }

        $year = $matches[1];
        $halfMonth = $matches[2];
        $orderStr = $matches[3];
        $fragment = $matches[4] ?? null;

        if (!is_numeric($orderStr) || bccomp($orderStr, (string)PHP_INT_MAX) > 0) {
            throw new MPCDesignationException("Comet order number out of range (overflow): $orderStr");
        }
        $orderNum = (int)$orderStr;

        if ($orderNum < 1) {
            throw new MPCDesignationException("Comet order number must be positive");
        }

        $century = (int)substr($year, 0, 2);
        $yearShort = substr($year, 2, 2);

        $reverseCenturyCodes = array_flip(self::CENTURY_CODES);
        if (!isset($reverseCenturyCodes[$century])) {
            throw new MPCDesignationException("Invalid century in year: $year");
        }

        $centuryCode = $reverseCenturyCodes[$century];
        $orderEncoded = self::encodeCycleCount($orderNum);
        $fragmentCode = $fragment === null ? '0' : strtolower($fragment);

        return "$centuryCode$yearShort$halfMonth$orderEncoded$fragmentCode";
    }

    // =========================================================================
    // Numbered comet designations
    // =========================================================================

    public static function unpackCometNumbered(string $packed): string {
        $p = trim($packed);

        if (!preg_match('/^(\d{4})([PD])([a-z]{1,2})?$/', $p, $matches)) {
            throw new MPCDesignationException("Invalid packed numbered comet designation");
        }

        $number = (int)$matches[1];
        $cometType = $matches[2];
        $fragment = $matches[3] ?? '';

        if ($fragment !== '') {
            return "$number$cometType-" . strtoupper($fragment);
        }
        return "$number$cometType";
    }

    public static function packCometNumbered(string $unpacked): string {
        $u = trim($unpacked);

        if (!preg_match('/^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:\/[A-Za-z].*)?$/', $u, $matches)) {
            throw new MPCDesignationException("Invalid unpacked numbered comet designation");
        }

        $number = (int)$matches[1];
        $cometType = $matches[2];
        $fragment = $matches[3] ?? '';

        if ($number < 1 || $number > 9999) {
            throw new MPCDesignationException("Comet number out of range (1-9999): $number");
        }

        $result = str_pad((string)$number, 4, '0', STR_PAD_LEFT) . $cometType;
        if ($fragment !== '') {
            $result .= strtolower($fragment);
        }
        return $result;
    }

    // =========================================================================
    // Natural satellite designations
    // =========================================================================

    public static function unpackSatellite(string $packed): string {
        $p = trim($packed);

        if (strlen($p) !== 8 || $p[0] !== 'S') {
            throw new MPCDesignationException("Invalid packed satellite designation");
        }

        $century = $p[1];
        $year = substr($p, 2, 2);
        $planet = $p[4];
        $numberEncoded = substr($p, 5, 2);

        if (!isset(self::CENTURY_CODES[$century])) {
            throw new MPCDesignationException("Invalid century code: $century");
        }

        if (!in_array($planet, self::SATELLITE_PLANETS)) {
            throw new MPCDesignationException("Invalid planet code: $planet");
        }

        $fullYear = self::CENTURY_CODES[$century] . $year;
        $number = self::decodeCycleCount($numberEncoded);

        return "S/$fullYear $planet $number";
    }

    public static function packSatellite(string $unpacked): string {
        $u = trim($unpacked);

        if (!preg_match('/^S\/(\d{4}) ([JSUN]) (\d+)$/', $u, $matches)) {
            throw new MPCDesignationException("Invalid unpacked satellite designation");
        }

        $year = $matches[1];
        $planet = $matches[2];
        $number = (int)$matches[3];

        if ($number < 1) {
            throw new MPCDesignationException("Satellite number must be positive");
        }

        $century = (int)substr($year, 0, 2);
        $yearShort = substr($year, 2, 2);

        $reverseCenturyCodes = array_flip(self::CENTURY_CODES);
        if (!isset($reverseCenturyCodes[$century])) {
            throw new MPCDesignationException("Invalid century in year: $year");
        }

        $centuryCode = $reverseCenturyCodes[$century];
        $numberEncoded = self::encodeCycleCount($number);

        return "S$centuryCode$yearShort$planet{$numberEncoded}0";
    }

    // =========================================================================
    // BCE year encoding for ancient comets
    // =========================================================================

    private static function encodeBCEYear(int $year): array {
        if ($year >= 0) {
            throw new MPCDesignationException("Not a BCE year: $year");
        }

        $absYear = abs($year);
        $code = 99 - ($absYear % 100);

        if ($absYear < 100) {
            $prefix = '/';
        } elseif ($absYear < 200) {
            $prefix = '.';
        } elseif ($absYear < 300) {
            $prefix = '-';
        } else {
            throw new MPCDesignationException("BCE year out of supported range: $year");
        }

        return [$prefix, str_pad((string)$code, 2, '0', STR_PAD_LEFT)];
    }

    private static function decodeBCEYear(string $prefix, string $code): int {
        $codeNum = (int)$code;
        $yearPart = 99 - $codeNum;

        return match($prefix) {
            '/' => -$yearPart,
            '.' => -($yearPart + 100),
            '-' => -($yearPart + 200),
            default => throw new MPCDesignationException("Invalid BCE prefix: $prefix")
        };
    }

    // =========================================================================
    // Ancient/BCE comet provisional designations
    // =========================================================================

    private static function packAncientCometProvisional(string $cometType, int $year, string $halfMonth, int $orderNum, string $fragment): string {
        $orderEncoded = self::encodeCycleCount($orderNum);
        $fragmentCode = $fragment === '' ? '0' : strtolower($fragment);

        if ($year < 0) {
            [$prefix, $code] = self::encodeBCEYear($year);
            return "$cometType$prefix$code$halfMonth$orderEncoded$fragmentCode";
        }

        return $cometType . str_pad((string)$year, 3, '0', STR_PAD_LEFT) . "$halfMonth$orderEncoded$fragmentCode";
    }

    public static function unpackAncientCometProvisional(string $packed): string {
        $p = trim($packed);

        if (strlen($p) !== 8) {
            throw new MPCDesignationException("Invalid ancient comet designation length");
        }

        $cometType = $p[0];
        if (!in_array($cometType, self::COMET_TYPES)) {
            throw new MPCDesignationException("Invalid comet type: $cometType");
        }

        if (in_array($p[1], ['/', '.', '-'])) {
            $year = self::decodeBCEYear($p[1], substr($p, 2, 2));
            $halfMonth = $p[4];
            $orderEncoded = substr($p, 5, 2);
            $fragment = $p[7];
        } else {
            $year = (int)substr($p, 1, 3);
            $halfMonth = $p[4];
            $orderEncoded = substr($p, 5, 2);
            $fragment = $p[7];
        }

        $orderNum = self::decodeCycleCount($orderEncoded);
        $result = "$cometType/$year $halfMonth$orderNum";

        if ($fragment !== '0') {
            $result .= '-' . strtoupper($fragment);
        }

        return $result;
    }

    // =========================================================================
    // Helper functions for comet format detection
    // =========================================================================

    private static function isAsteroidStylePacked(string $provisionalPart): bool {
        if (strlen($provisionalPart) !== 7) {
            return false;
        }
        $lastChar = $provisionalPart[6];
        return $lastChar >= 'A' && $lastChar <= 'Z';
    }

    private static function isAsteroidStyleUnpacked(string $provisional): bool {
        if (preg_match('/^\d{4} ([A-Z])(.)/', $provisional, $matches)) {
            $secondChar = $matches[2];
            return ctype_alpha($secondChar);
        }
        return false;
    }

    // =========================================================================
    // Full comet designations (with type prefix)
    // =========================================================================

    public static function unpackCometFull(string $packed): string {
        $p = $packed;
        $len = strlen($p);

        if ($len === 8) {
            $cometType = $p[0];
            $provisionalPart = substr($p, 1, 7);

            if (!in_array($cometType, self::COMET_TYPES)) {
                throw new MPCDesignationException("Invalid comet type: $cometType");
            }

            $provisional = self::isAsteroidStylePacked($provisionalPart)
                ? self::unpackProvisional($provisionalPart)
                : self::unpackCometProvisional($provisionalPart);

            return "$cometType/$provisional";
        }

        if ($len === 9) {
            $cometType = $p[0];
            $provisionalPart = substr($p, 1, 8);

            if (!in_array($cometType, self::COMET_TYPES)) {
                throw new MPCDesignationException("Invalid comet type: $cometType");
            }

            $provisional = self::unpackCometProvisional($provisionalPart);
            return "$cometType/$provisional";
        }

        if ($len === 12 || ($len < 12 && $p[0] === ' ')) {
            while (strlen($p) < 12) {
                $p = ' ' . $p;
            }

            $numPart = trim(substr($p, 0, 4));
            $cometType = $p[4];
            $provisionalPart = substr($p, 5, 7);

            if (!in_array($cometType, self::COMET_TYPES)) {
                throw new MPCDesignationException("Invalid comet type: $cometType");
            }

            $provisional = self::isAsteroidStylePacked($provisionalPart)
                ? self::unpackProvisional($provisionalPart)
                : self::unpackCometProvisional($provisionalPart);

            if ($numPart === '') {
                return "$cometType/$provisional";
            }

            $num = (int)$numPart;
            return "$num$cometType/$provisional";
        }

        throw new MPCDesignationException("Invalid packed full comet designation length");
    }

    public static function packCometFull(string $unpacked): string {
        $u = trim($unpacked);

        if (!preg_match('/^(\d*)([PCDXAI])\/(-?\d+) (.+)$/', $u, $matches)) {
            throw new MPCDesignationException("Invalid unpacked comet designation: $u");
        }

        $numberStr = $matches[1];
        $cometType = $matches[2];
        $year = (int)$matches[3];
        $provPart = $matches[4];

        // Check for ancient or BCE year
        if ($year < 1000) {
            if (preg_match('/^([A-Z])(\d+)(?:-([A-Z]))?$/', $provPart, $ancientMatches)) {
                $halfMonth = $ancientMatches[1];
                $orderNum = (int)$ancientMatches[2];
                $fragment = $ancientMatches[3] ?? '';
                return self::packAncientCometProvisional($cometType, $year, $halfMonth, $orderNum, $fragment);
            } else {
                throw new MPCDesignationException("Invalid ancient comet provisional: $provPart");
            }
        }

        // Modern comet
        $provisional = "$year $provPart";

        $provisionalPacked = self::isAsteroidStyleUnpacked($provisional)
            ? self::packProvisional($provisional)
            : self::packCometProvisional($provisional);

        if ($numberStr === '') {
            return "$cometType$provisionalPacked";
        }

        $num = (int)$numberStr;
        if ($num < 1 || $num > 9999) {
            throw new MPCDesignationException("Comet number out of range (1-9999): $num");
        }

        return str_pad((string)$num, 4, '0', STR_PAD_LEFT) . "$cometType$provisionalPacked";
    }

    // =========================================================================
    // Format detection
    // =========================================================================

    private static function isAllDigits(string $s): bool {
        return $s !== '' && ctype_digit($s);
    }

    public static function detectFormat(string $designation): FormatInfo {
        // Validate raw input BEFORE trimming
        self::validateRawInput($designation);

        // Check for packed full comet designation BEFORE trimming (12 chars with spaces)
        if (strlen($designation) === 12) {
            if (preg_match('/^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$/', $designation)) {
                return new FormatInfo('packed', 'comet_full', 'comet with provisional designation (12-char)');
            }
        }

        // Check for packed comet designation (8 chars)
        if (strlen($designation) === 8 && in_array($designation[0], self::COMET_TYPES)) {
            if (preg_match('/^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$/', $designation)) {
                return new FormatInfo('packed', 'comet_full', 'comet with provisional designation (8-char)');
            }
        }

        // Check for packed comet with 2-letter fragment (9 chars)
        if (strlen($designation) === 9 && in_array($designation[0], self::COMET_TYPES)) {
            if (preg_match('/^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$/', $designation)) {
                return new FormatInfo('packed', 'comet_full', 'comet with provisional designation (9-char, 2-letter fragment)');
            }
        }

        // Check for packed ancient comet (8 chars)
        if (strlen($designation) === 8 && in_array($designation[0], self::COMET_TYPES)) {
            if (preg_match('/^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/', $designation)) {
                return new FormatInfo('packed', 'comet_ancient', 'comet with ancient provisional (year < 1000)');
            }
        }

        // Check for packed BCE comet (8 chars)
        if (strlen($designation) === 8 && in_array($designation[0], self::COMET_TYPES)) {
            if (preg_match('/^([PCDXAI])([\/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/', $designation)) {
                return new FormatInfo('packed', 'comet_bce', 'comet with BCE provisional');
            }
        }

        $des = trim($designation);

        // Validate whitespace
        self::validateWhitespace($des);

        // Check for packed satellite designation (8 chars starting with S)
        if (strlen($des) === 8 && $des[0] === 'S') {
            if (preg_match('/^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$/', $des)) {
                $planet = $des[4];
                $planetName = self::SATELLITE_PLANET_NAMES[$planet] ?? $planet;
                return new FormatInfo('packed', 'satellite', "natural satellite ($planetName)");
            }
        }

        // Check for packed permanent (numbered) asteroid
        if (strlen($des) === 5) {
            if ($des[0] === '~') {
                if (preg_match('/^~[0-9A-Za-z]{4}$/', $des)) {
                    return new FormatInfo('packed', 'permanent', 'permanent numbered (tilde/base-62, >= 620000)');
                }
            } elseif (self::isAllDigits($des)) {
                return new FormatInfo('packed', 'permanent', 'permanent numbered (5-digit, < 100000)');
            } elseif (preg_match('/^[A-Za-z][0-9]{4}$/', $des)) {
                $subtype = ctype_upper($des[0])
                    ? 'permanent numbered (letter-prefix, 100000-359999)'
                    : 'permanent numbered (letter-prefix, 360000-619999)';
                return new FormatInfo('packed', 'permanent', $subtype);
            }

            // Check for packed numbered comet
            if (preg_match('/^[0-9]{4}[PD]$/', $des)) {
                $cometType = $des[4];
                $typeDesc = self::COMET_TYPE_DESCRIPTIONS[$cometType] ?? $cometType;
                return new FormatInfo('packed', 'comet_numbered', "comet numbered $typeDesc");
            }
        }

        // Check for packed numbered comet with fragment (6 or 7 chars: 0073Pa or 0073Paa)
        if (strlen($des) === 6 || strlen($des) === 7) {
            if (preg_match('/^[0-9]{4}[PD][a-z]{1,2}$/', $des)) {
                $cometType = $des[4];
                $typeDesc = self::COMET_TYPE_DESCRIPTIONS[$cometType] ?? $cometType;
                return new FormatInfo('packed', 'comet_numbered', "comet numbered $typeDesc with fragment");
            }
        }

        // Check for packed provisional asteroid (7 chars)
        if (strlen($des) === 7) {
            if ($des[0] === '_') {
                if (preg_match('/^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$/', $des)) {
                    return new FormatInfo('packed', 'provisional_extended', 'provisional (extended format, cycle >=620)');
                }
            }

            if (preg_match('/^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$/', $des)) {
                return new FormatInfo('packed', 'provisional', 'provisional');
            }

            if (str_starts_with($des, 'PLS') && self::isAllDigits(substr($des, 3))) {
                return new FormatInfo('packed', 'survey', 'survey (Palomar-Leiden)');
            }

            if (preg_match('/^T[123]S\d{4}$/', $des)) {
                return new FormatInfo('packed', 'survey', "survey (Trojan T-{$des[1]})");
            }

            if (preg_match('/^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$/', $des)) {
                return new FormatInfo('packed', 'comet_provisional', 'comet provisional');
            }
        }

        // --- UNPACKED FORMATS ---

        // Check for unpacked satellite
        if (preg_match('/^S\/\d{4} ([JSUN]) \d+$/', $des, $matches)) {
            $planet = $matches[1];
            $planetName = self::SATELLITE_PLANET_NAMES[$planet] ?? $planet;
            return new FormatInfo('unpacked', 'satellite', "natural satellite ($planetName)");
        }

        // Check for unpacked permanent (numbered) asteroid
        if (self::isAllDigits($des)) {
            return new FormatInfo('unpacked', 'permanent', 'permanent numbered');
        }

        // Check for unpacked survey designation
        if (preg_match('/^\d+ (P-L|T-[123])$/', $des, $matches)) {
            $survey = $matches[1];
            $subtype = $survey === 'P-L' ? 'survey (Palomar-Leiden)' : "survey (Trojan $survey)";
            return new FormatInfo('unpacked', 'survey', $subtype);
        }

        // Check for old-style asteroid designation
        if (preg_match('/^[AB]\d{3} [A-Z][A-Z]$/', $des)) {
            return new FormatInfo('unpacked', 'provisional', 'provisional (old-style pre-1925)');
        }

        // Check for unpacked provisional asteroid
        if (preg_match('/^\d{4} [A-Z][A-Z]\d*$/', $des)) {
            return new FormatInfo('unpacked', 'provisional', 'provisional');
        }

        // Check for unpacked comet with type prefix
        if (preg_match('/^(\d*)([PCDXAI])\/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$/', $des, $matches)) {
            $num = $matches[1];
            $ctype = $matches[2];
            $year = (int)$matches[3];

            $yearDesc = '';
            if ($year < 0) {
                $yearDesc = 'BCE';
            } elseif ($year < 1000) {
                $yearDesc = 'ancient';
            }

            $typeDesc = self::COMET_TYPE_DESCRIPTIONS[$ctype] ?? $ctype;

            if ($num !== '') {
                $subtype = $yearDesc !== ''
                    ? "comet numbered with $yearDesc provisional ($typeDesc)"
                    : "comet numbered with provisional ($typeDesc)";
            } else {
                $subtype = $yearDesc !== ''
                    ? "comet $yearDesc provisional ($typeDesc)"
                    : "comet provisional ($typeDesc)";
            }

            return new FormatInfo('unpacked', 'comet_full', $subtype);
        }

        // Check for unpacked numbered periodic comet (with optional fragment)
        if (preg_match('/^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:\/[A-Za-z].*)?$/', $des, $matches)) {
            $cometType = $matches[2];
            $fragment = $matches[3] ?? '';
            $typeDesc = self::COMET_TYPE_DESCRIPTIONS[$cometType] ?? $cometType;
            $subtype = $fragment !== ''
                ? "comet numbered $typeDesc with fragment"
                : "comet numbered $typeDesc";
            return new FormatInfo('unpacked', 'comet_numbered', $subtype);
        }

        throw new MPCDesignationException("Unable to detect designation format: $designation");
    }

    // =========================================================================
    // Main conversion functions
    // =========================================================================

    public static function convert(string $designation): ConversionResult {
        $info = self::detectFormat($designation);

        if ($info->format === 'packed') {
            $output = match($info->type) {
                'permanent' => (string)self::unpackPermanent($designation),
                'provisional', 'survey' => self::unpackProvisional($designation),
                'provisional_extended' => self::unpackExtendedProvisional($designation),
                'comet_numbered' => self::unpackCometNumbered($designation),
                'comet_provisional' => self::unpackCometProvisional($designation),
                'comet_full' => self::unpackCometFull($designation),
                'comet_ancient', 'comet_bce' => self::unpackAncientCometProvisional($designation),
                'satellite' => self::unpackSatellite($designation),
                default => throw new MPCDesignationException("Unknown type: {$info->type}")
            };
        } else {
            $output = match($info->type) {
                'permanent' => self::packPermanentFromString(trim($designation)),
                'provisional', 'survey' => self::packProvisional($designation),
                'comet_numbered' => self::packCometNumbered($designation),
                'comet_full' => self::packCometFull($designation),
                'satellite' => self::packSatellite($designation),
                default => throw new MPCDesignationException("Unknown type: {$info->type}")
            };
        }

        return new ConversionResult($designation, $output, $info);
    }

    private static function packPermanentFromString(string $s): string {
        if (!ctype_digit($s)) {
            throw new MPCDesignationException("Invalid asteroid number: $s");
        }
        if (bccomp($s, (string)self::MAX_ASTEROID_NUMBER) > 0) {
            throw new MPCDesignationException("Invalid asteroid number: $s");
        }
        $num = (int)$s;
        if ($num < 1) {
            throw new MPCDesignationException("Invalid asteroid number: $num");
        }
        return self::packPermanent($num);
    }

    public static function convertSimple(string $designation): string {
        return self::convert($designation)->output;
    }

    public static function pack(string $designation): string {
        $info = self::detectFormat($designation);
        if ($info->format === 'packed') {
            return trim($designation);
        }
        return self::convert($designation)->output;
    }

    public static function unpack(string $designation): string {
        $info = self::detectFormat($designation);
        if ($info->format === 'unpacked') {
            return trim($designation);
        }
        return self::convert($designation)->output;
    }

    public static function isValidDesignation(string $designation): bool {
        if ($designation === '') {
            return false;
        }
        try {
            self::detectFormat($designation);
            return true;
        } catch (MPCDesignationException $e) {
            return false;
        }
    }

    // =========================================================================
    // Helper functions for report format and fragment handling
    // =========================================================================

    /**
     * Convert a packed (minimal) designation to 12-character MPC report format.
     * Pads asteroids/provisionals on the left, comets on the right.
     */
    public static function toReportFormat(string $designation): string {
        $packed = self::pack($designation);
        $info = self::detectFormat($packed);

        // Numbered comets: right-pad to 12 chars (format: "0073P       ")
        // Handle fragments: "0073Pa" -> "0073P      a", "0073Paa" -> "0073P     aa"
        if ($info->type === 'comet_numbered') {
            if (preg_match('/^(\d{4}[PD])([a-z]{1,2})?$/', $packed, $matches)) {
                $basePart = $matches[1];  // "0073P"
                $fragment = $matches[2] ?? '';
                if ($fragment !== '') {
                    // Fragment goes at the end, base part stays at beginning
                    $padding = 12 - strlen($basePart) - strlen($fragment);
                    return $basePart . str_repeat(' ', $padding) . $fragment;
                }
                return str_pad($basePart, 12);
            }
        }

        // Everything else (asteroids, provisionals, surveys): left-pad to 12 chars
        return str_pad($packed, 12, ' ', STR_PAD_LEFT);
    }

    /**
     * Convert a 12-character MPC report format to minimal packed format.
     * Strips padding from asteroids/provisionals and comets.
     */
    public static function fromReportFormat(string $reportFormat): string {
        if (strlen($reportFormat) !== 12) {
            throw new MPCDesignationException("Report format must be exactly 12 characters");
        }

        // Check for numbered comet format (starts with 4 digits + P/D)
        if (preg_match('/^(\d{4}[PD])(\s*)([a-z]{0,2})$/', $reportFormat, $matches)) {
            $basePart = $matches[1];
            $fragment = $matches[3];
            return $basePart . $fragment;
        }

        // For everything else, just trim whitespace
        return trim($reportFormat);
    }

    /**
     * Check if a comet designation has a fragment suffix.
     * Works with both packed and unpacked formats.
     */
    public static function hasFragment(string $designation): bool {
        $desig = trim($designation);

        // Unpacked format with fragment: "73P-A", "73P-AA", "D/1993 F2-A"
        if (preg_match('/-[A-Z]{1,2}$/', $desig)) {
            return true;
        }

        // Packed numbered comet with fragment: "0073Pa", "0073Paa"
        if (preg_match('/^\d{4}[PD][a-z]{1,2}$/', $desig)) {
            return true;
        }

        // Packed provisional comet with fragment: "DJ93F02a" (8 chars: type + provisional + fragment)
        if (preg_match('/^[PCDXAI][A-L]\d{2}[A-Z]\d{2}[a-z]$/', $desig)) {
            return true;
        }

        return false;
    }

    /**
     * Get the fragment letter(s) from a comet designation.
     * Returns uppercase letter(s) or empty string if no fragment.
     */
    public static function getFragment(string $designation): string {
        $desig = trim($designation);

        // Unpacked format: "73P-A" -> "A", "73P-AA" -> "AA"
        if (preg_match('/-([A-Z]{1,2})$/', $desig, $matches)) {
            return $matches[1];
        }

        // Packed numbered comet: "0073Pa" -> "A", "0073Paa" -> "AA"
        if (preg_match('/^\d{4}[PD]([a-z]{1,2})$/', $desig, $matches)) {
            return strtoupper($matches[1]);
        }

        // Packed provisional comet: "DJ93F02a" -> "A" (8 chars: type + provisional + fragment)
        if (preg_match('/^[PCDXAI][A-L]\d{2}[A-Z]\d{2}([a-z])$/', $desig, $matches)) {
            return strtoupper($matches[1]);
        }

        return '';
    }

    /**
     * Get the parent comet designation without fragment suffix.
     * For non-comets or comets without fragments, returns the input as-is.
     */
    public static function getParent(string $designation): string {
        $desig = trim($designation);

        // Unpacked format: "73P-A" -> "73P"
        if (preg_match('/^(.+)-[A-Z]{1,2}$/', $desig, $matches)) {
            return $matches[1];
        }

        // Packed numbered comet: "0073Pa" -> "0073P"
        if (preg_match('/^(\d{4}[PD])[a-z]{1,2}$/', $desig, $matches)) {
            return $matches[1];
        }

        // Packed provisional comet: "DJ93F02a" -> "DJ93F020"
        if (preg_match('/^([A-L]\d{2}[A-Z]\d{2})[a-z]$/', $desig, $matches)) {
            return $matches[1] . '0';
        }

        return $desig;
    }

    /**
     * Check if two designations refer to the same object.
     * Normalizes both designations to packed format and compares them.
     */
    public static function designationsEqual(string $desig1, string $desig2): bool {
        try {
            $packed1 = self::pack($desig1);
            $packed2 = self::pack($desig2);
            return $packed1 === $packed2;
        } catch (MPCDesignationException $e) {
            return false;
        }
    }
}
