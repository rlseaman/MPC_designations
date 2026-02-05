package mpc;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Convert between packed and unpacked Minor Planet Center (MPC) designations
 * for asteroids, comets, and natural satellites.
 *
 * Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
 */
public class MPCDesignation {

    public static final String VERSION = "1.0.0";

    // Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
    public static final int MAX_ASTEROID_NUMBER = 15396335;

    // Base-62 character set
    private static final String BASE62_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    // Century codes for provisional designations
    private static final Map<Character, Integer> CENTURY_CODES = new HashMap<>();
    private static final Map<Integer, Character> REVERSE_CENTURY_CODES = new HashMap<>();

    static {
        char[] letters = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L'};
        for (int i = 0; i < letters.length; i++) {
            CENTURY_CODES.put(letters[i], 10 + i);
            REVERSE_CENTURY_CODES.put(10 + i, letters[i]);
        }
    }

    // Survey codes
    private static final Map<String, String> SURVEY_PACKED_TO_UNPACKED = new HashMap<>();
    private static final Map<String, String> SURVEY_UNPACKED_TO_PACKED = new HashMap<>();

    static {
        SURVEY_PACKED_TO_UNPACKED.put("PLS", "P-L");
        SURVEY_PACKED_TO_UNPACKED.put("T1S", "T-1");
        SURVEY_PACKED_TO_UNPACKED.put("T2S", "T-2");
        SURVEY_PACKED_TO_UNPACKED.put("T3S", "T-3");
        for (Map.Entry<String, String> entry : SURVEY_PACKED_TO_UNPACKED.entrySet()) {
            SURVEY_UNPACKED_TO_PACKED.put(entry.getValue(), entry.getKey());
        }
    }

    // Comet types
    private static final String COMET_TYPES = "PCDXAI";
    private static final Map<Character, String> COMET_TYPE_DESCRIPTIONS = new HashMap<>();

    static {
        COMET_TYPE_DESCRIPTIONS.put('P', "periodic");
        COMET_TYPE_DESCRIPTIONS.put('C', "non-periodic");
        COMET_TYPE_DESCRIPTIONS.put('D', "defunct");
        COMET_TYPE_DESCRIPTIONS.put('X', "uncertain orbit");
        COMET_TYPE_DESCRIPTIONS.put('A', "asteroid with comet designation");
        COMET_TYPE_DESCRIPTIONS.put('I', "interstellar");
    }

    // Satellite planet codes
    private static final String SATELLITE_PLANETS = "JSUN";
    private static final Map<Character, String> SATELLITE_PLANET_NAMES = new HashMap<>();

    static {
        SATELLITE_PLANET_NAMES.put('J', "Jupiter");
        SATELLITE_PLANET_NAMES.put('S', "Saturn");
        SATELLITE_PLANET_NAMES.put('U', "Uranus");
        SATELLITE_PLANET_NAMES.put('N', "Neptune");
    }

    /**
     * Format type enumeration
     */
    public enum FormatType {
        PACKED("packed"),
        UNPACKED("unpacked");

        private final String name;

        FormatType(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    /**
     * Information about a detected designation format
     */
    public static class Info {
        public FormatType format;
        public String type;
        public String subtype;

        public Info() {
            this.format = null;
            this.type = "";
            this.subtype = "";
        }
    }

    /**
     * Conversion result containing input, output, and format information
     */
    public static class Result {
        public String input;
        public String output;
        public Info info;

        public Result(String input, String output, Info info) {
            this.input = input;
            this.output = output;
            this.info = info;
        }
    }

    /**
     * Exception for invalid MPC designations
     */
    public static class MPCDesignationException extends Exception {
        public MPCDesignationException(String message) {
            super(message);
        }
    }

    // =========================================================================
    // Input validation
    // =========================================================================

    private static void validateRawInput(String s) throws MPCDesignationException {
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < 32 || c > 126) {
                throw new MPCDesignationException(
                    String.format("Invalid character in designation: '\\x%02x'", (int) c));
            }
        }
    }

    private static void validateWhitespace(String s) throws MPCDesignationException {
        boolean prevSpace = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < 32 || c > 126) {
                throw new MPCDesignationException("Invalid character in designation");
            }
            if (c == ' ') {
                if (prevSpace) {
                    throw new MPCDesignationException("Consecutive spaces in designation");
                }
                prevSpace = true;
            } else {
                prevSpace = false;
            }
        }
    }

    private static String sanitize(String designation) throws MPCDesignationException {
        validateRawInput(designation);
        String result = designation.trim();
        if (result.isEmpty()) {
            throw new MPCDesignationException("Empty designation");
        }
        return result;
    }

    private static boolean isValidHalfMonth(char c) {
        return c >= 'A' && c <= 'Y' && c != 'I';
    }

    // =========================================================================
    // Base-62 encoding utilities
    // =========================================================================

    private static int base62ToNum(char c) throws MPCDesignationException {
        int idx = BASE62_CHARS.indexOf(c);
        if (idx < 0) {
            throw new MPCDesignationException("Invalid base-62 character: " + c);
        }
        return idx;
    }

    private static char numToBase62(int n) throws MPCDesignationException {
        if (n < 0 || n > 61) {
            throw new MPCDesignationException("Number out of base-62 range: " + n);
        }
        return BASE62_CHARS.charAt(n);
    }

    private static int base62StringToNum(String s) throws MPCDesignationException {
        int result = 0;
        for (int i = 0; i < s.length(); i++) {
            result = result * 62 + base62ToNum(s.charAt(i));
        }
        return result;
    }

    private static String numToBase62String(int n, int width) throws MPCDesignationException {
        char[] result = new char[width];
        for (int i = width - 1; i >= 0; i--) {
            result[i] = numToBase62(n % 62);
            n /= 62;
        }
        return new String(result);
    }

    // =========================================================================
    // Cycle count encoding for provisional designations
    // =========================================================================

    private static int decodeCycleCount(String encoded) throws MPCDesignationException {
        if (encoded.length() < 2) {
            throw new MPCDesignationException("Invalid cycle count encoding");
        }

        char first = encoded.charAt(0);
        char second = encoded.charAt(1);

        int tens;
        if (first >= '0' && first <= '9') {
            tens = first - '0';
        } else if (first >= 'A' && first <= 'Z') {
            tens = first - 'A' + 10;
        } else if (first >= 'a' && first <= 'z') {
            tens = first - 'a' + 36;
        } else {
            throw new MPCDesignationException("Invalid cycle count encoding");
        }

        if (second < '0' || second > '9') {
            throw new MPCDesignationException("Invalid cycle count encoding");
        }

        return tens * 10 + (second - '0');
    }

    private static String encodeCycleCount(int count) throws MPCDesignationException {
        if (count < 0 || count >= 620) {
            throw new MPCDesignationException("Cycle count out of range (0-619): " + count);
        }

        int tens = count / 10;
        int ones = count % 10;

        char first;
        if (tens < 10) {
            first = (char) ('0' + tens);
        } else if (tens < 36) {
            first = (char) ('A' + tens - 10);
        } else {
            first = (char) ('a' + tens - 36);
        }

        return "" + first + (char) ('0' + ones);
    }

    // =========================================================================
    // Letter/position utilities for provisional designations
    // =========================================================================

    private static int letterToPosition(char letter) throws MPCDesignationException {
        if (letter < 'A' || letter > 'Z') {
            throw new MPCDesignationException("Invalid half-month letter: " + letter);
        }
        int pos = letter - 'A' + 1;
        if (letter > 'I') {
            pos--; // Skip I
        }
        return pos;
    }

    private static char positionToLetter(int pos) throws MPCDesignationException {
        if (pos < 1 || pos > 25) {
            throw new MPCDesignationException("Invalid letter position: " + pos);
        }
        int p = pos;
        if (p >= 9) {
            p++; // Skip I
        }
        return (char) ('A' + p - 1);
    }

    // =========================================================================
    // Permanent (numbered) asteroid designations
    // =========================================================================

    public static int unpackPermanent(String packed) throws MPCDesignationException {
        String p = packed.trim();
        if (p.length() != 5) {
            throw new MPCDesignationException("Invalid packed permanent designation length");
        }

        char first = p.charAt(0);

        // Tilde format (>= 620,000)
        if (first == '~') {
            return 620000 + base62StringToNum(p.substring(1, 5));
        }

        // Simple numeric format (< 100,000)
        if (first >= '0' && first <= '9') {
            try {
                return Integer.parseInt(p);
            } catch (NumberFormatException e) {
                throw new MPCDesignationException("Invalid packed permanent designation");
            }
        }

        // Extended format with uppercase letter (100,000 - 359,999)
        if (first >= 'A' && first <= 'Z') {
            int val = first - 55; // A=10, B=11, etc.
            try {
                int rest = Integer.parseInt(p.substring(1, 5));
                return val * 10000 + rest;
            } catch (NumberFormatException e) {
                throw new MPCDesignationException("Invalid packed permanent designation");
            }
        }

        // Extended format with lowercase letter (360,000 - 619,999)
        if (first >= 'a' && first <= 'z') {
            int val = first - 61; // a=36, b=37, etc.
            try {
                int rest = Integer.parseInt(p.substring(1, 5));
                return val * 10000 + rest;
            } catch (NumberFormatException e) {
                throw new MPCDesignationException("Invalid packed permanent designation");
            }
        }

        throw new MPCDesignationException("Invalid packed permanent designation");
    }

    public static String packPermanent(int number) throws MPCDesignationException {
        if (number < 1 || number > MAX_ASTEROID_NUMBER) {
            throw new MPCDesignationException("Invalid asteroid number: " + number);
        }

        if (number < 100000) {
            return String.format("%05d", number);
        }

        if (number < 620000) {
            int div = number / 10000;
            int mod = number % 10000;
            char letter;
            if (div < 36) {
                letter = (char) (div + 55); // A-Z
            } else {
                letter = (char) (div + 61); // a-z
            }
            return String.format("%c%04d", letter, mod);
        }

        // Tilde + base-62 format
        int offset = number - 620000;
        return "~" + numToBase62String(offset, 4);
    }

    // =========================================================================
    // Standard provisional asteroid designations
    // =========================================================================

    public static String unpackProvisional(String packed) throws MPCDesignationException {
        String p = packed.trim();

        // Check for survey designations first
        if (p.length() == 7 && SURVEY_PACKED_TO_UNPACKED.containsKey(p.substring(0, 3))) {
            String survey = SURVEY_PACKED_TO_UNPACKED.get(p.substring(0, 3));
            try {
                int num = Integer.parseInt(p.substring(3, 7));
                return num + " " + survey;
            } catch (NumberFormatException e) {
                throw new MPCDesignationException("Invalid survey number");
            }
        }

        if (p.length() != 7) {
            throw new MPCDesignationException("Invalid packed provisional designation length");
        }

        char century = p.charAt(0);
        String year = p.substring(1, 3);
        char halfMonth = p.charAt(3);
        String orderEncoded = p.substring(4, 6);
        char secondLetter = p.charAt(6);

        if (!CENTURY_CODES.containsKey(century)) {
            throw new MPCDesignationException("Invalid century code: " + century);
        }

        // Asteroid provisionals: only I-L valid (1800-2199)
        if (century != 'I' && century != 'J' && century != 'K' && century != 'L') {
            throw new MPCDesignationException(
                "Invalid century code for asteroid provisional: " + century + " (must be I-L for years 1800-2199)");
        }

        String fullYear = CENTURY_CODES.get(century) + year;
        int orderNum = decodeCycleCount(orderEncoded);
        int yearNum = Integer.parseInt(fullYear);

        // For pre-1925 designations, use A-prefix format (MPC canonical)
        // A-prefix: A=1 for 1xxx years, B=2 for 2xxx years (theoretical)
        if (yearNum < 1925) {
            char firstDigit = fullYear.charAt(0);
            String restOfYear = fullYear.substring(1);
            String prefix = "";
            if (firstDigit == '1') {
                prefix = "A";
            } else if (firstDigit == '2') {
                prefix = "B";
            }
            if (!prefix.isEmpty()) {
                if (orderNum == 0) {
                    return prefix + restOfYear + " " + halfMonth + secondLetter;
                }
                return prefix + restOfYear + " " + halfMonth + secondLetter + orderNum;
            }
        }

        if (orderNum == 0) {
            return fullYear + " " + halfMonth + secondLetter;
        }
        return fullYear + " " + halfMonth + secondLetter + orderNum;
    }

    public static String packProvisional(String unpacked) throws MPCDesignationException {
        String u = unpacked.trim();

        // Check for survey designations
        Pattern surveyPattern = Pattern.compile("^(\\d+) (P-L|T-[123])$");
        Matcher surveyMatcher = surveyPattern.matcher(u);
        if (surveyMatcher.matches()) {
            int number = Integer.parseInt(surveyMatcher.group(1));
            String survey = surveyMatcher.group(2);
            if (number < 1) {
                throw new MPCDesignationException("Survey number must be positive");
            }
            return SURVEY_UNPACKED_TO_PACKED.get(survey) + String.format("%04d", number);
        }

        // Check for old-style designation: "A908 CJ" or "B842 FA"
        Pattern oldStylePattern = Pattern.compile("^[AB](\\d)(\\d{2}) ([A-Z])([A-Z])$");
        Matcher oldStyleMatcher = oldStylePattern.matcher(u);
        if (oldStyleMatcher.matches()) {
            char centuryDigit = oldStyleMatcher.group(1).charAt(0);
            String yearShort = oldStyleMatcher.group(2);
            char halfMonth = oldStyleMatcher.group(3).charAt(0);
            char secondLetter = oldStyleMatcher.group(4).charAt(0);

            char centuryCode;
            switch (centuryDigit) {
                case '8': centuryCode = 'I'; break;
                case '9': centuryCode = 'J'; break;
                case '0': centuryCode = 'K'; break;
                default:
                    throw new MPCDesignationException("Invalid century digit in old-style designation");
            }

            return "" + centuryCode + yearShort + halfMonth + "00" + secondLetter;
        }

        // Match standard provisional: "1995 XA" or "1995 XA12"
        Pattern provisionalPattern = Pattern.compile("^(\\d{4}) ([A-Z])([A-Z])(\\d*)$");
        Matcher provisionalMatcher = provisionalPattern.matcher(u);
        if (!provisionalMatcher.matches()) {
            throw new MPCDesignationException("Invalid unpacked provisional designation: " + u);
        }

        String year = provisionalMatcher.group(1);
        char halfMonth = provisionalMatcher.group(2).charAt(0);
        char secondLetter = provisionalMatcher.group(3).charAt(0);
        String orderStr = provisionalMatcher.group(4);

        if (!isValidHalfMonth(halfMonth)) {
            throw new MPCDesignationException("Invalid half-month letter: " + halfMonth);
        }

        int yearInt = Integer.parseInt(year);

        // Asteroid provisionals: only years 1800-2199 valid
        if (yearInt < 1800 || yearInt > 2199) {
            throw new MPCDesignationException(
                "Year out of range for asteroid provisional: " + year + " (must be 1800-2199)");
        }

        int century = Integer.parseInt(year.substring(0, 2));
        String yearShort = year.substring(2, 4);

        if (!REVERSE_CENTURY_CODES.containsKey(century)) {
            throw new MPCDesignationException("Invalid century in year: " + year);
        }

        char centuryCode = REVERSE_CENTURY_CODES.get(century);
        int orderNum;
        if (orderStr.isEmpty()) {
            orderNum = 0;
        } else {
            try {
                long orderLong = Long.parseLong(orderStr);
                if (orderLong > Integer.MAX_VALUE) {
                    throw new MPCDesignationException("Cycle count out of range (overflow): " + orderStr);
                }
                orderNum = (int) orderLong;
            } catch (NumberFormatException e) {
                throw new MPCDesignationException("Cycle count out of range (overflow): " + orderStr);
            }
        }

        // Check if we need extended format
        if (orderNum >= 620) {
            return packExtendedProvisional(Integer.parseInt(year), halfMonth, secondLetter, orderNum);
        }

        String orderEncoded = encodeCycleCount(orderNum);
        return "" + centuryCode + yearShort + halfMonth + orderEncoded + secondLetter;
    }

    // =========================================================================
    // Extended provisional format (cycle >= 620)
    // =========================================================================

    private static String packExtendedProvisional(int year, char halfMonth, char secondLetter, int cycle)
            throws MPCDesignationException {
        int yearShort = year % 100;
        int letterPos = letterToPosition(secondLetter);
        int baseSequence = (cycle - 620) * 25 + letterPos - 1;
        String seqEncoded = numToBase62String(baseSequence, 4);
        char yearChar = numToBase62(yearShort);
        return "_" + yearChar + halfMonth + seqEncoded;
    }

    public static String unpackExtendedProvisional(String packed) throws MPCDesignationException {
        String p = packed.trim();
        if (p.length() != 7 || p.charAt(0) != '_') {
            throw new MPCDesignationException("Invalid extended packed provisional");
        }

        char yearDigit = p.charAt(1);
        char halfMonth = p.charAt(2);
        String seqEncoded = p.substring(3, 7);

        int baseSequence = base62StringToNum(seqEncoded);
        int cycle = 620 + baseSequence / 25;
        int letterPos = (baseSequence % 25) + 1;
        char secondLetter = positionToLetter(letterPos);

        int yearVal = base62ToNum(yearDigit);
        int year = 2000 + yearVal;

        return year + " " + halfMonth + secondLetter + cycle;
    }

    // =========================================================================
    // Comet provisional designations
    // =========================================================================

    public static String unpackCometProvisional(String packed) throws MPCDesignationException {
        String p = packed.trim();
        int length = p.length();

        if (length != 7 && length != 8) {
            throw new MPCDesignationException("Invalid packed comet provisional designation length");
        }

        char century = p.charAt(0);
        String year = p.substring(1, 3);
        char halfMonth = p.charAt(3);
        String orderEncoded = p.substring(4, 6);
        String fragment = length == 7 ? String.valueOf(p.charAt(6)) : p.substring(6, 8);

        if (!CENTURY_CODES.containsKey(century)) {
            throw new MPCDesignationException("Invalid century code: " + century);
        }

        String fullYear = CENTURY_CODES.get(century) + year;
        int orderNum = decodeCycleCount(orderEncoded);

        String result = fullYear + " " + halfMonth + orderNum;
        if (!fragment.equals("0")) {
            result += "-" + fragment.toUpperCase();
        }

        return result;
    }

    public static String packCometProvisional(String unpacked) throws MPCDesignationException {
        String u = unpacked.trim();

        // Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
        Pattern pattern = Pattern.compile("^(\\d{4}) ([A-Z])(\\d+)(?:-([A-Z]{1,2}))?$");
        Matcher matcher = pattern.matcher(u);
        if (!matcher.matches()) {
            throw new MPCDesignationException("Invalid unpacked comet provisional designation: " + u);
        }

        String year = matcher.group(1);
        char halfMonth = matcher.group(2).charAt(0);
        String orderStr = matcher.group(3);
        String fragment = matcher.group(4);

        int orderNum;
        try {
            long orderLong = Long.parseLong(orderStr);
            if (orderLong > Integer.MAX_VALUE) {
                throw new MPCDesignationException("Comet order number out of range (overflow): " + orderStr);
            }
            orderNum = (int) orderLong;
        } catch (NumberFormatException e) {
            throw new MPCDesignationException("Comet order number out of range (overflow): " + orderStr);
        }

        if (orderNum < 1) {
            throw new MPCDesignationException("Comet order number must be positive");
        }

        int century = Integer.parseInt(year.substring(0, 2));
        String yearShort = year.substring(2, 4);

        if (!REVERSE_CENTURY_CODES.containsKey(century)) {
            throw new MPCDesignationException("Invalid century in year: " + year);
        }

        char centuryCode = REVERSE_CENTURY_CODES.get(century);
        String orderEncoded = encodeCycleCount(orderNum);
        String fragmentCode = fragment == null ? "0" : fragment.toLowerCase();

        return "" + centuryCode + yearShort + halfMonth + orderEncoded + fragmentCode;
    }

    // =========================================================================
    // Numbered comet designations
    // =========================================================================

    /**
     * Unpack a numbered comet designation.
     * Supports fragments: 0073Pa -> 73P-A, 0073Paa -> 73P-AA
     */
    public static String unpackCometNumbered(String packed) throws MPCDesignationException {
        String p = packed.trim();
        int length = p.length();

        // Match with optional lowercase fragment: 0073P, 0073Pa, or 0073Paa
        Pattern pattern = Pattern.compile("^(\\d{4})([PD])([a-z]{1,2})?$");
        Matcher matcher = pattern.matcher(p);
        if (!matcher.matches()) {
            throw new MPCDesignationException("Invalid packed numbered comet designation");
        }

        int number = Integer.parseInt(matcher.group(1));
        String cometType = matcher.group(2);
        String fragment = matcher.group(3);

        String result = number + cometType;
        if (fragment != null && !fragment.isEmpty()) {
            result += "-" + fragment.toUpperCase();
        }
        return result;
    }

    /**
     * Pack a numbered comet designation.
     * Supports fragments: 73P-A -> 0073Pa, 73P-AA -> 0073Paa
     */
    public static String packCometNumbered(String unpacked) throws MPCDesignationException {
        String u = unpacked.trim();

        // Match "1P" or "354P" or "73P-A" or "73P-AA" or "1P/Halley" (with optional name after slash)
        Pattern pattern = Pattern.compile("^(\\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$");
        Matcher matcher = pattern.matcher(u);
        if (!matcher.matches()) {
            throw new MPCDesignationException("Invalid unpacked numbered comet designation");
        }

        int number = Integer.parseInt(matcher.group(1));
        String cometType = matcher.group(2);
        String fragment = matcher.group(3);

        if (number < 1 || number > 9999) {
            throw new MPCDesignationException("Comet number out of range (1-9999): " + number);
        }

        String result = String.format("%04d%s", number, cometType);
        if (fragment != null && !fragment.isEmpty()) {
            result += fragment.toLowerCase();
        }
        return result;
    }

    // =========================================================================
    // Natural satellite designations
    // =========================================================================

    public static String unpackSatellite(String packed) throws MPCDesignationException {
        String p = packed.trim();

        if (p.length() != 8 || p.charAt(0) != 'S') {
            throw new MPCDesignationException("Invalid packed satellite designation");
        }

        char century = p.charAt(1);
        String year = p.substring(2, 4);
        char planet = p.charAt(4);
        String numberEncoded = p.substring(5, 7);

        if (!CENTURY_CODES.containsKey(century)) {
            throw new MPCDesignationException("Invalid century code: " + century);
        }

        if (SATELLITE_PLANETS.indexOf(planet) < 0) {
            throw new MPCDesignationException("Invalid planet code: " + planet);
        }

        String fullYear = CENTURY_CODES.get(century) + year;
        int number = decodeCycleCount(numberEncoded);

        return "S/" + fullYear + " " + planet + " " + number;
    }

    public static String packSatellite(String unpacked) throws MPCDesignationException {
        String u = unpacked.trim();

        Pattern pattern = Pattern.compile("^S/(\\d{4}) ([JSUN]) (\\d+)$");
        Matcher matcher = pattern.matcher(u);
        if (!matcher.matches()) {
            throw new MPCDesignationException("Invalid unpacked satellite designation");
        }

        String year = matcher.group(1);
        char planet = matcher.group(2).charAt(0);
        int number = Integer.parseInt(matcher.group(3));

        if (number < 1) {
            throw new MPCDesignationException("Satellite number must be positive");
        }

        int century = Integer.parseInt(year.substring(0, 2));
        String yearShort = year.substring(2, 4);

        if (!REVERSE_CENTURY_CODES.containsKey(century)) {
            throw new MPCDesignationException("Invalid century in year: " + year);
        }

        char centuryCode = REVERSE_CENTURY_CODES.get(century);
        String numberEncoded = encodeCycleCount(number);

        return "S" + centuryCode + yearShort + planet + numberEncoded + "0";
    }

    // =========================================================================
    // BCE year encoding for ancient comets
    // =========================================================================

    private static String[] encodeBCEYear(int year) throws MPCDesignationException {
        if (year >= 0) {
            throw new MPCDesignationException("Not a BCE year: " + year);
        }

        int absYear = Math.abs(year);
        int code = 99 - (absYear % 100);

        String prefix;
        if (absYear < 100) {
            prefix = "/";
        } else if (absYear < 200) {
            prefix = ".";
        } else if (absYear < 300) {
            prefix = "-";
        } else {
            throw new MPCDesignationException("BCE year out of supported range: " + year);
        }

        return new String[]{prefix, String.format("%02d", code)};
    }

    private static int decodeBCEYear(char prefix, String code) throws MPCDesignationException {
        int codeNum = Integer.parseInt(code);
        int yearPart = 99 - codeNum;

        switch (prefix) {
            case '/': return -yearPart;
            case '.': return -(yearPart + 100);
            case '-': return -(yearPart + 200);
            default:
                throw new MPCDesignationException("Invalid BCE prefix: " + prefix);
        }
    }

    // =========================================================================
    // Ancient/BCE comet provisional designations
    // =========================================================================

    private static String packAncientCometProvisional(char cometType, int year, char halfMonth,
                                                       int orderNum, String fragment)
            throws MPCDesignationException {
        String orderEncoded = encodeCycleCount(orderNum);
        String fragmentCode = fragment.isEmpty() ? "0" : fragment.toLowerCase();

        if (year < 0) {
            String[] bceEncoded = encodeBCEYear(year);
            return "" + cometType + bceEncoded[0] + bceEncoded[1] + halfMonth + orderEncoded + fragmentCode;
        }

        return String.format("%c%03d%c%s%s", cometType, year, halfMonth, orderEncoded, fragmentCode);
    }

    public static String unpackAncientCometProvisional(String packed) throws MPCDesignationException {
        String p = packed.trim();

        if (p.length() != 8) {
            throw new MPCDesignationException("Invalid ancient comet designation length");
        }

        char cometType = p.charAt(0);
        if (COMET_TYPES.indexOf(cometType) < 0) {
            throw new MPCDesignationException("Invalid comet type: " + cometType);
        }

        int year;
        char halfMonth;
        String orderEncoded;
        char fragment;

        char possiblePrefix = p.charAt(1);
        if (possiblePrefix == '/' || possiblePrefix == '.' || possiblePrefix == '-') {
            year = decodeBCEYear(possiblePrefix, p.substring(2, 4));
            halfMonth = p.charAt(4);
            orderEncoded = p.substring(5, 7);
            fragment = p.charAt(7);
        } else {
            year = Integer.parseInt(p.substring(1, 4));
            halfMonth = p.charAt(4);
            orderEncoded = p.substring(5, 7);
            fragment = p.charAt(7);
        }

        int orderNum = decodeCycleCount(orderEncoded);
        String result = "" + cometType + "/" + year + " " + halfMonth + orderNum;

        if (fragment != '0') {
            result += "-" + Character.toUpperCase(fragment);
        }

        return result;
    }

    // =========================================================================
    // Helper functions for comet format detection
    // =========================================================================

    private static boolean isAsteroidStylePacked(String provisionalPart) {
        if (provisionalPart.length() != 7) {
            return false;
        }
        char lastChar = provisionalPart.charAt(6);
        return lastChar >= 'A' && lastChar <= 'Z';
    }

    private static boolean isAsteroidStyleUnpacked(String provisional) {
        Pattern pattern = Pattern.compile("^\\d{4} ([A-Z])(.)");
        Matcher matcher = pattern.matcher(provisional);
        if (matcher.find()) {
            char secondChar = matcher.group(2).charAt(0);
            return Character.isLetter(secondChar);
        }
        return false;
    }

    // =========================================================================
    // Full comet designations (with type prefix)
    // =========================================================================

    public static String unpackCometFull(String packed) throws MPCDesignationException {
        String p = packed;
        int length = p.length();

        if (length == 8) {
            // Compact 8-char format: type + 7-char provisional
            char cometType = p.charAt(0);
            String provisionalPart = p.substring(1, 8);

            if (COMET_TYPES.indexOf(cometType) < 0) {
                throw new MPCDesignationException("Invalid comet type: " + cometType);
            }

            String provisional;
            if (isAsteroidStylePacked(provisionalPart)) {
                provisional = unpackProvisional(provisionalPart);
            } else {
                provisional = unpackCometProvisional(provisionalPart);
            }

            return cometType + "/" + provisional;
        }

        if (length == 9) {
            // Compact 9-char format with 2-letter fragment
            char cometType = p.charAt(0);
            String provisionalPart = p.substring(1, 9);

            if (COMET_TYPES.indexOf(cometType) < 0) {
                throw new MPCDesignationException("Invalid comet type: " + cometType);
            }

            String provisional = unpackCometProvisional(provisionalPart);
            return cometType + "/" + provisional;
        }

        if (length == 12 || (length < 12 && p.charAt(0) == ' ')) {
            // Full 12-char format or trimmed version
            while (p.length() < 12) {
                p = " " + p;
            }

            String numPart = p.substring(0, 4).trim();
            char cometType = p.charAt(4);
            String provisionalPart = p.substring(5, 12);

            if (COMET_TYPES.indexOf(cometType) < 0) {
                throw new MPCDesignationException("Invalid comet type: " + cometType);
            }

            String provisional;
            if (isAsteroidStylePacked(provisionalPart)) {
                provisional = unpackProvisional(provisionalPart);
            } else {
                provisional = unpackCometProvisional(provisionalPart);
            }

            if (numPart.isEmpty()) {
                return cometType + "/" + provisional;
            }

            int num = Integer.parseInt(numPart);
            return num + "" + cometType + "/" + provisional;
        }

        throw new MPCDesignationException("Invalid packed full comet designation length");
    }

    public static String packCometFull(String unpacked) throws MPCDesignationException {
        String u = unpacked.trim();

        // Match: optional number, type, slash, year, provisional
        Pattern pattern = Pattern.compile("^(\\d*)([PCDXAI])/(-?\\d+) (.+)$");
        Matcher matcher = pattern.matcher(u);
        if (!matcher.matches()) {
            throw new MPCDesignationException("Invalid unpacked comet designation: " + u);
        }

        String numberStr = matcher.group(1);
        char cometType = matcher.group(2).charAt(0);
        int year = Integer.parseInt(matcher.group(3));
        String provPart = matcher.group(4);

        // Check for ancient or BCE year
        if (year < 1000) {
            // Parse the provisional part for ancient comets: "L1" or "L1-F"
            Pattern ancientPattern = Pattern.compile("^([A-Z])(\\d+)(?:-([A-Z]))?$");
            Matcher ancientMatcher = ancientPattern.matcher(provPart);
            if (ancientMatcher.matches()) {
                char halfMonth = ancientMatcher.group(1).charAt(0);
                int orderNum = Integer.parseInt(ancientMatcher.group(2));
                String fragment = ancientMatcher.group(3);
                return packAncientCometProvisional(cometType, year, halfMonth, orderNum,
                                                    fragment == null ? "" : fragment);
            } else {
                throw new MPCDesignationException("Invalid ancient comet provisional: " + provPart);
            }
        }

        // Modern comet - reconstruct provisional with year
        String provisional = year + " " + provPart;

        String provisionalPacked;
        if (isAsteroidStyleUnpacked(provisional)) {
            provisionalPacked = packProvisional(provisional);
        } else {
            provisionalPacked = packCometProvisional(provisional);
        }

        if (numberStr.isEmpty()) {
            return cometType + provisionalPacked;
        }

        int num = Integer.parseInt(numberStr);
        if (num < 1 || num > 9999) {
            throw new MPCDesignationException("Comet number out of range (1-9999): " + num);
        }

        return String.format("%04d%c%s", num, cometType, provisionalPacked);
    }

    // =========================================================================
    // Format detection helper functions
    // =========================================================================

    private static boolean isAllDigits(String s) {
        if (s.isEmpty()) return false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') return false;
        }
        return true;
    }

    private static boolean isBase62(char c) {
        return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    }

    private static boolean isDigitOrLower(char c) {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z');
    }

    // =========================================================================
    // Format detection
    // =========================================================================

    public static Info detectFormat(String designation) throws MPCDesignationException {
        Info info = new Info();

        // Validate raw input BEFORE trimming
        validateRawInput(designation);

        // Check for packed full comet designation BEFORE trimming (12 chars with spaces)
        if (designation.length() == 12) {
            if (Pattern.matches("^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation)) {
                info.format = FormatType.PACKED;
                info.type = "comet_full";
                info.subtype = "comet with provisional designation (12-char)";
                return info;
            }
        }

        // Check for packed comet designation (8 chars: type + 7 char provisional)
        if (designation.length() == 8 && COMET_TYPES.indexOf(designation.charAt(0)) >= 0) {
            if (Pattern.matches("^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$", designation)) {
                info.format = FormatType.PACKED;
                info.type = "comet_full";
                info.subtype = "comet with provisional designation (8-char)";
                return info;
            }
        }

        // Check for packed comet with 2-letter fragment (9 chars)
        if (designation.length() == 9 && COMET_TYPES.indexOf(designation.charAt(0)) >= 0) {
            if (Pattern.matches("^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$", designation)) {
                info.format = FormatType.PACKED;
                info.type = "comet_full";
                info.subtype = "comet with provisional designation (9-char, 2-letter fragment)";
                return info;
            }
        }

        // Check for packed ancient comet (8 chars)
        if (designation.length() == 8 && COMET_TYPES.indexOf(designation.charAt(0)) >= 0) {
            if (Pattern.matches("^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation)) {
                info.format = FormatType.PACKED;
                info.type = "comet_ancient";
                info.subtype = "comet with ancient provisional (year < 1000)";
                return info;
            }
        }

        // Check for packed BCE comet (8 chars)
        if (designation.length() == 8 && COMET_TYPES.indexOf(designation.charAt(0)) >= 0) {
            if (Pattern.matches("^([PCDXAI])([/.\\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation)) {
                info.format = FormatType.PACKED;
                info.type = "comet_bce";
                info.subtype = "comet with BCE provisional";
                return info;
            }
        }

        String des = designation.trim();

        // Validate whitespace
        validateWhitespace(des);

        // Check for packed satellite designation (8 chars starting with S)
        if (des.length() == 8 && des.charAt(0) == 'S') {
            if (Pattern.matches("^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$", des)) {
                info.format = FormatType.PACKED;
                info.type = "satellite";
                char planet = des.charAt(4);
                String planetName = SATELLITE_PLANET_NAMES.getOrDefault(planet, String.valueOf(planet));
                info.subtype = "natural satellite (" + planetName + ")";
                return info;
            }
        }

        // Check for packed permanent (numbered) asteroid
        if (des.length() == 5) {
            if (des.charAt(0) == '~') {
                if (Pattern.matches("^~[0-9A-Za-z]{4}$", des)) {
                    info.format = FormatType.PACKED;
                    info.type = "permanent";
                    info.subtype = "permanent numbered (tilde/base-62, >= 620000)";
                    return info;
                }
            } else if (isAllDigits(des)) {
                info.format = FormatType.PACKED;
                info.type = "permanent";
                info.subtype = "permanent numbered (5-digit, < 100000)";
                return info;
            } else if (Pattern.matches("^[A-Za-z][0-9]{4}$", des)) {
                info.format = FormatType.PACKED;
                info.type = "permanent";
                if (Character.isUpperCase(des.charAt(0))) {
                    info.subtype = "permanent numbered (letter-prefix, 100000-359999)";
                } else {
                    info.subtype = "permanent numbered (letter-prefix, 360000-619999)";
                }
                return info;
            }

            // Check for packed numbered comet (5 chars ending in P or D)
            if (Pattern.matches("^[0-9]{4}[PD]$", des)) {
                info.format = FormatType.PACKED;
                info.type = "comet_numbered";
                char cometType = des.charAt(4);
                String typeDesc = COMET_TYPE_DESCRIPTIONS.getOrDefault(cometType, String.valueOf(cometType));
                info.subtype = "comet numbered " + typeDesc;
                return info;
            }
        }

        // Check for packed numbered comet with fragment (6-7 chars: ####Pa or ####Paa)
        if (des.length() == 6 || des.length() == 7) {
            if (Pattern.matches("^[0-9]{4}[PD][a-z]{1,2}$", des)) {
                info.format = FormatType.PACKED;
                info.type = "comet_numbered";
                char cometType = des.charAt(4);
                String typeDesc = COMET_TYPE_DESCRIPTIONS.getOrDefault(cometType, String.valueOf(cometType));
                int fragLen = des.length() - 5;
                if (fragLen == 1) {
                    info.subtype = "comet numbered " + typeDesc + " with fragment";
                } else {
                    info.subtype = "comet numbered " + typeDesc + " with 2-letter fragment";
                }
                return info;
            }
        }

        // Check for packed provisional asteroid (7 chars)
        if (des.length() == 7) {
            // Extended format with underscore
            if (des.charAt(0) == '_') {
                if (Pattern.matches("^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$", des)) {
                    info.format = FormatType.PACKED;
                    info.type = "provisional_extended";
                    info.subtype = "provisional (extended format, cycle >=620)";
                    return info;
                }
            }

            // Standard provisional
            if (Pattern.matches("^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$", des)) {
                info.format = FormatType.PACKED;
                info.type = "provisional";
                info.subtype = "provisional";
                return info;
            }

            // Survey designations
            if (des.startsWith("PLS") && isAllDigits(des.substring(3))) {
                info.format = FormatType.PACKED;
                info.type = "survey";
                info.subtype = "survey (Palomar-Leiden)";
                return info;
            }

            if (Pattern.matches("^T[123]S\\d{4}$", des)) {
                info.format = FormatType.PACKED;
                info.type = "survey";
                info.subtype = "survey (Trojan T-" + des.charAt(1) + ")";
                return info;
            }

            // Check for packed comet provisional (7 chars)
            if (Pattern.matches("^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", des)) {
                info.format = FormatType.PACKED;
                info.type = "comet_provisional";
                info.subtype = "comet provisional";
                return info;
            }
        }

        // --- UNPACKED FORMATS ---

        // Check for unpacked satellite
        if (Pattern.matches("^S/\\d{4} ([JSUN]) \\d+$", des)) {
            info.format = FormatType.UNPACKED;
            info.type = "satellite";
            Matcher m = Pattern.compile("^S/\\d{4} ([JSUN]) \\d+$").matcher(des);
            if (m.matches()) {
                char planet = m.group(1).charAt(0);
                String planetName = SATELLITE_PLANET_NAMES.getOrDefault(planet, String.valueOf(planet));
                info.subtype = "natural satellite (" + planetName + ")";
            }
            return info;
        }

        // Check for unpacked permanent (numbered) asteroid
        if (isAllDigits(des)) {
            info.format = FormatType.UNPACKED;
            info.type = "permanent";
            info.subtype = "permanent numbered";
            return info;
        }

        // Check for unpacked survey designation
        if (Pattern.matches("^\\d+ (P-L|T-[123])$", des)) {
            info.format = FormatType.UNPACKED;
            info.type = "survey";
            Matcher m = Pattern.compile("^\\d+ (P-L|T-[123])$").matcher(des);
            if (m.matches()) {
                String survey = m.group(1);
                if (survey.equals("P-L")) {
                    info.subtype = "survey (Palomar-Leiden)";
                } else {
                    info.subtype = "survey (Trojan " + survey + ")";
                }
            }
            return info;
        }

        // Check for old-style asteroid designation
        if (Pattern.matches("^[AB]\\d{3} [A-Z][A-Z]$", des)) {
            info.format = FormatType.UNPACKED;
            info.type = "provisional";
            info.subtype = "provisional (old-style pre-1925)";
            return info;
        }

        // Check for unpacked provisional asteroid
        if (Pattern.matches("^\\d{4} [A-Z][A-Z]\\d*$", des)) {
            info.format = FormatType.UNPACKED;
            info.type = "provisional";
            info.subtype = "provisional";
            return info;
        }

        // Check for unpacked comet with type prefix
        if (Pattern.matches("^(\\d*)([PCDXAI])/(-?\\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$", des)) {
            Matcher m = Pattern.compile("^(\\d*)([PCDXAI])/(-?\\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$").matcher(des);
            if (m.matches()) {
                String num = m.group(1);
                char ctype = m.group(2).charAt(0);
                int year = Integer.parseInt(m.group(3));

                String yearDesc = "";
                if (year < 0) {
                    yearDesc = "BCE";
                } else if (year < 1000) {
                    yearDesc = "ancient";
                }

                info.format = FormatType.UNPACKED;
                info.type = "comet_full";

                String typeDesc = COMET_TYPE_DESCRIPTIONS.getOrDefault(ctype, String.valueOf(ctype));
                if (!num.isEmpty()) {
                    if (!yearDesc.isEmpty()) {
                        info.subtype = "comet numbered with " + yearDesc + " provisional (" + typeDesc + ")";
                    } else {
                        info.subtype = "comet numbered with provisional (" + typeDesc + ")";
                    }
                } else {
                    if (!yearDesc.isEmpty()) {
                        info.subtype = "comet " + yearDesc + " provisional (" + typeDesc + ")";
                    } else {
                        info.subtype = "comet provisional (" + typeDesc + ")";
                    }
                }
            }
            return info;
        }

        // Check for unpacked numbered periodic comet (with optional fragment)
        if (Pattern.matches("^(\\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$", des)) {
            info.format = FormatType.UNPACKED;
            info.type = "comet_numbered";
            Matcher m = Pattern.compile("^(\\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$").matcher(des);
            if (m.matches()) {
                char cometType = m.group(2).charAt(0);
                String typeDesc = COMET_TYPE_DESCRIPTIONS.getOrDefault(cometType, String.valueOf(cometType));
                String fragment = m.group(3);
                if (fragment != null && !fragment.isEmpty()) {
                    if (fragment.length() == 1) {
                        info.subtype = "comet numbered " + typeDesc + " with fragment";
                    } else {
                        info.subtype = "comet numbered " + typeDesc + " with 2-letter fragment";
                    }
                } else {
                    info.subtype = "comet numbered " + typeDesc;
                }
            }
            return info;
        }

        throw new MPCDesignationException("Unable to detect designation format: " + designation);
    }

    // =========================================================================
    // Main conversion functions
    // =========================================================================

    /**
     * Convert a designation between packed and unpacked formats.
     * Auto-detects the input format and converts to the other.
     */
    public static Result convert(String designation) throws MPCDesignationException {
        Info info = detectFormat(designation);
        String output;

        if (info.format == FormatType.PACKED) {
            switch (info.type) {
                case "permanent":
                    output = String.valueOf(unpackPermanent(designation));
                    break;
                case "provisional":
                case "survey":
                    output = unpackProvisional(designation);
                    break;
                case "provisional_extended":
                    output = unpackExtendedProvisional(designation);
                    break;
                case "comet_numbered":
                    output = unpackCometNumbered(designation);
                    break;
                case "comet_provisional":
                    output = unpackCometProvisional(designation);
                    break;
                case "comet_full":
                    output = unpackCometFull(designation);
                    break;
                case "comet_ancient":
                case "comet_bce":
                    output = unpackAncientCometProvisional(designation);
                    break;
                case "satellite":
                    output = unpackSatellite(designation);
                    break;
                default:
                    throw new MPCDesignationException("Unknown type: " + info.type);
            }
        } else {
            switch (info.type) {
                case "permanent":
                    try {
                        long num = Long.parseLong(designation.trim());
                        if (num < 1 || num > MAX_ASTEROID_NUMBER) {
                            throw new MPCDesignationException("Invalid asteroid number: " + num);
                        }
                        output = packPermanent((int) num);
                    } catch (NumberFormatException e) {
                        throw new MPCDesignationException("Invalid asteroid number (overflow): " + designation.trim());
                    }
                    break;
                case "provisional":
                case "survey":
                    output = packProvisional(designation);
                    break;
                case "comet_numbered":
                    output = packCometNumbered(designation);
                    break;
                case "comet_full":
                    output = packCometFull(designation);
                    break;
                case "satellite":
                    output = packSatellite(designation);
                    break;
                default:
                    throw new MPCDesignationException("Unknown type: " + info.type);
            }
        }

        return new Result(designation, output, info);
    }

    /**
     * Convert a designation and return just the output string.
     */
    public static String convertSimple(String designation) throws MPCDesignationException {
        return convert(designation).output;
    }

    /**
     * Ensure a designation is in packed format.
     */
    public static String pack(String designation) throws MPCDesignationException {
        Info info = detectFormat(designation);
        if (info.format == FormatType.PACKED) {
            return designation.trim();
        }
        return convert(designation).output;
    }

    /**
     * Ensure a designation is in unpacked (human-readable) format.
     */
    public static String unpack(String designation) throws MPCDesignationException {
        Info info = detectFormat(designation);
        if (info.format == FormatType.UNPACKED) {
            return designation.trim();
        }
        return convert(designation).output;
    }

    /**
     * Check if a string is a valid MPC designation.
     */
    public static boolean isValidDesignation(String designation) {
        if (designation == null || designation.isEmpty()) {
            return false;
        }
        try {
            detectFormat(designation);
            return true;
        } catch (MPCDesignationException e) {
            return false;
        }
    }

    // =========================================================================
    // Helper Functions for Format Conversion and Fragment Handling
    // =========================================================================

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
    public static String toReportFormat(String minimal) throws MPCDesignationException {
        minimal = minimal.trim();
        int length = minimal.length();

        Info info = detectFormat(minimal);

        if (info.format != FormatType.PACKED) {
            throw new MPCDesignationException("toReportFormat requires packed format input: " + minimal);
        }

        // Initialize 12-char output with spaces
        char[] report = "            ".toCharArray();

        switch (info.type) {
            case "permanent":
                // Right-align 5-char designation
                for (int i = 0; i < length; i++) {
                    report[12 - length + i] = minimal.charAt(i);
                }
                break;

            case "provisional":
            case "provisional_extended":
            case "survey":
                // Right-align 7-char designation
                for (int i = 0; i < length; i++) {
                    report[12 - length + i] = minimal.charAt(i);
                }
                break;

            case "comet_numbered":
                // Numbered comet: first 5 chars (####P), fragment in cols 11-12
                if (length == 5) {
                    // No fragment
                    for (int i = 0; i < 5; i++) {
                        report[i] = minimal.charAt(i);
                    }
                } else if (length == 6) {
                    // Single-letter fragment
                    for (int i = 0; i < 5; i++) {
                        report[i] = minimal.charAt(i);
                    }
                    report[11] = minimal.charAt(5);
                } else if (length == 7) {
                    // Two-letter fragment
                    for (int i = 0; i < 5; i++) {
                        report[i] = minimal.charAt(i);
                    }
                    report[10] = minimal.charAt(5);
                    report[11] = minimal.charAt(6);
                }
                break;

            case "comet_provisional":
            case "comet_full":
            case "comet_ancient":
            case "comet_bce":
                // Right-align in 12-char field
                for (int i = 0; i < length; i++) {
                    report[12 - length + i] = minimal.charAt(i);
                }
                break;

            case "satellite":
                // Right-align 8-char designation
                for (int i = 0; i < length; i++) {
                    report[12 - length + i] = minimal.charAt(i);
                }
                break;

            default:
                throw new MPCDesignationException("Unsupported type for report format: " + info.type);
        }

        return new String(report);
    }

    /**
     * Convert 12-character MPC report format to minimal packed format.
     */
    public static String fromReportFormat(String report) throws MPCDesignationException {
        if (report.length() > 12) {
            throw new MPCDesignationException("Report format too long: " + report);
        }

        // Pad to 12 chars if shorter
        while (report.length() < 12) {
            report = " " + report;
        }

        // Check for numbered comet with fragment (fragment in cols 11-12)
        // Pattern: ####P or ####D in cols 1-5, spaces in cols 6-10, lowercase in cols 11-12
        String first5 = report.substring(0, 5);
        String middle = report.substring(5, 10);
        char frag1 = report.charAt(10);
        char frag2 = report.charAt(11);

        // Check if this is a numbered comet format
        if (Pattern.matches("^[0-9]{4}[PD]$", first5) && middle.trim().isEmpty()) {
            StringBuilder result = new StringBuilder(first5);
            if (frag1 >= 'a' && frag1 <= 'z') {
                result.append(frag1);
            }
            if (frag2 >= 'a' && frag2 <= 'z') {
                result.append(frag2);
            }
            return result.toString();
        }

        // Standard case: just trim spaces
        return report.trim();
    }

    /**
     * Check if a designation has a comet fragment suffix.
     * Works with both packed and unpacked formats.
     */
    public static boolean hasFragment(String desig) {
        try {
            Info info = detectFormat(desig);
            String dtype = info.type;

            // Only comets can have fragments
            if (!dtype.equals("comet_numbered") && !dtype.equals("comet_provisional") && !dtype.equals("comet_full")) {
                return false;
            }

            desig = desig.trim();
            int length = desig.length();

            if (info.format == FormatType.UNPACKED) {
                // Look for "-X" or "-XX" at end
                return Pattern.matches(".*-[A-Z]{1,2}$", desig);
            } else {
                // Packed format
                if (dtype.equals("comet_numbered")) {
                    // Check for lowercase after P/D (position 5+)
                    if (length > 5) {
                        char c = desig.charAt(5);
                        return c >= 'a' && c <= 'z';
                    }
                } else if (dtype.equals("comet_provisional")) {
                    // 7-char: last char lowercase and not '0'
                    char lastChar = desig.charAt(length - 1);
                    return lastChar >= 'a' && lastChar <= 'z' && lastChar != '0';
                } else if (dtype.equals("comet_full")) {
                    char lastChar = desig.charAt(length - 1);
                    return lastChar >= 'a' && lastChar <= 'z' && lastChar != '0';
                }
            }
        } catch (MPCDesignationException e) {
            return false;
        }
        return false;
    }

    /**
     * Extract the fragment suffix from a comet designation.
     * Works with both packed and unpacked formats.
     * Fragment is returned in uppercase (e.g., "A", "AA").
     * Returns empty string if no fragment.
     */
    public static String getFragment(String desig) throws MPCDesignationException {
        Info info = detectFormat(desig);
        String dtype = info.type;

        // Only comets can have fragments
        if (!dtype.equals("comet_numbered") && !dtype.equals("comet_provisional") && !dtype.equals("comet_full")) {
            return "";
        }

        desig = desig.trim();
        int length = desig.length();

        if (info.format == FormatType.UNPACKED) {
            // Look for "-X" or "-XX" at end
            Matcher m = Pattern.compile("-([A-Z]{1,2})$").matcher(desig);
            if (m.find()) {
                return m.group(1);
            }
        } else {
            // Packed format
            if (dtype.equals("comet_numbered")) {
                // Fragment is lowercase after P/D
                if (length == 6) {
                    return String.valueOf(desig.charAt(5)).toUpperCase();
                } else if (length == 7) {
                    return desig.substring(5, 7).toUpperCase();
                }
            } else if (dtype.equals("comet_provisional")) {
                // 7-char: position 6 if lowercase and not '0'
                // 8-char: positions 6-7 if lowercase
                if (length == 7) {
                    char lastChar = desig.charAt(6);
                    if (lastChar >= 'a' && lastChar <= 'z' && lastChar != '0') {
                        return String.valueOf(lastChar).toUpperCase();
                    }
                } else if (length == 8) {
                    String frag = desig.substring(6, 8);
                    if (frag.charAt(0) >= 'a' && frag.charAt(0) <= 'z' &&
                        frag.charAt(1) >= 'a' && frag.charAt(1) <= 'z') {
                        return frag.toUpperCase();
                    }
                }
            } else if (dtype.equals("comet_full")) {
                // 8-char: position 7 if lowercase and not '0'
                // 9-char: positions 7-8 if lowercase
                if (length == 8) {
                    char lastChar = desig.charAt(7);
                    if (lastChar >= 'a' && lastChar <= 'z' && lastChar != '0') {
                        return String.valueOf(lastChar).toUpperCase();
                    }
                } else if (length == 9) {
                    String frag = desig.substring(7, 9);
                    if (frag.charAt(0) >= 'a' && frag.charAt(0) <= 'z' &&
                        frag.charAt(1) >= 'a' && frag.charAt(1) <= 'z') {
                        return frag.toUpperCase();
                    }
                }
            }
        }

        return "";
    }

    /**
     * Get the parent comet designation (without fragment suffix).
     * Works with both packed and unpacked formats.
     * Returns the designation in the same format (packed or unpacked) as input.
     */
    public static String getParent(String desig) throws MPCDesignationException {
        Info info = detectFormat(desig);
        String dtype = info.type;

        // Non-comets: return as-is
        if (!dtype.equals("comet_numbered") && !dtype.equals("comet_provisional") && !dtype.equals("comet_full")) {
            return desig.trim();
        }

        desig = desig.trim();
        int length = desig.length();

        if (info.format == FormatType.UNPACKED) {
            // Remove "-X" or "-XX" suffix if present
            return desig.replaceAll("-[A-Z]{1,2}$", "");
        } else {
            // Packed format
            if (dtype.equals("comet_numbered")) {
                // Remove lowercase fragment letters after P/D
                if (length > 5) {
                    char c = desig.charAt(5);
                    if (c >= 'a' && c <= 'z') {
                        return desig.substring(0, 5);
                    }
                }
            } else if (dtype.equals("comet_provisional")) {
                // 7-char: replace lowercase fragment with '0'
                // 8-char: replace 2 lowercase with '0', truncate
                if (length == 7) {
                    char lastChar = desig.charAt(6);
                    if (lastChar >= 'a' && lastChar <= 'z' && lastChar != '0') {
                        return desig.substring(0, 6) + "0";
                    }
                } else if (length == 8) {
                    char c = desig.charAt(6);
                    if (c >= 'a' && c <= 'z') {
                        return desig.substring(0, 6) + "0";
                    }
                }
            } else if (dtype.equals("comet_full")) {
                // 8-char: replace fragment with '0'
                // 9-char: replace fragment with '0', truncate
                if (length == 8) {
                    char lastChar = desig.charAt(7);
                    if (lastChar >= 'a' && lastChar <= 'z' && lastChar != '0') {
                        return desig.substring(0, 7) + "0";
                    }
                } else if (length == 9) {
                    char c = desig.charAt(7);
                    if (c >= 'a' && c <= 'z') {
                        return desig.substring(0, 7) + "0";
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
    public static boolean designationsEqual(String desig1, String desig2) {
        try {
            String packed1 = pack(desig1);
            String packed2 = pack(desig2);
            return packed1.equals(packed2);
        } catch (MPCDesignationException e) {
            return false;
        }
    }
}
