package mpc

/**
 * Convert between packed and unpacked Minor Planet Center (MPC) designations
 * for asteroids, comets, and natural satellites.
 *
 * Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
 */
object MPCDesignation {

    const val VERSION = "1.0.0"

    // Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
    const val MAX_ASTEROID_NUMBER = 15396335

    // Base-62 character set
    private const val BASE62_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    // Century codes for provisional designations
    private val centuryCodes = mapOf(
        'A' to 10, 'B' to 11, 'C' to 12, 'D' to 13, 'E' to 14, 'F' to 15,
        'G' to 16, 'H' to 17, 'I' to 18, 'J' to 19, 'K' to 20, 'L' to 21
    )
    private val reverseCenturyCodes = centuryCodes.entries.associate { it.value to it.key }

    // Survey codes
    private val surveyPackedToUnpacked = mapOf(
        "PLS" to "P-L", "T1S" to "T-1", "T2S" to "T-2", "T3S" to "T-3"
    )
    private val surveyUnpackedToPacked = surveyPackedToUnpacked.entries.associate { it.value to it.key }

    // Comet types
    private const val COMET_TYPES = "PCDXAI"
    private val cometTypeDescriptions = mapOf(
        'P' to "periodic", 'C' to "non-periodic", 'D' to "defunct",
        'X' to "uncertain orbit", 'A' to "asteroid with comet designation", 'I' to "interstellar"
    )

    // Satellite planet codes
    private const val SATELLITE_PLANETS = "JSUN"
    private val satellitePlanetNames = mapOf(
        'J' to "Jupiter", 'S' to "Saturn", 'U' to "Uranus", 'N' to "Neptune"
    )

    /**
     * Format type enumeration
     */
    enum class FormatType(val displayName: String) {
        PACKED("packed"),
        UNPACKED("unpacked")
    }

    /**
     * Information about a detected designation format
     */
    data class Info(
        val format: FormatType? = null,
        val type: String = "",
        val subtype: String = ""
    )

    /**
     * Conversion result containing input, output, and format information
     */
    data class Result(
        val input: String,
        val output: String,
        val info: Info
    )

    /**
     * Exception for invalid MPC designations
     */
    class MPCDesignationException(message: String) : Exception(message)

    // =========================================================================
    // Input validation
    // =========================================================================

    private fun validateRawInput(s: String) {
        for (c in s) {
            if (c.code < 32 || c.code > 126) {
                throw MPCDesignationException("Invalid character in designation: '\\x${c.code.toString(16).padStart(2, '0')}'")
            }
        }
    }

    private fun validateWhitespace(s: String) {
        var prevSpace = false
        for (c in s) {
            if (c.code < 32 || c.code > 126) {
                throw MPCDesignationException("Invalid character in designation")
            }
            if (c == ' ') {
                if (prevSpace) {
                    throw MPCDesignationException("Consecutive spaces in designation")
                }
                prevSpace = true
            } else {
                prevSpace = false
            }
        }
    }

    private fun sanitize(designation: String): String {
        validateRawInput(designation)
        val result = designation.trim()
        if (result.isEmpty()) {
            throw MPCDesignationException("Empty designation")
        }
        return result
    }

    private fun isValidHalfMonth(c: Char): Boolean = c in 'A'..'Y' && c != 'I'

    // =========================================================================
    // Base-62 encoding utilities
    // =========================================================================

    private fun base62ToNum(c: Char): Int {
        val idx = BASE62_CHARS.indexOf(c)
        if (idx < 0) {
            throw MPCDesignationException("Invalid base-62 character: $c")
        }
        return idx
    }

    private fun numToBase62(n: Int): Char {
        if (n < 0 || n > 61) {
            throw MPCDesignationException("Number out of base-62 range: $n")
        }
        return BASE62_CHARS[n]
    }

    private fun base62StringToNum(s: String): Int {
        var result = 0
        for (c in s) {
            result = result * 62 + base62ToNum(c)
        }
        return result
    }

    private fun numToBase62String(n: Int, width: Int): String {
        val result = CharArray(width)
        var num = n
        for (i in width - 1 downTo 0) {
            result[i] = numToBase62(num % 62)
            num /= 62
        }
        return String(result)
    }

    // =========================================================================
    // Cycle count encoding for provisional designations
    // =========================================================================

    private fun decodeCycleCount(encoded: String): Int {
        if (encoded.length < 2) {
            throw MPCDesignationException("Invalid cycle count encoding")
        }

        val first = encoded[0]
        val second = encoded[1]

        val tens = when {
            first in '0'..'9' -> first - '0'
            first in 'A'..'Z' -> first - 'A' + 10
            first in 'a'..'z' -> first - 'a' + 36
            else -> throw MPCDesignationException("Invalid cycle count encoding")
        }

        if (second !in '0'..'9') {
            throw MPCDesignationException("Invalid cycle count encoding")
        }

        return tens * 10 + (second - '0')
    }

    private fun encodeCycleCount(count: Int): String {
        if (count < 0 || count >= 620) {
            throw MPCDesignationException("Cycle count out of range (0-619): $count")
        }

        val tens = count / 10
        val ones = count % 10

        val first = when {
            tens < 10 -> ('0' + tens)
            tens < 36 -> ('A' + tens - 10)
            else -> ('a' + tens - 36)
        }

        return "$first$ones"
    }

    // =========================================================================
    // Letter/position utilities for provisional designations
    // =========================================================================

    private fun letterToPosition(letter: Char): Int {
        if (letter !in 'A'..'Z') {
            throw MPCDesignationException("Invalid half-month letter: $letter")
        }
        var pos = letter - 'A' + 1
        if (letter > 'I') {
            pos-- // Skip I
        }
        return pos
    }

    private fun positionToLetter(pos: Int): Char {
        if (pos < 1 || pos > 25) {
            throw MPCDesignationException("Invalid letter position: $pos")
        }
        var p = pos
        if (p >= 9) {
            p++ // Skip I
        }
        return 'A' + p - 1
    }

    // =========================================================================
    // Permanent (numbered) asteroid designations
    // =========================================================================

    fun unpackPermanent(packed: String): Int {
        val p = packed.trim()
        if (p.length != 5) {
            throw MPCDesignationException("Invalid packed permanent designation length")
        }

        val first = p[0]

        // Tilde format (>= 620,000)
        if (first == '~') {
            return 620000 + base62StringToNum(p.substring(1, 5))
        }

        // Simple numeric format (< 100,000)
        if (first in '0'..'9') {
            return p.toIntOrNull() ?: throw MPCDesignationException("Invalid packed permanent designation")
        }

        // Extended format with uppercase letter (100,000 - 359,999)
        if (first in 'A'..'Z') {
            val value = first.code - 55 // A=10, B=11, etc.
            val rest = p.substring(1, 5).toIntOrNull()
                ?: throw MPCDesignationException("Invalid packed permanent designation")
            return value * 10000 + rest
        }

        // Extended format with lowercase letter (360,000 - 619,999)
        if (first in 'a'..'z') {
            val value = first.code - 61 // a=36, b=37, etc.
            val rest = p.substring(1, 5).toIntOrNull()
                ?: throw MPCDesignationException("Invalid packed permanent designation")
            return value * 10000 + rest
        }

        throw MPCDesignationException("Invalid packed permanent designation")
    }

    fun packPermanent(number: Int): String {
        if (number < 1 || number > MAX_ASTEROID_NUMBER) {
            throw MPCDesignationException("Invalid asteroid number: $number")
        }

        if (number < 100000) {
            return number.toString().padStart(5, '0')
        }

        if (number < 620000) {
            val div = number / 10000
            val mod = number % 10000
            val letter = if (div < 36) {
                (div + 55).toChar() // A-Z
            } else {
                (div + 61).toChar() // a-z
            }
            return "$letter${mod.toString().padStart(4, '0')}"
        }

        // Tilde + base-62 format
        val offset = number - 620000
        return "~${numToBase62String(offset, 4)}"
    }

    // =========================================================================
    // Standard provisional asteroid designations
    // =========================================================================

    fun unpackProvisional(packed: String): String {
        val p = packed.trim()

        // Check for survey designations first
        if (p.length == 7 && p.substring(0, 3) in surveyPackedToUnpacked) {
            val survey = surveyPackedToUnpacked[p.substring(0, 3)]!!
            val num = p.substring(3, 7).toIntOrNull()
                ?: throw MPCDesignationException("Invalid survey number")
            return "$num $survey"
        }

        if (p.length != 7) {
            throw MPCDesignationException("Invalid packed provisional designation length")
        }

        val century = p[0]
        val year = p.substring(1, 3)
        val halfMonth = p[3]
        val orderEncoded = p.substring(4, 6)
        val secondLetter = p[6]

        if (century !in centuryCodes) {
            throw MPCDesignationException("Invalid century code: $century")
        }

        val fullYear = "${centuryCodes[century]}$year"
        val orderNum = decodeCycleCount(orderEncoded)

        return if (orderNum == 0) {
            "$fullYear $halfMonth$secondLetter"
        } else {
            "$fullYear $halfMonth$secondLetter$orderNum"
        }
    }

    fun packProvisional(unpacked: String): String {
        val u = unpacked.trim()

        // Check for survey designations
        val surveyPattern = Regex("""^(\d+) (P-L|T-[123])$""")
        surveyPattern.matchEntire(u)?.let { match ->
            val number = match.groupValues[1].toInt()
            val survey = match.groupValues[2]
            if (number < 1) {
                throw MPCDesignationException("Survey number must be positive")
            }
            return "${surveyUnpackedToPacked[survey]}${number.toString().padStart(4, '0')}"
        }

        // Check for old-style designation: "A908 CJ" or "B842 FA"
        val oldStylePattern = Regex("""^[AB](\d)(\d{2}) ([A-Z])([A-Z])$""")
        oldStylePattern.matchEntire(u)?.let { match ->
            val centuryDigit = match.groupValues[1][0]
            val yearShort = match.groupValues[2]
            val halfMonth = match.groupValues[3][0]
            val secondLetter = match.groupValues[4][0]

            val centuryCode = when (centuryDigit) {
                '8' -> 'I'
                '9' -> 'J'
                '0' -> 'K'
                else -> throw MPCDesignationException("Invalid century digit in old-style designation")
            }

            return "$centuryCode${yearShort}${halfMonth}00$secondLetter"
        }

        // Match standard provisional: "1995 XA" or "1995 XA12"
        val provisionalPattern = Regex("""^(\d{4}) ([A-Z])([A-Z])(\d*)$""")
        val match = provisionalPattern.matchEntire(u)
            ?: throw MPCDesignationException("Invalid unpacked provisional designation: $u")

        val year = match.groupValues[1]
        val halfMonth = match.groupValues[2][0]
        val secondLetter = match.groupValues[3][0]
        val orderStr = match.groupValues[4]

        if (!isValidHalfMonth(halfMonth)) {
            throw MPCDesignationException("Invalid half-month letter: $halfMonth")
        }

        val century = year.substring(0, 2).toInt()
        val yearShort = year.substring(2, 4)

        if (century !in reverseCenturyCodes) {
            throw MPCDesignationException("Invalid century in year: $year")
        }

        val centuryCode = reverseCenturyCodes[century]!!
        val orderNum = if (orderStr.isEmpty()) {
            0
        } else {
            try {
                val orderLong = orderStr.toLong()
                if (orderLong > Int.MAX_VALUE) {
                    throw MPCDesignationException("Cycle count out of range (overflow): $orderStr")
                }
                orderLong.toInt()
            } catch (e: NumberFormatException) {
                throw MPCDesignationException("Cycle count out of range (overflow): $orderStr")
            }
        }

        // Check if we need extended format
        if (orderNum >= 620) {
            return packExtendedProvisional(year.toInt(), halfMonth, secondLetter, orderNum)
        }

        val orderEncoded = encodeCycleCount(orderNum)
        return "$centuryCode$yearShort$halfMonth$orderEncoded$secondLetter"
    }

    // =========================================================================
    // Extended provisional format (cycle >= 620)
    // =========================================================================

    private fun packExtendedProvisional(year: Int, halfMonth: Char, secondLetter: Char, cycle: Int): String {
        val yearShort = year % 100
        val letterPos = letterToPosition(secondLetter)
        val baseSequence = (cycle - 620) * 25 + letterPos - 1
        val seqEncoded = numToBase62String(baseSequence, 4)
        val yearChar = numToBase62(yearShort)
        return "_$yearChar$halfMonth$seqEncoded"
    }

    fun unpackExtendedProvisional(packed: String): String {
        val p = packed.trim()
        if (p.length != 7 || p[0] != '_') {
            throw MPCDesignationException("Invalid extended packed provisional")
        }

        val yearDigit = p[1]
        val halfMonth = p[2]
        val seqEncoded = p.substring(3, 7)

        val baseSequence = base62StringToNum(seqEncoded)
        val cycle = 620 + baseSequence / 25
        val letterPos = (baseSequence % 25) + 1
        val secondLetter = positionToLetter(letterPos)

        val yearVal = base62ToNum(yearDigit)
        val year = 2000 + yearVal

        return "$year $halfMonth$secondLetter$cycle"
    }

    // =========================================================================
    // Comet provisional designations
    // =========================================================================

    fun unpackCometProvisional(packed: String): String {
        val p = packed.trim()
        val length = p.length

        if (length != 7 && length != 8) {
            throw MPCDesignationException("Invalid packed comet provisional designation length")
        }

        val century = p[0]
        val year = p.substring(1, 3)
        val halfMonth = p[3]
        val orderEncoded = p.substring(4, 6)
        val fragment = if (length == 7) p[6].toString() else p.substring(6, 8)

        if (century !in centuryCodes) {
            throw MPCDesignationException("Invalid century code: $century")
        }

        val fullYear = "${centuryCodes[century]}$year"
        val orderNum = decodeCycleCount(orderEncoded)

        var result = "$fullYear $halfMonth$orderNum"
        if (fragment != "0") {
            result += "-${fragment.uppercase()}"
        }

        return result
    }

    fun packCometProvisional(unpacked: String): String {
        val u = unpacked.trim()

        // Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
        val pattern = Regex("""^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$""")
        val match = pattern.matchEntire(u)
            ?: throw MPCDesignationException("Invalid unpacked comet provisional designation: $u")

        val year = match.groupValues[1]
        val halfMonth = match.groupValues[2][0]
        val orderStr = match.groupValues[3]
        val fragment = match.groupValues[4].takeIf { it.isNotEmpty() }

        val orderNum = try {
            val orderLong = orderStr.toLong()
            if (orderLong > Int.MAX_VALUE) {
                throw MPCDesignationException("Comet order number out of range (overflow): $orderStr")
            }
            orderLong.toInt()
        } catch (e: NumberFormatException) {
            throw MPCDesignationException("Comet order number out of range (overflow): $orderStr")
        }

        if (orderNum < 1) {
            throw MPCDesignationException("Comet order number must be positive")
        }

        val century = year.substring(0, 2).toInt()
        val yearShort = year.substring(2, 4)

        if (century !in reverseCenturyCodes) {
            throw MPCDesignationException("Invalid century in year: $year")
        }

        val centuryCode = reverseCenturyCodes[century]!!
        val orderEncoded = encodeCycleCount(orderNum)
        val fragmentCode = fragment?.lowercase() ?: "0"

        return "$centuryCode$yearShort$halfMonth$orderEncoded$fragmentCode"
    }

    // =========================================================================
    // Numbered comet designations
    // =========================================================================

    fun unpackCometNumbered(packed: String): String {
        val p = packed.trim()

        val pattern = Regex("""^(\d{4})([PD])$""")
        val match = pattern.matchEntire(p)
            ?: throw MPCDesignationException("Invalid packed numbered comet designation")

        val number = match.groupValues[1].toInt()
        val cometType = match.groupValues[2]
        return "$number$cometType"
    }

    fun packCometNumbered(unpacked: String): String {
        val u = unpacked.trim()

        // Match "1P" or "354P" or "1P/Halley" (with optional name after slash)
        val pattern = Regex("""^(\d+)([PD])(?:/[A-Za-z].*)?$""")
        val match = pattern.matchEntire(u)
            ?: throw MPCDesignationException("Invalid unpacked numbered comet designation")

        val number = match.groupValues[1].toInt()
        val cometType = match.groupValues[2]

        if (number < 1 || number > 9999) {
            throw MPCDesignationException("Comet number out of range (1-9999): $number")
        }

        return "${number.toString().padStart(4, '0')}$cometType"
    }

    // =========================================================================
    // Natural satellite designations
    // =========================================================================

    fun unpackSatellite(packed: String): String {
        val p = packed.trim()

        if (p.length != 8 || p[0] != 'S') {
            throw MPCDesignationException("Invalid packed satellite designation")
        }

        val century = p[1]
        val year = p.substring(2, 4)
        val planet = p[4]
        val numberEncoded = p.substring(5, 7)

        if (century !in centuryCodes) {
            throw MPCDesignationException("Invalid century code: $century")
        }

        if (planet !in SATELLITE_PLANETS) {
            throw MPCDesignationException("Invalid planet code: $planet")
        }

        val fullYear = "${centuryCodes[century]}$year"
        val number = decodeCycleCount(numberEncoded)

        return "S/$fullYear $planet $number"
    }

    fun packSatellite(unpacked: String): String {
        val u = unpacked.trim()

        val pattern = Regex("""^S/(\d{4}) ([JSUN]) (\d+)$""")
        val match = pattern.matchEntire(u)
            ?: throw MPCDesignationException("Invalid unpacked satellite designation")

        val year = match.groupValues[1]
        val planet = match.groupValues[2][0]
        val number = match.groupValues[3].toInt()

        if (number < 1) {
            throw MPCDesignationException("Satellite number must be positive")
        }

        val century = year.substring(0, 2).toInt()
        val yearShort = year.substring(2, 4)

        if (century !in reverseCenturyCodes) {
            throw MPCDesignationException("Invalid century in year: $year")
        }

        val centuryCode = reverseCenturyCodes[century]!!
        val numberEncoded = encodeCycleCount(number)

        return "S$centuryCode$yearShort$planet${numberEncoded}0"
    }

    // =========================================================================
    // BCE year encoding for ancient comets
    // =========================================================================

    private fun encodeBCEYear(year: Int): Pair<String, String> {
        if (year >= 0) {
            throw MPCDesignationException("Not a BCE year: $year")
        }

        val absYear = kotlin.math.abs(year)
        val code = 99 - (absYear % 100)

        val prefix = when {
            absYear < 100 -> "/"
            absYear < 200 -> "."
            absYear < 300 -> "-"
            else -> throw MPCDesignationException("BCE year out of supported range: $year")
        }

        return prefix to code.toString().padStart(2, '0')
    }

    private fun decodeBCEYear(prefix: Char, code: String): Int {
        val codeNum = code.toInt()
        val yearPart = 99 - codeNum

        return when (prefix) {
            '/' -> -yearPart
            '.' -> -(yearPart + 100)
            '-' -> -(yearPart + 200)
            else -> throw MPCDesignationException("Invalid BCE prefix: $prefix")
        }
    }

    // =========================================================================
    // Ancient/BCE comet provisional designations
    // =========================================================================

    private fun packAncientCometProvisional(
        cometType: Char,
        year: Int,
        halfMonth: Char,
        orderNum: Int,
        fragment: String
    ): String {
        val orderEncoded = encodeCycleCount(orderNum)
        val fragmentCode = if (fragment.isEmpty()) "0" else fragment.lowercase()

        if (year < 0) {
            val (prefix, bceCode) = encodeBCEYear(year)
            return "$cometType$prefix$bceCode$halfMonth$orderEncoded$fragmentCode"
        }

        return "$cometType${year.toString().padStart(3, '0')}$halfMonth$orderEncoded$fragmentCode"
    }

    fun unpackAncientCometProvisional(packed: String): String {
        val p = packed.trim()

        if (p.length != 8) {
            throw MPCDesignationException("Invalid ancient comet designation length")
        }

        val cometType = p[0]
        if (cometType !in COMET_TYPES) {
            throw MPCDesignationException("Invalid comet type: $cometType")
        }

        val year: Int
        val halfMonth: Char
        val orderEncoded: String
        val fragment: Char

        val possiblePrefix = p[1]
        if (possiblePrefix == '/' || possiblePrefix == '.' || possiblePrefix == '-') {
            year = decodeBCEYear(possiblePrefix, p.substring(2, 4))
            halfMonth = p[4]
            orderEncoded = p.substring(5, 7)
            fragment = p[7]
        } else {
            year = p.substring(1, 4).toInt()
            halfMonth = p[4]
            orderEncoded = p.substring(5, 7)
            fragment = p[7]
        }

        val orderNum = decodeCycleCount(orderEncoded)
        var result = "$cometType/$year $halfMonth$orderNum"

        if (fragment != '0') {
            result += "-${fragment.uppercaseChar()}"
        }

        return result
    }

    // =========================================================================
    // Helper functions for comet format detection
    // =========================================================================

    private fun isAsteroidStylePacked(provisionalPart: String): Boolean {
        if (provisionalPart.length != 7) return false
        val lastChar = provisionalPart[6]
        return lastChar in 'A'..'Z'
    }

    private fun isAsteroidStyleUnpacked(provisional: String): Boolean {
        val pattern = Regex("""^\d{4} ([A-Z])(.)""")
        val match = pattern.find(provisional) ?: return false
        val secondChar = match.groupValues[2][0]
        return secondChar.isLetter()
    }

    // =========================================================================
    // Full comet designations (with type prefix)
    // =========================================================================

    fun unpackCometFull(packed: String): String {
        var p = packed
        val length = p.length

        if (length == 8) {
            // Compact 8-char format: type + 7-char provisional
            val cometType = p[0]
            val provisionalPart = p.substring(1, 8)

            if (cometType !in COMET_TYPES) {
                throw MPCDesignationException("Invalid comet type: $cometType")
            }

            val provisional = if (isAsteroidStylePacked(provisionalPart)) {
                unpackProvisional(provisionalPart)
            } else {
                unpackCometProvisional(provisionalPart)
            }

            return "$cometType/$provisional"
        }

        if (length == 9) {
            // Compact 9-char format with 2-letter fragment
            val cometType = p[0]
            val provisionalPart = p.substring(1, 9)

            if (cometType !in COMET_TYPES) {
                throw MPCDesignationException("Invalid comet type: $cometType")
            }

            val provisional = unpackCometProvisional(provisionalPart)
            return "$cometType/$provisional"
        }

        if (length == 12 || (length < 12 && p[0] == ' ')) {
            // Full 12-char format or trimmed version
            while (p.length < 12) {
                p = " $p"
            }

            val numPart = p.substring(0, 4).trim()
            val cometType = p[4]
            val provisionalPart = p.substring(5, 12)

            if (cometType !in COMET_TYPES) {
                throw MPCDesignationException("Invalid comet type: $cometType")
            }

            val provisional = if (isAsteroidStylePacked(provisionalPart)) {
                unpackProvisional(provisionalPart)
            } else {
                unpackCometProvisional(provisionalPart)
            }

            return if (numPart.isEmpty()) {
                "$cometType/$provisional"
            } else {
                val num = numPart.toInt()
                "$num$cometType/$provisional"
            }
        }

        throw MPCDesignationException("Invalid packed full comet designation length")
    }

    fun packCometFull(unpacked: String): String {
        val u = unpacked.trim()

        // Match: optional number, type, slash, year, provisional
        val pattern = Regex("""^(\d*)([PCDXAI])/(-?\d+) (.+)$""")
        val match = pattern.matchEntire(u)
            ?: throw MPCDesignationException("Invalid unpacked comet designation: $u")

        val numberStr = match.groupValues[1]
        val cometType = match.groupValues[2][0]
        val year = match.groupValues[3].toInt()
        val provPart = match.groupValues[4]

        // Check for ancient or BCE year
        if (year < 1000) {
            // Parse the provisional part for ancient comets: "L1" or "L1-F"
            val ancientPattern = Regex("""^([A-Z])(\d+)(?:-([A-Z]))?$""")
            val ancientMatch = ancientPattern.matchEntire(provPart)
                ?: throw MPCDesignationException("Invalid ancient comet provisional: $provPart")

            val halfMonth = ancientMatch.groupValues[1][0]
            val orderNum = ancientMatch.groupValues[2].toInt()
            val fragment = ancientMatch.groupValues[3]
            return packAncientCometProvisional(cometType, year, halfMonth, orderNum, fragment)
        }

        // Modern comet - reconstruct provisional with year
        val provisional = "$year $provPart"

        val provisionalPacked = if (isAsteroidStyleUnpacked(provisional)) {
            packProvisional(provisional)
        } else {
            packCometProvisional(provisional)
        }

        if (numberStr.isEmpty()) {
            return "$cometType$provisionalPacked"
        }

        val num = numberStr.toInt()
        if (num < 1 || num > 9999) {
            throw MPCDesignationException("Comet number out of range (1-9999): $num")
        }

        return "${num.toString().padStart(4, '0')}$cometType$provisionalPacked"
    }

    // =========================================================================
    // Format detection helper functions
    // =========================================================================

    private fun isAllDigits(s: String): Boolean {
        return s.isNotEmpty() && s.all { it in '0'..'9' }
    }

    // =========================================================================
    // Format detection
    // =========================================================================

    fun detectFormat(designation: String): Info {
        // Validate raw input BEFORE trimming
        validateRawInput(designation)

        // Check for packed full comet designation BEFORE trimming (12 chars with spaces)
        if (designation.length == 12) {
            val pattern = Regex("""^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$""")
            if (pattern.matches(designation)) {
                return Info(FormatType.PACKED, "comet_full", "comet with provisional designation (12-char)")
            }
        }

        // Check for packed comet designation (8 chars: type + 7 char provisional)
        if (designation.length == 8 && designation[0] in COMET_TYPES) {
            val pattern = Regex("""^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$""")
            if (pattern.matches(designation)) {
                return Info(FormatType.PACKED, "comet_full", "comet with provisional designation (8-char)")
            }
        }

        // Check for packed comet with 2-letter fragment (9 chars)
        if (designation.length == 9 && designation[0] in COMET_TYPES) {
            val pattern = Regex("""^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$""")
            if (pattern.matches(designation)) {
                return Info(FormatType.PACKED, "comet_full", "comet with provisional designation (9-char, 2-letter fragment)")
            }
        }

        // Check for packed ancient comet (8 chars)
        if (designation.length == 8 && designation[0] in COMET_TYPES) {
            val pattern = Regex("""^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$""")
            if (pattern.matches(designation)) {
                return Info(FormatType.PACKED, "comet_ancient", "comet with ancient provisional (year < 1000)")
            }
        }

        // Check for packed BCE comet (8 chars)
        if (designation.length == 8 && designation[0] in COMET_TYPES) {
            val pattern = Regex("""^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$""")
            if (pattern.matches(designation)) {
                return Info(FormatType.PACKED, "comet_bce", "comet with BCE provisional")
            }
        }

        val des = designation.trim()

        // Validate whitespace
        validateWhitespace(des)

        // Check for packed satellite designation (8 chars starting with S)
        if (des.length == 8 && des[0] == 'S') {
            val pattern = Regex("""^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$""")
            if (pattern.matches(des)) {
                val planet = des[4]
                val planetName = satellitePlanetNames[planet] ?: planet.toString()
                return Info(FormatType.PACKED, "satellite", "natural satellite ($planetName)")
            }
        }

        // Check for packed permanent (numbered) asteroid
        if (des.length == 5) {
            if (des[0] == '~') {
                val pattern = Regex("""^~[0-9A-Za-z]{4}$""")
                if (pattern.matches(des)) {
                    return Info(FormatType.PACKED, "permanent", "permanent numbered (tilde/base-62, >= 620000)")
                }
            } else if (isAllDigits(des)) {
                return Info(FormatType.PACKED, "permanent", "permanent numbered (5-digit, < 100000)")
            } else {
                val pattern = Regex("""^[A-Za-z][0-9]{4}$""")
                if (pattern.matches(des)) {
                    val subtype = if (des[0].isUpperCase()) {
                        "permanent numbered (letter-prefix, 100000-359999)"
                    } else {
                        "permanent numbered (letter-prefix, 360000-619999)"
                    }
                    return Info(FormatType.PACKED, "permanent", subtype)
                }
            }

            // Check for packed numbered comet (5 chars ending in P or D)
            val cometPattern = Regex("""^[0-9]{4}[PD]$""")
            if (cometPattern.matches(des)) {
                val cometType = des[4]
                val typeDesc = cometTypeDescriptions[cometType] ?: cometType.toString()
                return Info(FormatType.PACKED, "comet_numbered", "comet numbered $typeDesc")
            }
        }

        // Check for packed provisional asteroid (7 chars)
        if (des.length == 7) {
            // Extended format with underscore
            if (des[0] == '_') {
                val pattern = Regex("""^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$""")
                if (pattern.matches(des)) {
                    return Info(FormatType.PACKED, "provisional_extended", "provisional (extended format, cycle >=620)")
                }
            }

            // Standard provisional
            val provPattern = Regex("""^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$""")
            if (provPattern.matches(des)) {
                return Info(FormatType.PACKED, "provisional", "provisional")
            }

            // Survey designations
            if (des.startsWith("PLS") && isAllDigits(des.substring(3))) {
                return Info(FormatType.PACKED, "survey", "survey (Palomar-Leiden)")
            }

            val surveyPattern = Regex("""^T[123]S\d{4}$""")
            if (surveyPattern.matches(des)) {
                return Info(FormatType.PACKED, "survey", "survey (Trojan T-${des[1]})")
            }

            // Check for packed comet provisional (7 chars)
            val cometProvPattern = Regex("""^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$""")
            if (cometProvPattern.matches(des)) {
                return Info(FormatType.PACKED, "comet_provisional", "comet provisional")
            }
        }

        // --- UNPACKED FORMATS ---

        // Check for unpacked satellite
        val satPattern = Regex("""^S/\d{4} ([JSUN]) \d+$""")
        satPattern.matchEntire(des)?.let { match ->
            val planet = match.groupValues[1][0]
            val planetName = satellitePlanetNames[planet] ?: planet.toString()
            return Info(FormatType.UNPACKED, "satellite", "natural satellite ($planetName)")
        }

        // Check for unpacked permanent (numbered) asteroid
        if (isAllDigits(des)) {
            return Info(FormatType.UNPACKED, "permanent", "permanent numbered")
        }

        // Check for unpacked survey designation
        val surveyUnpackedPattern = Regex("""^\d+ (P-L|T-[123])$""")
        surveyUnpackedPattern.matchEntire(des)?.let { match ->
            val survey = match.groupValues[1]
            val subtype = if (survey == "P-L") {
                "survey (Palomar-Leiden)"
            } else {
                "survey (Trojan $survey)"
            }
            return Info(FormatType.UNPACKED, "survey", subtype)
        }

        // Check for old-style asteroid designation
        val oldStylePattern = Regex("""^[AB]\d{3} [A-Z][A-Z]$""")
        if (oldStylePattern.matches(des)) {
            return Info(FormatType.UNPACKED, "provisional", "provisional (old-style pre-1925)")
        }

        // Check for unpacked provisional asteroid
        val provUnpackedPattern = Regex("""^\d{4} [A-Z][A-Z]\d*$""")
        if (provUnpackedPattern.matches(des)) {
            return Info(FormatType.UNPACKED, "provisional", "provisional")
        }

        // Check for unpacked comet with type prefix
        val cometFullPattern = Regex("""^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$""")
        cometFullPattern.matchEntire(des)?.let { match ->
            val num = match.groupValues[1]
            val ctype = match.groupValues[2][0]
            val year = match.groupValues[3].toInt()

            val yearDesc = when {
                year < 0 -> "BCE"
                year < 1000 -> "ancient"
                else -> ""
            }

            val typeDesc = cometTypeDescriptions[ctype] ?: ctype.toString()
            val subtype = if (num.isNotEmpty()) {
                if (yearDesc.isNotEmpty()) {
                    "comet numbered with $yearDesc provisional ($typeDesc)"
                } else {
                    "comet numbered with provisional ($typeDesc)"
                }
            } else {
                if (yearDesc.isNotEmpty()) {
                    "comet $yearDesc provisional ($typeDesc)"
                } else {
                    "comet provisional ($typeDesc)"
                }
            }

            return Info(FormatType.UNPACKED, "comet_full", subtype)
        }

        // Check for unpacked numbered periodic comet
        val numberedCometPattern = Regex("""^(\d+)([PD])(?:/[A-Za-z].*)?$""")
        numberedCometPattern.matchEntire(des)?.let { match ->
            val cometType = match.groupValues[2][0]
            val typeDesc = cometTypeDescriptions[cometType] ?: cometType.toString()
            return Info(FormatType.UNPACKED, "comet_numbered", "comet numbered $typeDesc")
        }

        throw MPCDesignationException("Unable to detect designation format: $designation")
    }

    // =========================================================================
    // Main conversion functions
    // =========================================================================

    /**
     * Convert a designation between packed and unpacked formats.
     * Auto-detects the input format and converts to the other.
     */
    fun convert(designation: String): Result {
        val info = detectFormat(designation)

        val output = if (info.format == FormatType.PACKED) {
            when (info.type) {
                "permanent" -> unpackPermanent(designation).toString()
                "provisional", "survey" -> unpackProvisional(designation)
                "provisional_extended" -> unpackExtendedProvisional(designation)
                "comet_numbered" -> unpackCometNumbered(designation)
                "comet_provisional" -> unpackCometProvisional(designation)
                "comet_full" -> unpackCometFull(designation)
                "comet_ancient", "comet_bce" -> unpackAncientCometProvisional(designation)
                "satellite" -> unpackSatellite(designation)
                else -> throw MPCDesignationException("Unknown type: ${info.type}")
            }
        } else {
            when (info.type) {
                "permanent" -> {
                    try {
                        val num = designation.trim().toLong()
                        if (num < 1 || num > MAX_ASTEROID_NUMBER) {
                            throw MPCDesignationException("Invalid asteroid number: $num")
                        }
                        packPermanent(num.toInt())
                    } catch (e: NumberFormatException) {
                        throw MPCDesignationException("Invalid asteroid number (overflow): ${designation.trim()}")
                    }
                }
                "provisional", "survey" -> packProvisional(designation)
                "comet_numbered" -> packCometNumbered(designation)
                "comet_full" -> packCometFull(designation)
                "satellite" -> packSatellite(designation)
                else -> throw MPCDesignationException("Unknown type: ${info.type}")
            }
        }

        return Result(designation, output, info)
    }

    /**
     * Convert a designation and return just the output string.
     */
    fun convertSimple(designation: String): String = convert(designation).output

    /**
     * Ensure a designation is in packed format.
     */
    fun pack(designation: String): String {
        val info = detectFormat(designation)
        return if (info.format == FormatType.PACKED) {
            designation.trim()
        } else {
            convert(designation).output
        }
    }

    /**
     * Ensure a designation is in unpacked (human-readable) format.
     */
    fun unpack(designation: String): String {
        val info = detectFormat(designation)
        return if (info.format == FormatType.UNPACKED) {
            designation.trim()
        } else {
            convert(designation).output
        }
    }

    /**
     * Check if a string is a valid MPC designation.
     */
    fun isValidDesignation(designation: String?): Boolean {
        if (designation.isNullOrEmpty()) return false
        return try {
            detectFormat(designation)
            true
        } catch (e: MPCDesignationException) {
            false
        }
    }
}
