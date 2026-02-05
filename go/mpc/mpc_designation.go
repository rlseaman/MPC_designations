// Package mpc provides functions for converting between packed and unpacked
// Minor Planet Center (MPC) designations for asteroids, comets, and natural satellites.
//
// Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
package mpc

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// Version of the library
const Version = "1.0.0"

// Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
const MaxAsteroidNumber = 15396335

// Error types
var (
	ErrInvalidFormat    = errors.New("invalid format")
	ErrOutOfRange       = errors.New("out of range")
	ErrInvalidCharacter = errors.New("invalid character")
	ErrEmptyDesignation = errors.New("empty designation")
)

// FormatType indicates whether a designation is packed or unpacked
type FormatType int

const (
	FormatPacked FormatType = iota
	FormatUnpacked
)

func (f FormatType) String() string {
	if f == FormatPacked {
		return "packed"
	}
	return "unpacked"
}

// Info contains information about a detected designation format
type Info struct {
	Format  FormatType
	Type    string
	Subtype string
}

// Result contains the conversion result and format information
type Result struct {
	Input  string
	Output string
	Info   Info
}

// Base-62 character set
const base62Chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

// Century codes for provisional designations
var centuryCodes = map[byte]int{
	'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15,
	'G': 16, 'H': 17, 'I': 18, 'J': 19, 'K': 20, 'L': 21,
}

var reverseCenturyCodes = map[int]byte{
	10: 'A', 11: 'B', 12: 'C', 13: 'D', 14: 'E', 15: 'F',
	16: 'G', 17: 'H', 18: 'I', 19: 'J', 20: 'K', 21: 'L',
}

// Survey codes
var surveyPackedToUnpacked = map[string]string{
	"PLS": "P-L", "T1S": "T-1", "T2S": "T-2", "T3S": "T-3",
}

var surveyUnpackedToPacked = map[string]string{
	"P-L": "PLS", "T-1": "T1S", "T-2": "T2S", "T-3": "T3S",
}

// Comet types
var cometTypes = map[byte]bool{
	'P': true, 'C': true, 'D': true, 'X': true, 'A': true, 'I': true,
}

var cometTypeDescriptions = map[byte]string{
	'P': "periodic", 'C': "non-periodic", 'D': "defunct",
	'X': "uncertain orbit", 'A': "asteroid with comet designation", 'I': "interstellar",
}

// Satellite planet codes
var satellitePlanets = map[byte]bool{'J': true, 'S': true, 'U': true, 'N': true}

var satellitePlanetNames = map[byte]string{
	'J': "Jupiter", 'S': "Saturn", 'U': "Uranus", 'N': "Neptune",
}

// validateRawInput validates characters before trimming
func validateRawInput(s string) error {
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c < 32 || c > 126 {
			return fmt.Errorf("%w: invalid character '\\x%02x'", ErrInvalidCharacter, c)
		}
	}
	return nil
}

// validateWhitespace checks for consecutive spaces
func validateWhitespace(s string) error {
	prevSpace := false
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c < 32 || c > 126 {
			return fmt.Errorf("%w: invalid character", ErrInvalidCharacter)
		}
		if c == ' ' {
			if prevSpace {
				return fmt.Errorf("%w: consecutive spaces", ErrInvalidFormat)
			}
			prevSpace = true
		} else {
			prevSpace = false
		}
	}
	return nil
}

// sanitize validates and trims a designation string
func sanitize(designation string) (string, error) {
	if err := validateRawInput(designation); err != nil {
		return "", err
	}
	result := strings.TrimSpace(designation)
	if result == "" {
		return "", ErrEmptyDesignation
	}
	return result, nil
}

// isValidHalfMonth checks if a letter is valid for half-month (A-Y excluding I)
func isValidHalfMonth(c byte) bool {
	return c >= 'A' && c <= 'Y' && c != 'I'
}

// base62ToNum converts a base-62 character to its numeric value
func base62ToNum(c byte) (int, error) {
	idx := strings.IndexByte(base62Chars, c)
	if idx < 0 {
		return 0, fmt.Errorf("%w: invalid base-62 character '%c'", ErrInvalidFormat, c)
	}
	return idx, nil
}

// numToBase62 converts a numeric value (0-61) to its base-62 character
func numToBase62(n int) (byte, error) {
	if n < 0 || n > 61 {
		return 0, fmt.Errorf("%w: number out of base-62 range: %d", ErrOutOfRange, n)
	}
	return base62Chars[n], nil
}

// base62StringToNum converts a base-62 string to a number
func base62StringToNum(s string) (int, error) {
	result := 0
	for i := 0; i < len(s); i++ {
		val, err := base62ToNum(s[i])
		if err != nil {
			return 0, err
		}
		result = result*62 + val
	}
	return result, nil
}

// numToBase62String converts a number to a base-62 string of specified width
func numToBase62String(n, width int) (string, error) {
	result := make([]byte, width)
	for i := width - 1; i >= 0; i-- {
		c, err := numToBase62(n % 62)
		if err != nil {
			return "", err
		}
		result[i] = c
		n /= 62
	}
	return string(result), nil
}

// decodeCycleCount decodes the cycle count from packed provisional format
func decodeCycleCount(encoded string) (int, error) {
	if len(encoded) < 2 {
		return 0, fmt.Errorf("%w: invalid cycle count encoding", ErrInvalidFormat)
	}

	first := encoded[0]
	second := encoded[1]

	var tens int
	if first >= '0' && first <= '9' {
		tens = int(first - '0')
	} else if first >= 'A' && first <= 'Z' {
		tens = int(first-'A') + 10
	} else if first >= 'a' && first <= 'z' {
		tens = int(first-'a') + 36
	} else {
		return 0, fmt.Errorf("%w: invalid cycle count encoding", ErrInvalidFormat)
	}

	if second < '0' || second > '9' {
		return 0, fmt.Errorf("%w: invalid cycle count encoding", ErrInvalidFormat)
	}

	return tens*10 + int(second-'0'), nil
}

// encodeCycleCount encodes a cycle count for packed provisional format
func encodeCycleCount(count int) (string, error) {
	if count < 0 || count >= 620 {
		return "", fmt.Errorf("%w: cycle count out of range (0-619): %d", ErrOutOfRange, count)
	}

	tens := count / 10
	ones := count % 10

	var first byte
	if tens < 10 {
		first = byte('0' + tens)
	} else if tens < 36 {
		first = byte('A' + tens - 10)
	} else {
		first = byte('a' + tens - 36)
	}

	return string([]byte{first, byte('0' + ones)}), nil
}

// letterToPosition converts a half-month letter to its position (A=1, B=2, ..., skipping I)
func letterToPosition(letter byte) (int, error) {
	if letter < 'A' || letter > 'Z' {
		return 0, fmt.Errorf("%w: invalid half-month letter: %c", ErrInvalidFormat, letter)
	}
	pos := int(letter-'A') + 1
	if letter > 'I' {
		pos--
	}
	return pos, nil
}

// positionToLetter converts a position to half-month letter (1=A, 2=B, ..., skipping I)
func positionToLetter(pos int) (byte, error) {
	// Second letters A-Z excluding I = 25 positions (1-25)
	// A-H = positions 1-8, J-Z = positions 9-25 (I is skipped)
	if pos < 1 || pos > 25 {
		return 0, fmt.Errorf("%w: invalid letter position: %d", ErrOutOfRange, pos)
	}
	p := pos
	if p >= 9 {
		p++ // Skip I
	}
	return byte('A' + p - 1), nil
}

// UnpackPermanent unpacks a permanent (numbered) asteroid designation
func UnpackPermanent(packed string) (int, error) {
	p := strings.TrimSpace(packed)
	if len(p) != 5 {
		return 0, fmt.Errorf("%w: invalid packed permanent designation length", ErrInvalidFormat)
	}

	first := p[0]

	// Tilde format (>= 620,000)
	if first == '~' {
		val, err := base62StringToNum(p[1:5])
		if err != nil {
			return 0, err
		}
		return 620000 + val, nil
	}

	// Simple numeric format (< 100,000)
	if first >= '0' && first <= '9' {
		num, err := strconv.Atoi(p)
		if err != nil {
			return 0, fmt.Errorf("%w: invalid packed permanent designation", ErrInvalidFormat)
		}
		return num, nil
	}

	// Extended format with uppercase letter (100,000 - 359,999)
	if first >= 'A' && first <= 'Z' {
		val := int(first) - 55 // A=10, B=11, etc.
		rest, err := strconv.Atoi(p[1:5])
		if err != nil {
			return 0, fmt.Errorf("%w: invalid packed permanent designation", ErrInvalidFormat)
		}
		return val*10000 + rest, nil
	}

	// Extended format with lowercase letter (360,000 - 619,999)
	if first >= 'a' && first <= 'z' {
		val := int(first) - 61 // a=36, b=37, etc.
		rest, err := strconv.Atoi(p[1:5])
		if err != nil {
			return 0, fmt.Errorf("%w: invalid packed permanent designation", ErrInvalidFormat)
		}
		return val*10000 + rest, nil
	}

	return 0, fmt.Errorf("%w: invalid packed permanent designation", ErrInvalidFormat)
}

// PackPermanent packs a permanent (numbered) asteroid designation
func PackPermanent(number int) (string, error) {
	if number < 1 || number > MaxAsteroidNumber {
		return "", fmt.Errorf("%w: invalid asteroid number: %d", ErrOutOfRange, number)
	}

	if number < 100000 {
		return fmt.Sprintf("%05d", number), nil
	}

	if number < 620000 {
		div := number / 10000
		mod := number % 10000
		var letter byte
		if div < 36 {
			letter = byte(div + 55) // A-Z
		} else {
			letter = byte(div + 61) // a-z
		}
		return fmt.Sprintf("%c%04d", letter, mod), nil
	}

	// Tilde + base-62 format
	offset := number - 620000
	b62, err := numToBase62String(offset, 4)
	if err != nil {
		return "", err
	}
	return "~" + b62, nil
}

// UnpackProvisional unpacks a provisional asteroid designation
func UnpackProvisional(packed string) (string, error) {
	p := strings.TrimSpace(packed)

	// Check for survey designations first
	if len(p) == 7 {
		prefix := p[0:3]
		if survey, ok := surveyPackedToUnpacked[prefix]; ok {
			num, err := strconv.Atoi(p[3:7])
			if err != nil {
				return "", fmt.Errorf("%w: invalid survey number", ErrInvalidFormat)
			}
			return fmt.Sprintf("%d %s", num, survey), nil
		}
	}

	if len(p) != 7 {
		return "", fmt.Errorf("%w: invalid packed provisional designation length", ErrInvalidFormat)
	}

	century := p[0]
	year := p[1:3]
	halfMonth := p[3]
	orderEncoded := p[4:6]
	secondLetter := p[6]

	centuryVal, ok := centuryCodes[century]
	if !ok {
		return "", fmt.Errorf("%w: invalid century code: %c", ErrInvalidFormat, century)
	}

	// Asteroid provisionals: only I-L valid (1800-2199)
	if century != 'I' && century != 'J' && century != 'K' && century != 'L' {
		return "", fmt.Errorf("%w: invalid century code for asteroid provisional: %c (must be I-L for years 1800-2199)", ErrInvalidFormat, century)
	}

	orderNum, err := decodeCycleCount(orderEncoded)
	if err != nil {
		return "", err
	}

	fullYear := fmt.Sprintf("%d%s", centuryVal, year)
	yearNum, _ := strconv.Atoi(fullYear)

	// For pre-1925 designations, use A-prefix format (MPC canonical)
	// A-prefix: A=1 for 1xxx years, B=2 for 2xxx years (theoretical)
	if yearNum < 1925 {
		firstDigit := fullYear[0]
		restOfYear := fullYear[1:]
		var prefix string
		if firstDigit == '1' {
			prefix = "A"
		} else if firstDigit == '2' {
			prefix = "B"
		}
		if prefix != "" {
			if orderNum == 0 {
				return fmt.Sprintf("%s%s %c%c", prefix, restOfYear, halfMonth, secondLetter), nil
			}
			return fmt.Sprintf("%s%s %c%c%d", prefix, restOfYear, halfMonth, secondLetter, orderNum), nil
		}
	}

	if orderNum == 0 {
		return fmt.Sprintf("%s %c%c", fullYear, halfMonth, secondLetter), nil
	}
	return fmt.Sprintf("%s %c%c%d", fullYear, halfMonth, secondLetter, orderNum), nil
}

// PackProvisional packs a provisional asteroid designation
func PackProvisional(unpacked string) (string, error) {
	u := strings.TrimSpace(unpacked)

	// Check for survey designations
	parts := strings.Split(u, " ")
	if len(parts) == 2 {
		if code, ok := surveyUnpackedToPacked[parts[1]]; ok {
			num, err := strconv.Atoi(parts[0])
			if err != nil {
				return "", fmt.Errorf("%w: invalid survey designation", ErrInvalidFormat)
			}
			if num < 1 {
				return "", fmt.Errorf("%w: survey number must be positive", ErrOutOfRange)
			}
			return fmt.Sprintf("%s%04d", code, num), nil
		}
	}

	// Check for old-style designation: "A908 CJ" or "B842 FA"
	if len(u) == 7 && (u[0] == 'A' || u[0] == 'B') && u[4] == ' ' {
		centuryDigit := u[1]
		yearShort := u[2:4]
		halfMonth := u[5]
		secondLetter := u[6]

		var centuryCode byte
		switch centuryDigit {
		case '8':
			centuryCode = 'I'
		case '9':
			centuryCode = 'J'
		case '0':
			centuryCode = 'K'
		default:
			return "", fmt.Errorf("%w: invalid century digit in old-style designation", ErrInvalidFormat)
		}

		return fmt.Sprintf("%c%s%c00%c", centuryCode, yearShort, halfMonth, secondLetter), nil
	}

	// Match standard provisional: "1995 XA" or "1995 XA12"
	if len(u) < 7 || u[4] != ' ' {
		return "", fmt.Errorf("%w: invalid unpacked provisional designation", ErrInvalidFormat)
	}

	year := u[0:4]
	halfMonth := u[5]
	secondLetter := u[6]

	if !isValidHalfMonth(halfMonth) {
		return "", fmt.Errorf("%w: invalid half-month letter: %c", ErrInvalidFormat, halfMonth)
	}

	orderNum := 0
	if len(u) > 7 {
		var err error
		orderNum, err = strconv.Atoi(u[7:])
		if err != nil {
			return "", fmt.Errorf("%w: invalid order number", ErrInvalidFormat)
		}
	}

	yearInt, err := strconv.Atoi(year)
	if err != nil {
		return "", fmt.Errorf("%w: invalid year", ErrInvalidFormat)
	}

	// Asteroid provisionals: only years 1800-2199 valid
	if yearInt < 1800 || yearInt > 2199 {
		return "", fmt.Errorf("%w: year out of range for asteroid provisional: %s (must be 1800-2199)", ErrOutOfRange, year)
	}

	century := yearInt / 100
	yearShort := year[2:4]

	centuryCode, ok := reverseCenturyCodes[century]
	if !ok {
		return "", fmt.Errorf("%w: invalid century in year: %s", ErrInvalidFormat, year)
	}

	// Check for extended format
	if orderNum >= 620 {
		return packExtendedProvisional(yearInt, halfMonth, secondLetter, orderNum)
	}

	orderEncoded, err := encodeCycleCount(orderNum)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%c%s%c%s%c", centuryCode, yearShort, halfMonth, orderEncoded, secondLetter), nil
}

// packExtendedProvisional packs a provisional with cycle >= 620
func packExtendedProvisional(year int, halfMonth, secondLetter byte, cycle int) (string, error) {
	yearShort := year % 100

	letterPos, err := letterToPosition(secondLetter)
	if err != nil {
		return "", err
	}
	baseSequence := (cycle-620)*25 + letterPos - 1

	seqEncoded, err := numToBase62String(baseSequence, 4)
	if err != nil {
		return "", err
	}

	yearChar, err := numToBase62(yearShort)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("_%c%c%s", yearChar, halfMonth, seqEncoded), nil
}

// UnpackExtendedProvisional unpacks an extended provisional (underscore format)
func UnpackExtendedProvisional(packed string) (string, error) {
	p := strings.TrimSpace(packed)
	if len(p) != 7 || p[0] != '_' {
		return "", fmt.Errorf("%w: invalid extended packed provisional", ErrInvalidFormat)
	}

	yearDigit := p[1]
	halfMonth := p[2]
	seqEncoded := p[3:7]

	baseSequence, err := base62StringToNum(seqEncoded)
	if err != nil {
		return "", err
	}

	cycle := 620 + baseSequence/25
	letterPos := (baseSequence % 25) + 1
	secondLetter, err := positionToLetter(letterPos)
	if err != nil {
		return "", err
	}

	yearVal, err := base62ToNum(yearDigit)
	if err != nil {
		return "", err
	}

	// Extended format is for years 2000-2099, year code is year % 100
	year := 2000 + yearVal

	return fmt.Sprintf("%d %c%c%d", year, halfMonth, secondLetter, cycle), nil
}

// UnpackCometProvisional unpacks a comet provisional designation
func UnpackCometProvisional(packed string) (string, error) {
	p := strings.TrimSpace(packed)
	length := len(p)

	if length != 7 && length != 8 {
		return "", fmt.Errorf("%w: invalid packed comet provisional designation length", ErrInvalidFormat)
	}

	century := p[0]
	year := p[1:3]
	halfMonth := p[3]
	orderEncoded := p[4:6]

	var fragment string
	if length == 7 {
		fragment = string(p[6])
	} else {
		fragment = p[6:8]
	}

	centuryVal, ok := centuryCodes[century]
	if !ok {
		return "", fmt.Errorf("%w: invalid century code: %c", ErrInvalidFormat, century)
	}

	fullYear := fmt.Sprintf("%d%s", centuryVal, year)
	orderNum, err := decodeCycleCount(orderEncoded)
	if err != nil {
		return "", err
	}

	result := fmt.Sprintf("%s %c%d", fullYear, halfMonth, orderNum)
	if fragment != "0" {
		result += "-" + strings.ToUpper(fragment)
	}

	return result, nil
}

// PackCometProvisional packs a comet provisional designation
func PackCometProvisional(unpacked string) (string, error) {
	u := strings.TrimSpace(unpacked)

	// Parse: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
	spaceIdx := strings.Index(u, " ")
	if spaceIdx != 4 {
		return "", fmt.Errorf("%w: invalid unpacked comet provisional designation", ErrInvalidFormat)
	}

	year := u[0:4]
	rest := u[5:]

	// Extract half-month, order, and optional fragment
	halfMonth := rest[0]
	rest = rest[1:]

	var orderStr, fragment string
	dashIdx := strings.Index(rest, "-")
	if dashIdx >= 0 {
		orderStr = rest[0:dashIdx]
		fragment = rest[dashIdx+1:]
	} else {
		orderStr = rest
		fragment = ""
	}

	orderNum, err := strconv.Atoi(orderStr)
	if err != nil {
		return "", fmt.Errorf("%w: invalid order number in comet designation", ErrInvalidFormat)
	}

	if orderNum < 1 {
		return "", fmt.Errorf("%w: comet order number must be positive", ErrOutOfRange)
	}

	yearInt, err := strconv.Atoi(year)
	if err != nil {
		return "", fmt.Errorf("%w: invalid year in comet designation", ErrInvalidFormat)
	}

	century := yearInt / 100
	yearShort := year[2:4]

	centuryCode, ok := reverseCenturyCodes[century]
	if !ok {
		return "", fmt.Errorf("%w: invalid century in year: %s", ErrInvalidFormat, year)
	}

	orderEncoded, err := encodeCycleCount(orderNum)
	if err != nil {
		return "", err
	}

	fragmentCode := "0"
	if fragment != "" {
		// Validate fragment: must be 1-2 uppercase letters
		if len(fragment) > 2 {
			return "", fmt.Errorf("%w: fragment too long (max 2 characters)", ErrInvalidFormat)
		}
		for i := 0; i < len(fragment); i++ {
			if fragment[i] < 'A' || fragment[i] > 'Z' {
				return "", fmt.Errorf("%w: invalid fragment (must be letters)", ErrInvalidFormat)
			}
		}
		fragmentCode = strings.ToLower(fragment)
	}

	return fmt.Sprintf("%c%s%c%s%s", centuryCode, yearShort, halfMonth, orderEncoded, fragmentCode), nil
}

// UnpackCometNumbered unpacks a numbered periodic comet designation
// Supports fragments: 0073Pa -> 73P-A, 0073Paa -> 73P-AA
func UnpackCometNumbered(packed string) (string, error) {
	p := strings.TrimSpace(packed)
	length := len(p)

	if length < 5 || length > 7 {
		return "", fmt.Errorf("%w: invalid packed numbered comet designation", ErrInvalidFormat)
	}

	cometType := p[4]
	if cometType != 'P' && cometType != 'D' {
		return "", fmt.Errorf("%w: invalid comet type: %c", ErrInvalidFormat, cometType)
	}

	num, err := strconv.Atoi(p[0:4])
	if err != nil {
		return "", fmt.Errorf("%w: invalid packed numbered comet designation", ErrInvalidFormat)
	}

	result := fmt.Sprintf("%d%c", num, cometType)

	// Check for fragment (lowercase letters after comet type)
	if length > 5 {
		fragment := p[5:]
		// Validate fragment: must be 1-2 lowercase letters
		for i := 0; i < len(fragment); i++ {
			if fragment[i] < 'a' || fragment[i] > 'z' {
				return "", fmt.Errorf("%w: invalid fragment in packed comet designation", ErrInvalidFormat)
			}
		}
		result += "-" + strings.ToUpper(fragment)
	}

	return result, nil
}

// PackCometNumbered packs a numbered periodic comet designation
// Supports fragments: 73P-A -> 0073Pa, 73P-AA -> 0073Paa
func PackCometNumbered(unpacked string) (string, error) {
	u := strings.TrimSpace(unpacked)

	// Find where the number ends
	numEnd := 0
	for i := 0; i < len(u); i++ {
		if u[i] < '0' || u[i] > '9' {
			numEnd = i
			break
		}
	}

	if numEnd == 0 {
		return "", fmt.Errorf("%w: invalid unpacked numbered comet designation", ErrInvalidFormat)
	}

	num, err := strconv.Atoi(u[0:numEnd])
	if err != nil {
		return "", fmt.Errorf("%w: invalid unpacked numbered comet designation", ErrInvalidFormat)
	}

	cometType := u[numEnd]
	if cometType != 'P' && cometType != 'D' {
		return "", fmt.Errorf("%w: invalid comet type: %c", ErrInvalidFormat, cometType)
	}

	if num < 1 || num > 9999 {
		return "", fmt.Errorf("%w: comet number out of range (1-9999): %d", ErrOutOfRange, num)
	}

	result := fmt.Sprintf("%04d%c", num, cometType)

	// Check for fragment suffix: "-A" or "-AA"
	rest := u[numEnd+1:]
	if strings.HasPrefix(rest, "-") {
		fragment := rest[1:]
		// Check for /name suffix and strip it
		slashIdx := strings.Index(fragment, "/")
		if slashIdx >= 0 {
			fragment = fragment[0:slashIdx]
		}

		// Validate fragment: must be 1-2 uppercase letters
		if len(fragment) < 1 || len(fragment) > 2 {
			return "", fmt.Errorf("%w: invalid fragment length (1-2 letters)", ErrInvalidFormat)
		}
		for i := 0; i < len(fragment); i++ {
			if fragment[i] < 'A' || fragment[i] > 'Z' {
				return "", fmt.Errorf("%w: invalid fragment (must be uppercase letters)", ErrInvalidFormat)
			}
		}
		result += strings.ToLower(fragment)
	}

	return result, nil
}

// UnpackSatellite unpacks a natural satellite provisional designation
func UnpackSatellite(packed string) (string, error) {
	p := strings.TrimSpace(packed)
	if len(p) != 8 || p[0] != 'S' {
		return "", fmt.Errorf("%w: invalid packed satellite designation", ErrInvalidFormat)
	}

	century := p[1]
	year := p[2:4]
	planet := p[4]
	numberEncoded := p[5:7]

	centuryVal, ok := centuryCodes[century]
	if !ok {
		return "", fmt.Errorf("%w: invalid century code: %c", ErrInvalidFormat, century)
	}

	if !satellitePlanets[planet] {
		return "", fmt.Errorf("%w: invalid planet code: %c", ErrInvalidFormat, planet)
	}

	fullYear := fmt.Sprintf("%d%s", centuryVal, year)
	number, err := decodeCycleCount(numberEncoded)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("S/%s %c %d", fullYear, planet, number), nil
}

// PackSatellite packs a natural satellite provisional designation
func PackSatellite(unpacked string) (string, error) {
	u := strings.TrimSpace(unpacked)

	// Parse: "S/2019 S 22" or "S/2023 S 1"
	if len(u) < 10 || u[0:2] != "S/" || u[6] != ' ' || u[8] != ' ' {
		return "", fmt.Errorf("%w: invalid unpacked satellite designation", ErrInvalidFormat)
	}

	year := u[2:6]
	planet := u[7]
	numberStr := u[9:]

	if !satellitePlanets[planet] {
		return "", fmt.Errorf("%w: invalid planet code: %c", ErrInvalidFormat, planet)
	}

	number, err := strconv.Atoi(numberStr)
	if err != nil {
		return "", fmt.Errorf("%w: invalid satellite number", ErrInvalidFormat)
	}

	if number < 1 {
		return "", fmt.Errorf("%w: satellite number must be positive", ErrOutOfRange)
	}

	yearInt, err := strconv.Atoi(year)
	if err != nil {
		return "", fmt.Errorf("%w: invalid year", ErrInvalidFormat)
	}

	century := yearInt / 100
	yearShort := year[2:4]

	centuryCode, ok := reverseCenturyCodes[century]
	if !ok {
		return "", fmt.Errorf("%w: invalid century in year: %s", ErrInvalidFormat, year)
	}

	numberEncoded, err := encodeCycleCount(number)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("S%c%s%c%s0", centuryCode, yearShort, planet, numberEncoded), nil
}

// isAsteroidStylePacked checks if 7-char provisional uses asteroid-style format
func isAsteroidStylePacked(p string) bool {
	if len(p) != 7 {
		return false
	}
	last := p[6]
	return last >= 'A' && last <= 'Z'
}

// isAsteroidStyleUnpacked checks if unpacked provisional uses asteroid-style format
func isAsteroidStyleUnpacked(p string) bool {
	if len(p) < 7 || p[4] != ' ' {
		return false
	}
	if len(p) > 5 {
		c := p[6]
		return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
	}
	return false
}

// UnpackCometFull unpacks a full comet designation
func UnpackCometFull(packed string) (string, error) {
	p := packed
	length := len(p)

	if length == 8 {
		// Compact 8-char format: type + 7-char provisional
		cometType := p[0]
		provisionalPart := p[1:]

		if !cometTypes[cometType] {
			return "", fmt.Errorf("%w: invalid comet type: %c", ErrInvalidFormat, cometType)
		}

		var provisional string
		var err error
		if isAsteroidStylePacked(provisionalPart) {
			provisional, err = UnpackProvisional(provisionalPart)
		} else {
			provisional, err = UnpackCometProvisional(provisionalPart)
		}
		if err != nil {
			return "", err
		}

		return fmt.Sprintf("%c/%s", cometType, provisional), nil
	}

	if length == 9 {
		// Compact 9-char format with 2-letter fragment
		cometType := p[0]
		provisionalPart := p[1:]

		if !cometTypes[cometType] {
			return "", fmt.Errorf("%w: invalid comet type: %c", ErrInvalidFormat, cometType)
		}

		provisional, err := UnpackCometProvisional(provisionalPart)
		if err != nil {
			return "", err
		}

		return fmt.Sprintf("%c/%s", cometType, provisional), nil
	}

	if length == 12 || (length < 12 && p[0] == ' ') {
		// Full 12-char format
		for len(p) < 12 {
			p = " " + p
		}

		numPart := strings.TrimSpace(p[0:4])
		cometType := p[4]
		provisionalPart := p[5:12]

		if !cometTypes[cometType] {
			return "", fmt.Errorf("%w: invalid comet type: %c", ErrInvalidFormat, cometType)
		}

		var provisional string
		var err error
		if isAsteroidStylePacked(provisionalPart) {
			provisional, err = UnpackProvisional(provisionalPart)
		} else {
			provisional, err = UnpackCometProvisional(provisionalPart)
		}
		if err != nil {
			return "", err
		}

		if numPart == "" {
			return fmt.Sprintf("%c/%s", cometType, provisional), nil
		}

		num, err := strconv.Atoi(numPart)
		if err != nil {
			return "", fmt.Errorf("%w: invalid comet number", ErrInvalidFormat)
		}

		return fmt.Sprintf("%d%c/%s", num, cometType, provisional), nil
	}

	return "", fmt.Errorf("%w: invalid packed full comet designation length", ErrInvalidFormat)
}

// PackCometFull packs a full comet designation
func PackCometFull(unpacked string) (string, error) {
	u := strings.TrimSpace(unpacked)

	// Parse: optional number, type, slash, year, provisional
	slashIdx := strings.Index(u, "/")
	if slashIdx < 1 {
		return "", fmt.Errorf("%w: invalid unpacked comet designation", ErrInvalidFormat)
	}

	prefix := u[0:slashIdx]
	rest := u[slashIdx+1:]

	// Find number and type
	var number string
	var cometType byte

	for i := len(prefix) - 1; i >= 0; i-- {
		if prefix[i] >= 'A' && prefix[i] <= 'Z' {
			cometType = prefix[i]
			number = prefix[0:i]
			break
		}
	}

	if cometType == 0 || !cometTypes[cometType] {
		return "", fmt.Errorf("%w: invalid comet type", ErrInvalidFormat)
	}

	// Parse year and provisional part
	spaceIdx := strings.Index(rest, " ")
	if spaceIdx < 1 {
		return "", fmt.Errorf("%w: invalid unpacked comet designation", ErrInvalidFormat)
	}

	yearStr := rest[0:spaceIdx]
	provPart := rest[spaceIdx+1:]

	year, err := strconv.Atoi(yearStr)
	if err != nil {
		return "", fmt.Errorf("%w: invalid year in comet designation", ErrInvalidFormat)
	}

	// Check for ancient or BCE year
	if year < 1000 {
		return packAncientCometProvisional(cometType, year, provPart)
	}

	// Modern comet - reconstruct provisional with year
	provisional := fmt.Sprintf("%d %s", year, provPart)

	var provisionalPacked string
	if isAsteroidStyleUnpacked(provisional) {
		provisionalPacked, err = PackProvisional(provisional)
	} else {
		provisionalPacked, err = PackCometProvisional(provisional)
	}
	if err != nil {
		return "", err
	}

	if number == "" {
		return fmt.Sprintf("%c%s", cometType, provisionalPacked), nil
	}

	num, err := strconv.Atoi(number)
	if err != nil || num < 1 || num > 9999 {
		return "", fmt.Errorf("%w: comet number out of range (1-9999)", ErrOutOfRange)
	}

	return fmt.Sprintf("%04d%c%s", num, cometType, provisionalPacked), nil
}

// packAncientCometProvisional packs ancient or BCE comet provisional
func packAncientCometProvisional(cometType byte, year int, provPart string) (string, error) {
	// Parse provPart: "V1" or "V1-A"
	var halfMonth byte
	var orderStr, fragment string

	halfMonth = provPart[0]
	rest := provPart[1:]

	dashIdx := strings.Index(rest, "-")
	if dashIdx >= 0 {
		orderStr = rest[0:dashIdx]
		fragment = rest[dashIdx+1:]
	} else {
		orderStr = rest
		fragment = ""
	}

	orderNum, err := strconv.Atoi(orderStr)
	if err != nil {
		return "", fmt.Errorf("%w: invalid order number", ErrInvalidFormat)
	}

	orderEncoded, err := encodeCycleCount(orderNum)
	if err != nil {
		return "", err
	}

	fragmentCode := "0"
	if fragment != "" {
		// Validate fragment: must be 1-2 uppercase letters
		if len(fragment) > 2 {
			return "", fmt.Errorf("%w: fragment too long (max 2 characters)", ErrInvalidFormat)
		}
		for i := 0; i < len(fragment); i++ {
			if fragment[i] < 'A' || fragment[i] > 'Z' {
				return "", fmt.Errorf("%w: invalid fragment (must be letters)", ErrInvalidFormat)
			}
		}
		fragmentCode = strings.ToLower(fragment)
	}

	if year < 0 {
		// BCE year
		prefix, code, err := encodeBCEYear(year)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%c%s%s%c%s%s", cometType, prefix, code, halfMonth, orderEncoded, fragmentCode), nil
	}

	// Ancient year (1-999)
	return fmt.Sprintf("%c%03d%c%s%s", cometType, year, halfMonth, orderEncoded, fragmentCode), nil
}

// encodeBCEYear encodes a BCE year
func encodeBCEYear(year int) (string, string, error) {
	if year >= 0 {
		return "", "", fmt.Errorf("%w: not a BCE year", ErrInvalidFormat)
	}

	absYear := -year
	code := 99 - (absYear % 100)

	if absYear < 100 {
		return "/", fmt.Sprintf("%02d", code), nil
	} else if absYear < 200 {
		return ".", fmt.Sprintf("%02d", code), nil
	} else if absYear < 300 {
		return "-", fmt.Sprintf("%02d", code), nil
	}

	return "", "", fmt.Errorf("%w: BCE year out of supported range", ErrOutOfRange)
}

// decodeBCEYear decodes a BCE year from packed format
func decodeBCEYear(prefix byte, code string) (int, error) {
	codeNum, err := strconv.Atoi(code)
	if err != nil {
		return 0, fmt.Errorf("%w: invalid BCE year code", ErrInvalidFormat)
	}

	yearPart := 99 - codeNum

	switch prefix {
	case '/':
		return -yearPart, nil
	case '.':
		return -(yearPart + 100), nil
	case '-':
		return -(yearPart + 200), nil
	default:
		return 0, fmt.Errorf("%w: invalid BCE prefix", ErrInvalidFormat)
	}
}

// UnpackAncientCometProvisional unpacks ancient/BCE comet provisional
func UnpackAncientCometProvisional(packed string) (string, error) {
	p := strings.TrimSpace(packed)
	if len(p) != 8 {
		return "", fmt.Errorf("%w: invalid ancient comet designation length", ErrInvalidFormat)
	}

	cometType := p[0]
	if !cometTypes[cometType] {
		return "", fmt.Errorf("%w: invalid comet type: %c", ErrInvalidFormat, cometType)
	}

	var year int
	var halfMonth byte
	var orderEncoded string
	var fragment byte

	// Check for BCE prefix
	if p[1] == '/' || p[1] == '.' || p[1] == '-' {
		var err error
		year, err = decodeBCEYear(p[1], p[2:4])
		if err != nil {
			return "", err
		}
		halfMonth = p[4]
		orderEncoded = p[5:7]
		fragment = p[7]
	} else {
		// Ancient year (3 digits)
		var err error
		year, err = strconv.Atoi(p[1:4])
		if err != nil {
			return "", fmt.Errorf("%w: invalid ancient year", ErrInvalidFormat)
		}
		halfMonth = p[4]
		orderEncoded = p[5:7]
		fragment = p[7]
	}

	orderNum, err := decodeCycleCount(orderEncoded)
	if err != nil {
		return "", err
	}

	result := fmt.Sprintf("%c/%d %c%d", cometType, year, halfMonth, orderNum)
	if fragment != '0' {
		result += "-" + strings.ToUpper(string(fragment))
	}

	return result, nil
}

// DetectFormat detects if a designation is packed or unpacked and what type it is
func DetectFormat(designation string) (Info, error) {
	var info Info

	// Validate raw input BEFORE trimming
	if err := validateRawInput(designation); err != nil {
		return info, err
	}

	// Check for packed full comet designation BEFORE trimming (12 chars with spaces)
	if len(designation) == 12 {
		if isPackedCometFull12(designation) {
			info.Format = FormatPacked
			info.Type = "comet_full"
			info.Subtype = "comet with provisional designation (12-char)"
			return info, nil
		}
	}

	// Check for packed comet designation (8 chars)
	if len(designation) == 8 && cometTypes[designation[0]] {
		if isPackedCometFull8(designation) {
			info.Format = FormatPacked
			info.Type = "comet_full"
			info.Subtype = "comet with provisional designation (8-char)"
			return info, nil
		}
	}

	// Check for packed comet with 2-letter fragment (9 chars)
	if len(designation) == 9 && cometTypes[designation[0]] {
		if isPackedCometFull9(designation) {
			info.Format = FormatPacked
			info.Type = "comet_full"
			info.Subtype = "comet with provisional designation (9-char, 2-letter fragment)"
			return info, nil
		}
	}

	// Check for packed ancient comet (8 chars)
	if len(designation) == 8 && cometTypes[designation[0]] {
		if isPackedAncientComet(designation) {
			info.Format = FormatPacked
			info.Type = "comet_ancient"
			info.Subtype = "comet with ancient provisional (year < 1000)"
			return info, nil
		}
	}

	// Check for packed BCE comet (8 chars)
	if len(designation) == 8 && cometTypes[designation[0]] {
		if isPackedBCEComet(designation) {
			info.Format = FormatPacked
			info.Type = "comet_bce"
			info.Subtype = "comet with BCE provisional"
			return info, nil
		}
	}

	des := strings.TrimSpace(designation)

	// Validate whitespace
	if err := validateWhitespace(des); err != nil {
		return info, err
	}

	// Check for packed satellite designation (8 chars starting with S)
	if len(des) == 8 && des[0] == 'S' {
		if isPackedSatellite(des) {
			info.Format = FormatPacked
			info.Type = "satellite"
			planet := des[4]
			planetName := satellitePlanetNames[planet]
			if planetName == "" {
				planetName = string(planet)
			}
			info.Subtype = fmt.Sprintf("natural satellite (%s)", planetName)
			return info, nil
		}
	}

	// Check for packed permanent (numbered) asteroid
	if len(des) == 5 {
		if des[0] == '~' {
			if isPackedTilde(des) {
				info.Format = FormatPacked
				info.Type = "permanent"
				info.Subtype = "permanent numbered (tilde/base-62, >= 620000)"
				return info, nil
			}
		} else if isAllDigits(des) {
			info.Format = FormatPacked
			info.Type = "permanent"
			info.Subtype = "permanent numbered (5-digit, < 100000)"
			return info, nil
		} else if isPackedLetterPrefix(des) {
			info.Format = FormatPacked
			info.Type = "permanent"
			if des[0] >= 'A' && des[0] <= 'Z' {
				info.Subtype = "permanent numbered (letter-prefix, 100000-359999)"
			} else {
				info.Subtype = "permanent numbered (letter-prefix, 360000-619999)"
			}
			return info, nil
		}

		// Check for packed numbered comet (5 chars ending in P or D)
		if isPackedCometNumbered(des) {
			info.Format = FormatPacked
			info.Type = "comet_numbered"
			cometType := des[4]
			typeDesc := cometTypeDescriptions[cometType]
			info.Subtype = fmt.Sprintf("comet numbered %s", typeDesc)
			return info, nil
		}
	}

	// Check for packed numbered comet with fragment (6-7 chars: ####Pa or ####Paa)
	if len(des) == 6 || len(des) == 7 {
		if isPackedCometNumbered(des) {
			info.Format = FormatPacked
			info.Type = "comet_numbered"
			cometType := des[4]
			typeDesc := cometTypeDescriptions[cometType]
			fragLen := len(des) - 5
			if fragLen == 1 {
				info.Subtype = fmt.Sprintf("comet numbered %s with fragment", typeDesc)
			} else {
				info.Subtype = fmt.Sprintf("comet numbered %s with 2-letter fragment", typeDesc)
			}
			return info, nil
		}
	}

	// Check for packed provisional asteroid (7 chars)
	if len(des) == 7 {
		// Extended format with underscore
		if des[0] == '_' && isPackedExtended(des) {
			info.Format = FormatPacked
			info.Type = "provisional_extended"
			info.Subtype = "provisional (extended format, cycle >=620)"
			return info, nil
		}

		// Standard provisional
		if isPackedProvisional(des) {
			info.Format = FormatPacked
			info.Type = "provisional"
			info.Subtype = "provisional"
			return info, nil
		}

		// Survey designations
		if strings.HasPrefix(des, "PLS") && isAllDigits(des[3:]) {
			info.Format = FormatPacked
			info.Type = "survey"
			info.Subtype = "survey (Palomar-Leiden)"
			return info, nil
		}

		if isPackedSurveyT(des) {
			info.Format = FormatPacked
			info.Type = "survey"
			info.Subtype = fmt.Sprintf("survey (Trojan T-%c)", des[1])
			return info, nil
		}

		// Check for packed comet provisional (7 chars)
		if isPackedCometProv(des) {
			info.Format = FormatPacked
			info.Type = "comet_provisional"
			info.Subtype = "comet provisional"
			return info, nil
		}
	}

	// --- UNPACKED FORMATS ---

	// Check for unpacked satellite
	if strings.HasPrefix(des, "S/") && isUnpackedSatellite(des) {
		info.Format = FormatUnpacked
		info.Type = "satellite"
		planet := des[7]
		planetName := satellitePlanetNames[planet]
		if planetName == "" {
			planetName = string(planet)
		}
		info.Subtype = fmt.Sprintf("natural satellite (%s)", planetName)
		return info, nil
	}

	// Check for unpacked permanent (numbered) asteroid
	if isAllDigits(des) && len(des) > 0 {
		info.Format = FormatUnpacked
		info.Type = "permanent"
		info.Subtype = "permanent numbered"
		return info, nil
	}

	// Check for unpacked survey designation
	if isUnpackedSurvey(des) {
		info.Format = FormatUnpacked
		info.Type = "survey"
		if strings.HasSuffix(des, "P-L") {
			info.Subtype = "survey (Palomar-Leiden)"
		} else {
			parts := strings.Split(des, " ")
			info.Subtype = fmt.Sprintf("survey (Trojan %s)", parts[1])
		}
		return info, nil
	}

	// Check for old-style asteroid designation
	if isUnpackedOldStyle(des) {
		info.Format = FormatUnpacked
		info.Type = "provisional"
		info.Subtype = "provisional (old-style pre-1925)"
		return info, nil
	}

	// Check for unpacked provisional asteroid
	if isUnpackedProvisional(des) {
		info.Format = FormatUnpacked
		info.Type = "provisional"
		info.Subtype = "provisional"
		return info, nil
	}

	// Check for unpacked comet with type prefix
	if cometType, year, ok := parseUnpackedCometFull(des); ok {
		info.Format = FormatUnpacked
		info.Type = "comet_full"
		typeDesc := cometTypeDescriptions[cometType]

		var yearDesc string
		if year < 0 {
			yearDesc = "BCE"
		} else if year < 1000 {
			yearDesc = "ancient"
		}

		if yearDesc != "" {
			info.Subtype = fmt.Sprintf("comet %s provisional (%s)", yearDesc, typeDesc)
		} else {
			info.Subtype = fmt.Sprintf("comet provisional (%s)", typeDesc)
		}
		return info, nil
	}

	// Check for unpacked numbered periodic comet
	if isUnpackedCometNumbered(des) {
		info.Format = FormatUnpacked
		info.Type = "comet_numbered"
		// Find the comet type letter
		for i := 0; i < len(des); i++ {
			if des[i] == 'P' || des[i] == 'D' {
				typeDesc := cometTypeDescriptions[des[i]]
				info.Subtype = fmt.Sprintf("comet numbered %s", typeDesc)
				break
			}
		}
		return info, nil
	}

	return info, fmt.Errorf("%w: unable to detect designation format", ErrInvalidFormat)
}

// Helper functions for format detection (using direct character checks instead of regex)

func isAllDigits(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return len(s) > 0
}

func isPackedCometFull12(s string) bool {
	// Pattern: ^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$
	if len(s) != 12 {
		return false
	}
	for i := 0; i < 4; i++ {
		if s[i] != ' ' && (s[i] < '0' || s[i] > '9') {
			return false
		}
	}
	if !cometTypes[s[4]] {
		return false
	}
	// Check provisional part
	if s[5] < 'I' || s[5] > 'L' {
		return false
	}
	if s[6] < '0' || s[6] > '9' || s[7] < '0' || s[7] > '9' {
		return false
	}
	if s[8] < 'A' || s[8] > 'Z' {
		return false
	}
	if !isBase62(s[9]) || !isBase62(s[10]) {
		return false
	}
	if !isDigitOrLower(s[11]) {
		return false
	}
	return true
}

func isPackedCometFull8(s string) bool {
	if len(s) != 8 || !cometTypes[s[0]] {
		return false
	}
	if s[1] < 'A' || s[1] > 'L' {
		return false
	}
	if s[2] < '0' || s[2] > '9' || s[3] < '0' || s[3] > '9' {
		return false
	}
	if s[4] < 'A' || s[4] > 'Z' {
		return false
	}
	if !isBase62(s[5]) || !isBase62(s[6]) || !isBase62(s[7]) {
		return false
	}
	return true
}

func isPackedCometFull9(s string) bool {
	if len(s) != 9 || !cometTypes[s[0]] {
		return false
	}
	if s[1] < 'A' || s[1] > 'L' {
		return false
	}
	if s[2] < '0' || s[2] > '9' || s[3] < '0' || s[3] > '9' {
		return false
	}
	if s[4] < 'A' || s[4] > 'Z' {
		return false
	}
	if !isBase62(s[5]) || !isBase62(s[6]) {
		return false
	}
	if s[7] < 'a' || s[7] > 'z' || s[8] < 'a' || s[8] > 'z' {
		return false
	}
	return true
}

func isPackedAncientComet(s string) bool {
	if len(s) != 8 || !cometTypes[s[0]] {
		return false
	}
	if s[1] < '0' || s[1] > '9' || s[2] < '0' || s[2] > '9' || s[3] < '0' || s[3] > '9' {
		return false
	}
	if s[4] < 'A' || s[4] > 'Z' {
		return false
	}
	if !isBase62(s[5]) || !isBase62(s[6]) {
		return false
	}
	if !isDigitOrLower(s[7]) {
		return false
	}
	return true
}

func isPackedBCEComet(s string) bool {
	if len(s) != 8 || !cometTypes[s[0]] {
		return false
	}
	if s[1] != '/' && s[1] != '.' && s[1] != '-' {
		return false
	}
	if s[2] < '0' || s[2] > '9' || s[3] < '0' || s[3] > '9' {
		return false
	}
	if s[4] < 'A' || s[4] > 'Z' {
		return false
	}
	if !isBase62(s[5]) || !isBase62(s[6]) {
		return false
	}
	if !isDigitOrLower(s[7]) {
		return false
	}
	return true
}

func isPackedSatellite(s string) bool {
	if len(s) != 8 || s[0] != 'S' {
		return false
	}
	if s[1] < 'A' || s[1] > 'L' {
		return false
	}
	if s[2] < '0' || s[2] > '9' || s[3] < '0' || s[3] > '9' {
		return false
	}
	if !satellitePlanets[s[4]] {
		return false
	}
	if !isBase62(s[5]) || !isBase62(s[6]) {
		return false
	}
	if s[7] != '0' {
		return false
	}
	return true
}

func isPackedTilde(s string) bool {
	if len(s) != 5 || s[0] != '~' {
		return false
	}
	for i := 1; i < 5; i++ {
		if !isBase62(s[i]) {
			return false
		}
	}
	return true
}

func isPackedLetterPrefix(s string) bool {
	if len(s) != 5 {
		return false
	}
	if !unicode.IsLetter(rune(s[0])) {
		return false
	}
	for i := 1; i < 5; i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

func isPackedExtended(s string) bool {
	if len(s) != 7 || s[0] != '_' {
		return false
	}
	// Year code: digit (0-9 for 2000-2009) or letter (A=10 for 2010, etc.)
	c := s[1]
	if !((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
		return false
	}
	if s[2] < 'A' || s[2] > 'Z' {
		return false
	}
	for i := 3; i < 7; i++ {
		if !isBase62(s[i]) {
			return false
		}
	}
	return true
}

func isPackedProvisional(s string) bool {
	if len(s) != 7 {
		return false
	}
	if s[0] < 'A' || s[0] > 'L' {
		return false
	}
	if s[1] < '0' || s[1] > '9' || s[2] < '0' || s[2] > '9' {
		return false
	}
	if s[3] < 'A' || s[3] > 'Z' {
		return false
	}
	if !isBase62(s[4]) || !isBase62(s[5]) {
		return false
	}
	if s[6] < 'A' || s[6] > 'Z' {
		return false
	}
	return true
}

func isPackedSurveyT(s string) bool {
	if len(s) != 7 || s[0] != 'T' {
		return false
	}
	if s[1] < '1' || s[1] > '3' {
		return false
	}
	if s[2] != 'S' {
		return false
	}
	for i := 3; i < 7; i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

func isPackedCometNumbered(s string) bool {
	length := len(s)
	if length < 5 || length > 7 {
		return false
	}
	for i := 0; i < 4; i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	if s[4] != 'P' && s[4] != 'D' {
		return false
	}
	// Check for optional fragment (lowercase letters)
	for i := 5; i < length; i++ {
		if s[i] < 'a' || s[i] > 'z' {
			return false
		}
	}
	return true
}

func isPackedCometProv(s string) bool {
	if len(s) != 7 {
		return false
	}
	if s[0] < 'I' || s[0] > 'L' {
		return false
	}
	if s[1] < '0' || s[1] > '9' || s[2] < '0' || s[2] > '9' {
		return false
	}
	if s[3] < 'A' || s[3] > 'Z' {
		return false
	}
	if !isBase62(s[4]) || !isBase62(s[5]) {
		return false
	}
	if !isDigitOrLower(s[6]) {
		return false
	}
	return true
}

func isUnpackedSatellite(s string) bool {
	// Minimum: "S/YYYY P N" = 10 chars
	if len(s) < 10 || s[0:2] != "S/" {
		return false
	}
	for i := 2; i < 6; i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	if s[6] != ' ' {
		return false
	}
	if !satellitePlanets[s[7]] {
		return false
	}
	if s[8] != ' ' {
		return false
	}
	for i := 9; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return len(s) >= 10 && len(s[9:]) > 0 // Must have at least one digit for number
}

func isUnpackedSurvey(s string) bool {
	parts := strings.Split(s, " ")
	if len(parts) != 2 {
		return false
	}
	if !isAllDigits(parts[0]) {
		return false
	}
	return parts[1] == "P-L" || parts[1] == "T-1" || parts[1] == "T-2" || parts[1] == "T-3"
}

func isUnpackedOldStyle(s string) bool {
	if len(s) != 7 {
		return false
	}
	if s[0] != 'A' && s[0] != 'B' {
		return false
	}
	for i := 1; i < 4; i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	if s[4] != ' ' {
		return false
	}
	if s[5] < 'A' || s[5] > 'Z' || s[6] < 'A' || s[6] > 'Z' {
		return false
	}
	return true
}

func isUnpackedProvisional(s string) bool {
	if len(s) < 7 || s[4] != ' ' {
		return false
	}
	for i := 0; i < 4; i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	if s[5] < 'A' || s[5] > 'Z' || s[6] < 'A' || s[6] > 'Z' {
		return false
	}
	for i := 7; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

func parseUnpackedCometFull(s string) (cometType byte, year int, ok bool) {
	slashIdx := strings.Index(s, "/")
	if slashIdx < 1 || slashIdx >= len(s)-1 {
		return 0, 0, false
	}

	// Find comet type (last letter before slash)
	for i := slashIdx - 1; i >= 0; i-- {
		c := s[i]
		if cometTypes[c] {
			cometType = c
			break
		}
	}

	if cometType == 0 {
		return 0, 0, false
	}

	// Parse year (first space-delimited part after slash)
	rest := s[slashIdx+1:]
	spaceIdx := strings.Index(rest, " ")
	if spaceIdx < 1 {
		return 0, 0, false
	}

	yearStr := rest[0:spaceIdx]
	var err error
	year, err = strconv.Atoi(yearStr)
	if err != nil {
		return 0, 0, false
	}

	return cometType, year, true
}

func isUnpackedCometNumbered(s string) bool {
	// Pattern: number followed by P or D, optionally followed by -fragment and/or /name
	numEnd := 0
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			numEnd = i
			break
		}
	}

	if numEnd == 0 || numEnd >= len(s) {
		return false
	}

	c := s[numEnd]
	if c != 'P' && c != 'D' {
		return false
	}

	// Rest should be empty, start with -, or start with /
	if numEnd+1 < len(s) {
		nextChar := s[numEnd+1]
		if nextChar != '/' && nextChar != '-' {
			return false
		}
		// If it starts with -, validate fragment
		if nextChar == '-' {
			rest := s[numEnd+2:]
			// Fragment should be 1-2 uppercase letters, optionally followed by /name
			fragEnd := 0
			for i := 0; i < len(rest); i++ {
				if rest[i] == '/' {
					fragEnd = i
					break
				}
				if rest[i] < 'A' || rest[i] > 'Z' {
					return false
				}
				fragEnd = i + 1
			}
			if fragEnd < 1 || fragEnd > 2 {
				return false
			}
		}
	}

	return true
}

func isBase62(c byte) bool {
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

func isDigitOrLower(c byte) bool {
	return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
}

// Convert converts a designation between packed and unpacked formats
func Convert(designation string) (Result, error) {
	info, err := DetectFormat(designation)
	if err != nil {
		return Result{}, err
	}

	var output string

	if info.Format == FormatPacked {
		switch info.Type {
		case "permanent":
			num, err := UnpackPermanent(designation)
			if err != nil {
				return Result{}, err
			}
			output = strconv.Itoa(num)
		case "provisional", "survey":
			output, err = UnpackProvisional(designation)
		case "provisional_extended":
			output, err = UnpackExtendedProvisional(designation)
		case "comet_numbered":
			output, err = UnpackCometNumbered(designation)
		case "comet_provisional":
			output, err = UnpackCometProvisional(designation)
		case "comet_full":
			output, err = UnpackCometFull(designation)
		case "comet_ancient", "comet_bce":
			output, err = UnpackAncientCometProvisional(designation)
		case "satellite":
			output, err = UnpackSatellite(designation)
		default:
			return Result{}, fmt.Errorf("%w: unknown type: %s", ErrInvalidFormat, info.Type)
		}
	} else {
		switch info.Type {
		case "permanent":
			num, err := strconv.Atoi(strings.TrimSpace(designation))
			if err != nil {
				return Result{}, fmt.Errorf("%w: invalid number", ErrInvalidFormat)
			}
			output, err = PackPermanent(num)
			if err != nil {
				return Result{}, err
			}
		case "provisional", "survey":
			output, err = PackProvisional(designation)
		case "comet_numbered":
			output, err = PackCometNumbered(designation)
		case "comet_full":
			output, err = PackCometFull(designation)
		case "satellite":
			output, err = PackSatellite(designation)
		default:
			return Result{}, fmt.Errorf("%w: unknown type: %s", ErrInvalidFormat, info.Type)
		}
	}

	if err != nil {
		return Result{}, err
	}

	return Result{Input: designation, Output: output, Info: info}, nil
}

// ConvertSimple converts a designation and returns just the output string
func ConvertSimple(designation string) (string, error) {
	result, err := Convert(designation)
	if err != nil {
		return "", err
	}
	return result.Output, nil
}

// Pack ensures a designation is in packed format
func Pack(designation string) (string, error) {
	info, err := DetectFormat(designation)
	if err != nil {
		return "", err
	}

	if info.Format == FormatPacked {
		return strings.TrimSpace(designation), nil
	}

	result, err := Convert(designation)
	if err != nil {
		return "", err
	}
	return result.Output, nil
}

// Unpack ensures a designation is in unpacked (human-readable) format
func Unpack(designation string) (string, error) {
	info, err := DetectFormat(designation)
	if err != nil {
		return "", err
	}

	if info.Format == FormatUnpacked {
		return strings.TrimSpace(designation), nil
	}

	result, err := Convert(designation)
	if err != nil {
		return "", err
	}
	return result.Output, nil
}

// =============================================================================
// Helper Functions for Format Conversion and Fragment Handling
// =============================================================================

// ToReportFormat converts minimal packed format to 12-character MPC report format.
// The 12-character format is used in MPC observation records (columns 1-12).
// For numbered comets with fragments, the fragment letter(s) go in columns 11-12.
//
// Examples:
//
//	"0073Pa"   -> "0073P      a" (numbered comet with single fragment)
//	"0073Paa"  -> "0073P     aa" (numbered comet with double fragment)
//	"00001"    -> "       00001" (numbered asteroid)
//	"J95X00A"  -> "     J95X00A" (provisional asteroid)
//	"CJ95O010" -> "    CJ95O010" (provisional comet)
func ToReportFormat(minimal string) (string, error) {
	minimal = strings.TrimSpace(minimal)
	length := len(minimal)

	info, err := DetectFormat(minimal)
	if err != nil {
		return "", err
	}

	if info.Format != FormatPacked {
		return "", fmt.Errorf("%w: ToReportFormat requires packed format input", ErrInvalidFormat)
	}

	// Initialize 12-char output with spaces
	report := []byte("            ")

	switch info.Type {
	case "permanent":
		// Right-align 5-char designation
		for i := 0; i < length; i++ {
			report[12-length+i] = minimal[i]
		}

	case "provisional", "provisional_extended", "survey":
		// Right-align 7-char designation
		for i := 0; i < length; i++ {
			report[12-length+i] = minimal[i]
		}

	case "comet_numbered":
		// Numbered comet: first 5 chars (####P), fragment in cols 11-12
		if length == 5 {
			// No fragment
			for i := 0; i < 5; i++ {
				report[i] = minimal[i]
			}
		} else if length == 6 {
			// Single-letter fragment
			for i := 0; i < 5; i++ {
				report[i] = minimal[i]
			}
			report[11] = minimal[5]
		} else if length == 7 {
			// Two-letter fragment
			for i := 0; i < 5; i++ {
				report[i] = minimal[i]
			}
			report[10] = minimal[5]
			report[11] = minimal[6]
		}

	case "comet_provisional", "comet_full", "comet_ancient", "comet_bce":
		// Right-align in 12-char field
		for i := 0; i < length; i++ {
			report[12-length+i] = minimal[i]
		}

	case "satellite":
		// Right-align 8-char designation
		for i := 0; i < length; i++ {
			report[12-length+i] = minimal[i]
		}

	default:
		return "", fmt.Errorf("%w: unsupported type for report format: %s", ErrInvalidFormat, info.Type)
	}

	return string(report), nil
}

// FromReportFormat converts 12-character MPC report format to minimal packed format.
func FromReportFormat(report string) (string, error) {
	length := len(report)
	if length > 12 {
		return "", fmt.Errorf("%w: report format too long", ErrInvalidFormat)
	}

	// Pad to 12 chars if shorter
	for len(report) < 12 {
		report = " " + report
	}

	// Check for numbered comet with fragment (fragment in cols 11-12)
	// Pattern: ####P or ####D in cols 1-5, spaces in cols 6-10, lowercase in cols 11-12
	first5 := report[0:5]
	middle := report[5:10]
	frag1 := report[10]
	frag2 := report[11]

	// Check if this is a numbered comet format
	if len(first5) == 5 &&
		first5[0] >= '0' && first5[0] <= '9' &&
		first5[1] >= '0' && first5[1] <= '9' &&
		first5[2] >= '0' && first5[2] <= '9' &&
		first5[3] >= '0' && first5[3] <= '9' &&
		(first5[4] == 'P' || first5[4] == 'D') &&
		strings.TrimSpace(middle) == "" {

		result := first5
		if frag1 >= 'a' && frag1 <= 'z' {
			result += string(frag1)
		}
		if frag2 >= 'a' && frag2 <= 'z' {
			result += string(frag2)
		}
		return result, nil
	}

	// Standard case: just trim spaces
	return strings.TrimSpace(report), nil
}

// HasFragment checks if a designation has a comet fragment suffix.
// Works with both packed and unpacked formats.
func HasFragment(desig string) bool {
	info, err := DetectFormat(desig)
	if err != nil {
		return false
	}

	dtype := info.Type

	// Only comets can have fragments
	if dtype != "comet_numbered" && dtype != "comet_provisional" && dtype != "comet_full" {
		return false
	}

	desig = strings.TrimSpace(desig)
	length := len(desig)

	if info.Format == FormatUnpacked {
		// Look for "-X" or "-XX" at end
		if length >= 3 {
			dashIdx := strings.LastIndex(desig, "-")
			if dashIdx >= 0 && dashIdx < length-1 {
				frag := desig[dashIdx+1:]
				if len(frag) >= 1 && len(frag) <= 2 {
					allUpper := true
					for i := 0; i < len(frag); i++ {
						if frag[i] < 'A' || frag[i] > 'Z' {
							allUpper = false
							break
						}
					}
					return allUpper
				}
			}
		}
	} else {
		// Packed format
		if dtype == "comet_numbered" {
			// Check for lowercase after P/D (position 5+)
			if length > 5 && desig[5] >= 'a' && desig[5] <= 'z' {
				return true
			}
		} else if dtype == "comet_provisional" {
			// 7-char: last char lowercase and not '0'
			// 8-char: last two chars lowercase
			lastChar := desig[length-1]
			if lastChar >= 'a' && lastChar <= 'z' && lastChar != '0' {
				return true
			}
		} else if dtype == "comet_full" {
			// Check last char(s)
			lastChar := desig[length-1]
			if lastChar >= 'a' && lastChar <= 'z' && lastChar != '0' {
				return true
			}
		}
	}

	return false
}

// GetFragment extracts the fragment suffix from a comet designation.
// Works with both packed and unpacked formats.
// Fragment is returned in uppercase (e.g., "A", "AA").
// Returns empty string if no fragment.
func GetFragment(desig string) (string, error) {
	info, err := DetectFormat(desig)
	if err != nil {
		return "", err
	}

	dtype := info.Type

	// Only comets can have fragments
	if dtype != "comet_numbered" && dtype != "comet_provisional" && dtype != "comet_full" {
		return "", nil
	}

	desig = strings.TrimSpace(desig)
	length := len(desig)

	if info.Format == FormatUnpacked {
		// Look for "-X" or "-XX" at end
		dashIdx := strings.LastIndex(desig, "-")
		if dashIdx >= 0 && dashIdx < length-1 {
			frag := desig[dashIdx+1:]
			if len(frag) >= 1 && len(frag) <= 2 {
				allUpper := true
				for i := 0; i < len(frag); i++ {
					if frag[i] < 'A' || frag[i] > 'Z' {
						allUpper = false
						break
					}
				}
				if allUpper {
					return frag, nil
				}
			}
		}
	} else {
		// Packed format
		if dtype == "comet_numbered" {
			// Fragment is lowercase after P/D
			if length == 6 {
				return strings.ToUpper(string(desig[5])), nil
			} else if length == 7 {
				return strings.ToUpper(desig[5:7]), nil
			}
		} else if dtype == "comet_provisional" {
			// 7-char: position 6 if lowercase and not '0'
			// 8-char: positions 6-7 if lowercase
			if length == 7 {
				lastChar := desig[6]
				if lastChar >= 'a' && lastChar <= 'z' && lastChar != '0' {
					return strings.ToUpper(string(lastChar)), nil
				}
			} else if length == 8 {
				frag := desig[6:8]
				if frag[0] >= 'a' && frag[0] <= 'z' && frag[1] >= 'a' && frag[1] <= 'z' {
					return strings.ToUpper(frag), nil
				}
			}
		} else if dtype == "comet_full" {
			// 8-char: position 7 if lowercase and not '0'
			// 9-char: positions 7-8 if lowercase
			if length == 8 {
				lastChar := desig[7]
				if lastChar >= 'a' && lastChar <= 'z' && lastChar != '0' {
					return strings.ToUpper(string(lastChar)), nil
				}
			} else if length == 9 {
				frag := desig[7:9]
				if frag[0] >= 'a' && frag[0] <= 'z' && frag[1] >= 'a' && frag[1] <= 'z' {
					return strings.ToUpper(frag), nil
				}
			}
		}
	}

	return "", nil
}

// GetParent returns the parent comet designation (without fragment suffix).
// Works with both packed and unpacked formats.
// Returns the designation in the same format (packed or unpacked) as input.
func GetParent(desig string) (string, error) {
	info, err := DetectFormat(desig)
	if err != nil {
		return "", err
	}

	dtype := info.Type

	// Non-comets: return as-is
	if dtype != "comet_numbered" && dtype != "comet_provisional" && dtype != "comet_full" {
		return strings.TrimSpace(desig), nil
	}

	desig = strings.TrimSpace(desig)
	length := len(desig)

	if info.Format == FormatUnpacked {
		// Remove "-X" or "-XX" suffix if present
		dashIdx := strings.LastIndex(desig, "-")
		if dashIdx >= 0 && dashIdx < length-1 {
			frag := desig[dashIdx+1:]
			if len(frag) >= 1 && len(frag) <= 2 {
				allUpper := true
				for i := 0; i < len(frag); i++ {
					if frag[i] < 'A' || frag[i] > 'Z' {
						allUpper = false
						break
					}
				}
				if allUpper {
					return desig[0:dashIdx], nil
				}
			}
		}
	} else {
		// Packed format
		if dtype == "comet_numbered" {
			// Remove lowercase fragment letters after P/D
			if length > 5 && desig[5] >= 'a' && desig[5] <= 'z' {
				return desig[0:5], nil
			}
		} else if dtype == "comet_provisional" {
			// 7-char: replace lowercase fragment with '0'
			// 8-char: replace 2 lowercase with '0', truncate
			if length == 7 {
				lastChar := desig[6]
				if lastChar >= 'a' && lastChar <= 'z' && lastChar != '0' {
					return desig[0:6] + "0", nil
				}
			} else if length == 8 {
				frag := desig[6:8]
				if frag[0] >= 'a' && frag[0] <= 'z' {
					return desig[0:6] + "0", nil
				}
			}
		} else if dtype == "comet_full" {
			// 8-char: replace fragment with '0'
			// 9-char: replace fragment with '0', truncate
			if length == 8 {
				lastChar := desig[7]
				if lastChar >= 'a' && lastChar <= 'z' && lastChar != '0' {
					return desig[0:7] + "0", nil
				}
			} else if length == 9 {
				frag := desig[7:9]
				if frag[0] >= 'a' && frag[0] <= 'z' {
					return desig[0:7] + "0", nil
				}
			}
		}
	}

	return desig, nil
}

// DesignationsEqual checks if two designations refer to the same object.
// This function normalizes both designations to packed format and compares them,
// handling different formats (packed/unpacked).
func DesignationsEqual(desig1, desig2 string) bool {
	packed1, err := Pack(desig1)
	if err != nil {
		return false
	}

	packed2, err := Pack(desig2)
	if err != nil {
		return false
	}

	return packed1 == packed2
}
