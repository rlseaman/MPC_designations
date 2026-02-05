//! MPC Designation Converter
//!
//! Convert between packed and unpacked Minor Planet Center (MPC) designations
//! for asteroids, comets, and natural satellites.
//!
//! Based on MPC specification: <https://www.minorplanetcenter.net/iau/info/PackedDes.html>

use std::collections::HashMap;
use std::fmt;
use std::sync::OnceLock;
use regex::Regex;

// =============================================================================
// Regex helper macro - must be defined before use
// =============================================================================

macro_rules! regex_captures {
    ($pattern:expr, $text:expr) => {{
        static RE: OnceLock<Regex> = OnceLock::new();
        let re = RE.get_or_init(|| Regex::new($pattern).unwrap());
        re.captures($text)
    }};
}

/// Base-62 character set: 0-9, A-Z, a-z
const BASE62_CHARS: &[u8] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/// Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
pub const MAX_ASTEROID_NUMBER: u32 = 15396335;

/// Error type for MPC designation operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MPCDesignationError {
    message: String,
}

impl MPCDesignationError {
    pub fn new(msg: impl Into<String>) -> Self {
        MPCDesignationError { message: msg.into() }
    }
}

impl fmt::Display for MPCDesignationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for MPCDesignationError {}

pub type Result<T> = std::result::Result<T, MPCDesignationError>;

/// Format detection result
#[derive(Debug, Clone)]
pub struct FormatInfo {
    pub format: String,
    pub designation_type: String,
    pub subtype: String,
}

/// Conversion result with detailed information
#[derive(Debug, Clone)]
pub struct ConvertResult {
    pub input: String,
    pub output: String,
    pub info: FormatInfo,
}

// =============================================================================
// Lookup tables
// =============================================================================

lazy_static::lazy_static! {
    static ref CENTURY_CODES: HashMap<char, u32> = {
        let mut m = HashMap::new();
        m.insert('A', 10); m.insert('B', 11); m.insert('C', 12);
        m.insert('D', 13); m.insert('E', 14); m.insert('F', 15);
        m.insert('G', 16); m.insert('H', 17); m.insert('I', 18);
        m.insert('J', 19); m.insert('K', 20); m.insert('L', 21);
        m
    };

    static ref REVERSE_CENTURY_CODES: HashMap<u32, char> = {
        CENTURY_CODES.iter().map(|(&k, &v)| (v, k)).collect()
    };

    static ref SURVEY_PACKED_TO_UNPACKED: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("PLS", "P-L");
        m.insert("T1S", "T-1");
        m.insert("T2S", "T-2");
        m.insert("T3S", "T-3");
        m
    };

    static ref SURVEY_UNPACKED_TO_PACKED: HashMap<&'static str, &'static str> = {
        SURVEY_PACKED_TO_UNPACKED.iter().map(|(&k, &v)| (v, k)).collect()
    };
}

const COMET_TYPES: &[char] = &['P', 'C', 'D', 'X', 'A', 'I'];
const SATELLITE_PLANETS: &[char] = &['J', 'S', 'U', 'N'];

fn planet_name(code: char) -> &'static str {
    match code {
        'J' => "Jupiter",
        'S' => "Saturn",
        'U' => "Uranus",
        'N' => "Neptune",
        _ => "Unknown",
    }
}

// =============================================================================
// Input validation
// =============================================================================

fn validate_raw_input(s: &str) -> Result<()> {
    for c in s.chars() {
        let code = c as u32;
        if code < 32 || code > 126 {
            return Err(MPCDesignationError::new(format!(
                "Invalid character in designation: {:?}", c
            )));
        }
    }
    Ok(())
}

fn validate_whitespace(s: &str) -> Result<()> {
    let mut prev_space = false;
    for c in s.chars() {
        let code = c as u32;
        if code < 32 || code > 126 {
            return Err(MPCDesignationError::new(format!(
                "Invalid character in designation: {:?}", c
            )));
        }
        if c == ' ' {
            if prev_space {
                return Err(MPCDesignationError::new("Consecutive spaces in designation"));
            }
            prev_space = true;
        } else {
            prev_space = false;
        }
    }
    Ok(())
}

fn is_valid_half_month(c: char) -> bool {
    c.is_ascii_uppercase() && c >= 'A' && c <= 'Y' && c != 'I'
}

// =============================================================================
// Base-62 encoding utilities
// =============================================================================

fn base62_to_num(c: char) -> Result<u32> {
    match BASE62_CHARS.iter().position(|&x| x == c as u8) {
        Some(idx) => Ok(idx as u32),
        None => Err(MPCDesignationError::new(format!("Invalid base-62 character: {}", c))),
    }
}

fn num_to_base62(num: u32) -> Result<char> {
    if num > 61 {
        return Err(MPCDesignationError::new(format!("Number out of base-62 range: {}", num)));
    }
    Ok(BASE62_CHARS[num as usize] as char)
}

fn base62_string_to_num(s: &str) -> Result<u32> {
    let mut result: u32 = 0;
    for c in s.chars() {
        result = result * 62 + base62_to_num(c)?;
    }
    Ok(result)
}

fn num_to_base62_string(mut num: u32, width: usize) -> Result<String> {
    let mut result = Vec::with_capacity(width);
    for _ in 0..width {
        result.push(num_to_base62(num % 62)?);
        num /= 62;
    }
    result.reverse();
    Ok(result.into_iter().collect())
}

// =============================================================================
// Permanent (numbered) asteroid designations
// =============================================================================

fn unpack_permanent(packed: &str) -> Result<u32> {
    let packed = packed.trim();
    let chars: Vec<char> = packed.chars().collect();
    let length = chars.len();

    if length == 0 {
        return Err(MPCDesignationError::new("Empty designation"));
    }

    let first = chars[0];

    // Check for tilde format first (>= 620,000)
    if first == '~' && length == 5 {
        let base62_part: String = chars[1..5].iter().collect();
        return Ok(620000 + base62_string_to_num(&base62_part)?);
    }

    if length != 5 {
        return Err(MPCDesignationError::new(format!(
            "Invalid packed permanent designation length: {}", packed
        )));
    }

    let rest: String = chars[1..5].iter().collect();

    if first.is_ascii_digit() {
        packed.parse::<u32>().map_err(|_| {
            MPCDesignationError::new(format!("Invalid packed permanent designation: {}", packed))
        })
    } else if first >= 'A' && first <= 'Z' {
        let val = (first as u32) - 55; // A=10, B=11, etc.
        let rest_num: u32 = rest.parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid packed permanent designation: {}", packed))
        })?;
        Ok(val * 10000 + rest_num)
    } else if first >= 'a' && first <= 'z' {
        let val = (first as u32) - 61; // a=36, b=37, etc.
        let rest_num: u32 = rest.parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid packed permanent designation: {}", packed))
        })?;
        Ok(val * 10000 + rest_num)
    } else {
        Err(MPCDesignationError::new(format!(
            "Invalid packed permanent designation: {}", packed
        )))
    }
}

fn pack_permanent(number: u32) -> Result<String> {
    if number < 1 || number > MAX_ASTEROID_NUMBER {
        return Err(MPCDesignationError::new(format!("Invalid asteroid number: {}", number)));
    }

    if number < 100000 {
        Ok(format!("{:05}", number))
    } else if number < 620000 {
        let div = number / 10000;
        let rem = number % 10000;
        let letter = if div < 36 {
            (div as u8 + 55) as char
        } else {
            (div as u8 + 61) as char
        };
        Ok(format!("{}{:04}", letter, rem))
    } else {
        let offset = number - 620000;
        Ok(format!("~{}", num_to_base62_string(offset, 4)?))
    }
}

// =============================================================================
// Cycle count encoding for provisional designations
// =============================================================================

fn decode_cycle_count(encoded: &str) -> Result<u32> {
    let chars: Vec<char> = encoded.chars().collect();
    if chars.len() != 2 {
        return Err(MPCDesignationError::new(format!("Invalid cycle count encoding: {}", encoded)));
    }

    let first = chars[0];
    let second = chars[1];

    let tens = if first.is_ascii_digit() {
        first.to_digit(10).unwrap()
    } else if first >= 'A' && first <= 'Z' {
        (first as u32) - 55
    } else if first >= 'a' && first <= 'z' {
        (first as u32) - 61
    } else {
        return Err(MPCDesignationError::new(format!("Invalid cycle count encoding: {}", encoded)));
    };

    if !second.is_ascii_digit() {
        return Err(MPCDesignationError::new(format!("Invalid cycle count encoding: {}", encoded)));
    }

    Ok(tens * 10 + second.to_digit(10).unwrap())
}

fn encode_cycle_count(count: u32) -> Result<String> {
    if count >= 620 {
        return Err(MPCDesignationError::new(format!("Cycle count out of range (0-619): {}", count)));
    }

    let tens = count / 10;
    let ones = count % 10;

    let first = if tens < 10 {
        char::from_digit(tens, 10).unwrap()
    } else if tens < 36 {
        (tens as u8 + 55) as char
    } else {
        (tens as u8 + 61) as char
    };

    Ok(format!("{}{}", first, ones))
}

// =============================================================================
// Letter/position utilities
// =============================================================================

fn letter_to_position(letter: char) -> Result<u32> {
    if !letter.is_ascii_uppercase() {
        return Err(MPCDesignationError::new(format!("Invalid half-month letter: {}", letter)));
    }

    let mut pos = (letter as u32) - ('A' as u32) + 1;
    if letter > 'I' {
        pos -= 1; // Skip I
    }
    Ok(pos)
}

fn position_to_letter(pos: u32) -> Result<char> {
    if pos < 1 || pos > 25 {
        return Err(MPCDesignationError::new(format!("Invalid letter position: {}", pos)));
    }

    let mut p = pos;
    if p >= 9 {
        p += 1; // Skip I
    }
    Ok(((('A' as u32) + p - 1) as u8) as char)
}

// =============================================================================
// Extended provisional format (cycle >= 620)
// =============================================================================

fn needs_extended_format(cycle: u32) -> bool {
    cycle >= 620
}

fn pack_extended_provisional(year: u32, half_month: char, second_letter: char, cycle: u32) -> Result<String> {
    let year_short = year % 100;
    let base_sequence = (cycle - 620) * 25 + letter_to_position(second_letter)? - 1;
    let seq_encoded = num_to_base62_string(base_sequence, 4)?;
    Ok(format!("_{}{}{}", num_to_base62(year_short)?, half_month, seq_encoded))
}

fn unpack_extended_provisional(packed: &str) -> Result<String> {
    let packed = packed.trim();
    let chars: Vec<char> = packed.chars().collect();

    if chars.len() != 7 || chars[0] != '_' {
        return Err(MPCDesignationError::new(format!(
            "Invalid extended packed provisional: {}", packed
        )));
    }

    let year_digit = chars[1];
    let half_month = chars[2];
    let seq_encoded: String = chars[3..7].iter().collect();

    let base_sequence = base62_string_to_num(&seq_encoded)?;
    let cycle = 620 + base_sequence / 25;
    let letter_pos = (base_sequence % 25) + 1;
    let second_letter = position_to_letter(letter_pos)?;

    let year = 2000 + base62_to_num(year_digit)?;

    Ok(format!("{} {}{}{}", year, half_month, second_letter, cycle))
}

// =============================================================================
// Standard provisional asteroid designations
// =============================================================================

fn unpack_provisional(packed: &str) -> Result<String> {
    let packed = packed.trim();
    let chars: Vec<char> = packed.chars().collect();

    // Check for survey designations first
    if chars.len() == 7 {
        let prefix: String = chars[0..3].iter().collect();
        if let Some(&survey) = SURVEY_PACKED_TO_UNPACKED.get(prefix.as_str()) {
            let number: String = chars[3..7].iter().collect();
            let num: u32 = number.parse().map_err(|_| {
                MPCDesignationError::new(format!("Invalid survey number: {}", number))
            })?;
            return Ok(format!("{} {}", num, survey));
        }
    }

    if chars.len() != 7 {
        return Err(MPCDesignationError::new(format!(
            "Invalid packed provisional designation length: {}", packed
        )));
    }

    let century = chars[0];
    let year: String = chars[1..3].iter().collect();
    let half_month = chars[3];
    let order_encoded: String = chars[4..6].iter().collect();
    let second_letter = chars[6];

    // Asteroid provisionals: only I-L valid (1800-2199)
    if century != 'I' && century != 'J' && century != 'K' && century != 'L' {
        return Err(MPCDesignationError::new(format!(
            "Invalid century code for asteroid provisional: {} (must be I-L for years 1800-2199)", century
        )));
    }

    let century_val = CENTURY_CODES.get(&century).ok_or_else(|| {
        MPCDesignationError::new(format!("Invalid century code: {}", century))
    })?;

    let full_year = format!("{}{}", century_val, year);
    let year_num: u32 = full_year.parse().unwrap();
    let order_num = decode_cycle_count(&order_encoded)?;

    // For pre-1925 designations, use A-prefix format (MPC canonical)
    if year_num < 1925 {
        let first_digit = full_year.chars().next().unwrap();
        let rest_of_year: String = full_year.chars().skip(1).collect();
        let prefix = match first_digit {
            '1' => "A",
            '2' => "B",
            _ => "",
        };
        if !prefix.is_empty() {
            if order_num == 0 {
                return Ok(format!("{}{} {}{}", prefix, rest_of_year, half_month, second_letter));
            }
            return Ok(format!("{}{} {}{}{}", prefix, rest_of_year, half_month, second_letter, order_num));
        }
    }

    if order_num == 0 {
        Ok(format!("{} {}{}", full_year, half_month, second_letter))
    } else {
        Ok(format!("{} {}{}{}", full_year, half_month, second_letter, order_num))
    }
}

fn pack_provisional(unpacked: &str) -> Result<String> {
    let unpacked = unpacked.trim();

    // Check for survey designations
    if let Some(caps) = regex_captures!(r"^(\d+) (P-L|T-[123])$", unpacked) {
        let number: u32 = caps[1].parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid survey number: {}", &caps[1]))
        })?;
        if number < 1 {
            return Err(MPCDesignationError::new(format!("Survey number must be positive: {}", number)));
        }
        let survey_packed = SURVEY_UNPACKED_TO_PACKED.get(&caps[2]).unwrap();
        return Ok(format!("{}{:04}", survey_packed, number));
    }

    // Check for old-style designation: "A908 CJ" or "B842 FA"
    if let Some(caps) = regex_captures!(r"^([AB])(\d)(\d{2}) ([A-Z])([A-Z])$", unpacked) {
        let century_digit = caps[2].chars().next().unwrap();
        let year_short = &caps[3];
        let half_month = caps[4].chars().next().unwrap();
        let second_letter = caps[5].chars().next().unwrap();

        let century_code = match century_digit {
            '8' => 'I',
            '9' => 'J',
            '0' => 'K',
            _ => return Err(MPCDesignationError::new(format!(
                "Invalid century digit in old-style designation: {}", century_digit
            ))),
        };

        return Ok(format!("{}{}{}00{}", century_code, year_short, half_month, second_letter));
    }

    // Match standard provisional: "1995 XA" or "1995 XA12"
    if let Some(caps) = regex_captures!(r"^(\d{4}) ([A-Z])([A-Z])(\d*)$", unpacked) {
        let year = &caps[1];
        let half_month = caps[2].chars().next().unwrap();
        let second_letter = caps[3].chars().next().unwrap();
        let order_str = &caps[4];

        if !is_valid_half_month(half_month) {
            return Err(MPCDesignationError::new(format!("Invalid half-month letter: {}", half_month)));
        }

        // Asteroid provisionals: only years 1800-2199 valid
        let year_int: u32 = year.parse().unwrap();
        if year_int < 1800 || year_int > 2199 {
            return Err(MPCDesignationError::new(format!(
                "Year out of range for asteroid provisional: {} (must be 1800-2199)", year
            )));
        }

        let century: u32 = year[0..2].parse().unwrap();
        let year_short = &year[2..4];

        let century_code = REVERSE_CENTURY_CODES.get(&century).ok_or_else(|| {
            MPCDesignationError::new(format!("Invalid century in year: {}", year))
        })?;

        let order_num: u32 = if order_str.is_empty() { 0 } else {
            order_str.parse().map_err(|_| {
                MPCDesignationError::new(format!("Invalid order number: {}", order_str))
            })?
        };

        if needs_extended_format(order_num) {
            return pack_extended_provisional(year.parse().unwrap(), half_month, second_letter, order_num);
        }

        let order_encoded = encode_cycle_count(order_num)?;
        return Ok(format!("{}{}{}{}{}", century_code, year_short, half_month, order_encoded, second_letter));
    }

    Err(MPCDesignationError::new(format!("Invalid unpacked provisional designation: {}", unpacked)))
}

// =============================================================================
// Comet provisional designations
// =============================================================================

fn unpack_comet_provisional(packed: &str) -> Result<String> {
    let packed = packed.trim();
    let chars: Vec<char> = packed.chars().collect();
    let length = chars.len();

    if length != 7 && length != 8 {
        return Err(MPCDesignationError::new(format!(
            "Invalid packed comet provisional designation length: {}", packed
        )));
    }

    let century = chars[0];
    let year: String = chars[1..3].iter().collect();
    let half_month = chars[3];
    let order_encoded: String = chars[4..6].iter().collect();

    let fragment: String = if length == 7 {
        chars[6].to_string()
    } else {
        chars[6..8].iter().collect()
    };

    let century_val = CENTURY_CODES.get(&century).ok_or_else(|| {
        MPCDesignationError::new(format!("Invalid century code: {}", century))
    })?;

    let full_year = format!("{}{}", century_val, year);
    let order_num = decode_cycle_count(&order_encoded)?;

    let mut result = format!("{} {}{}", full_year, half_month, order_num);

    if fragment != "0" {
        result.push_str(&format!("-{}", fragment.to_uppercase()));
    }

    Ok(result)
}

fn pack_comet_provisional(unpacked: &str) -> Result<String> {
    let unpacked = unpacked.trim();

    if let Some(caps) = regex_captures!(r"^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$", unpacked) {
        let year = &caps[1];
        let half_month = caps[2].chars().next().unwrap();
        let order_num: u32 = caps[3].parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid order number: {}", &caps[3]))
        })?;
        let fragment = caps.get(4).map(|m| m.as_str()).unwrap_or("");

        if order_num < 1 {
            return Err(MPCDesignationError::new(format!("Comet order number must be positive: {}", order_num)));
        }

        let century: u32 = year[0..2].parse().unwrap();
        let year_short = &year[2..4];

        let century_code = REVERSE_CENTURY_CODES.get(&century).ok_or_else(|| {
            MPCDesignationError::new(format!("Invalid century in year: {}", year))
        })?;

        let order_encoded = encode_cycle_count(order_num)?;
        let fragment_code = if fragment.is_empty() { "0".to_string() } else { fragment.to_lowercase() };

        return Ok(format!("{}{}{}{}{}", century_code, year_short, half_month, order_encoded, fragment_code));
    }

    Err(MPCDesignationError::new(format!("Invalid unpacked comet provisional designation: {}", unpacked)))
}

// =============================================================================
// Numbered comet designations
// =============================================================================

/// Unpack a numbered comet designation.
/// Supports fragments: 0073Pa -> 73P-A, 0073Paa -> 73P-AA
fn unpack_comet_numbered(packed: &str) -> Result<String> {
    let packed = packed.trim();

    // Match with optional lowercase fragment: 0073P, 0073Pa, or 0073Paa
    if let Some(caps) = regex_captures!(r"^(\d{4})([PD])([a-z]{1,2})?$", packed) {
        let number: u32 = caps[1].parse().unwrap();
        let comet_type = &caps[2];
        let fragment = caps.get(3).map(|m| m.as_str()).unwrap_or("");

        let mut result = format!("{}{}", number, comet_type);
        if !fragment.is_empty() {
            result.push('-');
            result.push_str(&fragment.to_uppercase());
        }
        return Ok(result);
    }

    Err(MPCDesignationError::new(format!("Invalid packed numbered comet designation: {}", packed)))
}

/// Pack a numbered comet designation.
/// Supports fragments: 73P-A -> 0073Pa, 73P-AA -> 0073Paa
fn pack_comet_numbered(unpacked: &str) -> Result<String> {
    let unpacked = unpacked.trim();

    // Match "1P" or "354P" or "73P-A" or "73P-AA" or "1P/Halley" (with optional name after slash)
    if let Some(caps) = regex_captures!(r"^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$", unpacked) {
        let number: u32 = caps[1].parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid comet number: {}", &caps[1]))
        })?;
        let comet_type = &caps[2];
        let fragment = caps.get(3).map(|m| m.as_str()).unwrap_or("");

        if number < 1 || number > 9999 {
            return Err(MPCDesignationError::new(format!("Comet number out of range (1-9999): {}", number)));
        }

        let mut result = format!("{:04}{}", number, comet_type);
        if !fragment.is_empty() {
            result.push_str(&fragment.to_lowercase());
        }
        return Ok(result);
    }

    Err(MPCDesignationError::new(format!("Invalid unpacked numbered comet designation: {}", unpacked)))
}

// =============================================================================
// Natural satellite designations
// =============================================================================

fn unpack_satellite(packed: &str) -> Result<String> {
    let packed = packed.trim();
    let chars: Vec<char> = packed.chars().collect();

    if chars.len() != 8 || chars[0] != 'S' {
        return Err(MPCDesignationError::new(format!("Invalid packed satellite designation: {}", packed)));
    }

    let century = chars[1];
    let year: String = chars[2..4].iter().collect();
    let planet = chars[4];
    let number_encoded: String = chars[5..7].iter().collect();

    let century_val = CENTURY_CODES.get(&century).ok_or_else(|| {
        MPCDesignationError::new(format!("Invalid century code: {}", century))
    })?;

    if !SATELLITE_PLANETS.contains(&planet) {
        return Err(MPCDesignationError::new(format!("Invalid planet code: {}", planet)));
    }

    let full_year = format!("{}{}", century_val, year);
    let number = decode_cycle_count(&number_encoded)?;

    Ok(format!("S/{} {} {}", full_year, planet, number))
}

fn pack_satellite(unpacked: &str) -> Result<String> {
    let unpacked = unpacked.trim();

    if let Some(caps) = regex_captures!(r"^S/(\d{4}) ([JSUN]) (\d+)$", unpacked) {
        let year = &caps[1];
        let planet = caps[2].chars().next().unwrap();
        let number: u32 = caps[3].parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid satellite number: {}", &caps[3]))
        })?;

        if number < 1 {
            return Err(MPCDesignationError::new(format!("Satellite number must be positive: {}", number)));
        }

        let century: u32 = year[0..2].parse().unwrap();
        let year_short = &year[2..4];

        let century_code = REVERSE_CENTURY_CODES.get(&century).ok_or_else(|| {
            MPCDesignationError::new(format!("Invalid century in year: {}", year))
        })?;

        let number_encoded = encode_cycle_count(number)?;

        return Ok(format!("S{}{}{}{}0", century_code, year_short, planet, number_encoded));
    }

    Err(MPCDesignationError::new(format!("Invalid unpacked satellite designation: {}", unpacked)))
}

// =============================================================================
// BCE year encoding for ancient comets
// =============================================================================

fn encode_bce_year(year: i32) -> Result<(char, String)> {
    if year >= 0 {
        return Err(MPCDesignationError::new(format!("Not a BCE year: {}", year)));
    }

    let abs_year = year.abs() as u32;
    let code = 99 - (abs_year % 100);

    let prefix = if abs_year < 100 {
        '/'
    } else if abs_year < 200 {
        '.'
    } else if abs_year < 300 {
        '-'
    } else {
        return Err(MPCDesignationError::new(format!("BCE year out of supported range: {}", year)));
    };

    Ok((prefix, format!("{:02}", code)))
}

fn decode_bce_year(prefix: char, code: &str) -> Result<i32> {
    let code_num: u32 = code.parse().map_err(|_| {
        MPCDesignationError::new(format!("Invalid BCE year code: {}", code))
    })?;
    let year_part = 99 - code_num;

    let year = match prefix {
        '/' => -(year_part as i32),
        '.' => -((year_part + 100) as i32),
        '-' => -((year_part + 200) as i32),
        _ => return Err(MPCDesignationError::new(format!("Invalid BCE prefix: {}", prefix))),
    };

    Ok(year)
}

fn is_ancient_year(year: i32) -> bool {
    year < 1000
}

// =============================================================================
// Ancient/BCE comet provisional designations
// =============================================================================

fn pack_ancient_comet_provisional(comet_type: char, year: i32, half_month: char, order_num: u32, fragment: &str) -> Result<String> {
    let order_encoded = encode_cycle_count(order_num)?;
    let fragment_code = if fragment.is_empty() { "0".to_string() } else { fragment.to_lowercase() };

    if year < 0 {
        let (prefix, code) = encode_bce_year(year)?;
        Ok(format!("{}{}{}{}{}{}", comet_type, prefix, code, half_month, order_encoded, fragment_code))
    } else {
        Ok(format!("{}{:03}{}{}{}", comet_type, year, half_month, order_encoded, fragment_code))
    }
}

fn unpack_ancient_comet_provisional(packed: &str) -> Result<String> {
    let packed = packed.trim();
    let chars: Vec<char> = packed.chars().collect();

    if chars.len() != 8 {
        return Err(MPCDesignationError::new(format!("Invalid ancient comet designation length: {}", packed)));
    }

    let comet_type = chars[0];

    if !COMET_TYPES.contains(&comet_type) {
        return Err(MPCDesignationError::new(format!("Invalid comet type: {}", comet_type)));
    }

    let (year, half_month, order_encoded, fragment) = if "/.\\-".contains(chars[1]) {
        let prefix = chars[1];
        let year_code: String = chars[2..4].iter().collect();
        let year = decode_bce_year(prefix, &year_code)?;
        (year, chars[4], chars[5..7].iter().collect::<String>(), chars[7])
    } else {
        let year: i32 = chars[1..4].iter().collect::<String>().parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid ancient year: {}", packed))
        })?;
        (year, chars[4], chars[5..7].iter().collect::<String>(), chars[7])
    };

    let order_num = decode_cycle_count(&order_encoded)?;
    let mut result = format!("{}/{} {}{}", comet_type, year, half_month, order_num);

    if fragment != '0' {
        result.push_str(&format!("-{}", fragment.to_uppercase()));
    }

    Ok(result)
}

// =============================================================================
// Helper functions for comet format detection
// =============================================================================

fn is_asteroid_style_packed(provisional_part: &str) -> bool {
    let chars: Vec<char> = provisional_part.chars().collect();
    if chars.len() != 7 {
        return false;
    }
    chars[6].is_ascii_uppercase()
}

fn is_asteroid_style_unpacked(provisional: &str) -> bool {
    if let Some(caps) = regex_captures!(r"^\d{4} ([A-Z])(.)", provisional) {
        let second_char = caps[2].chars().next().unwrap();
        return second_char.is_ascii_alphabetic();
    }
    false
}

// =============================================================================
// Full comet designations
// =============================================================================

fn unpack_comet_full(packed: &str) -> Result<String> {
    let mut packed = packed.to_string();
    let length = packed.len();

    let (comet_type, provisional_part, num_str) = if length == 8 {
        let chars: Vec<char> = packed.chars().collect();
        (chars[0], chars[1..8].iter().collect::<String>(), String::new())
    } else if length == 9 {
        let chars: Vec<char> = packed.chars().collect();
        (chars[0], chars[1..9].iter().collect::<String>(), String::new())
    } else if length == 12 || (length < 12 && packed.starts_with(' ')) {
        while packed.len() < 12 {
            packed.insert(0, ' ');
        }
        let chars: Vec<char> = packed.chars().collect();
        let num_part: String = chars[0..4].iter().collect();
        (chars[4], chars[5..12].iter().collect::<String>(), num_part.trim().to_string())
    } else {
        return Err(MPCDesignationError::new(format!(
            "Invalid packed full comet designation length: {}", packed
        )));
    };

    if !COMET_TYPES.contains(&comet_type) {
        return Err(MPCDesignationError::new(format!("Invalid comet type: {}", comet_type)));
    }

    let provisional = if is_asteroid_style_packed(&provisional_part) {
        unpack_provisional(&provisional_part)?
    } else {
        unpack_comet_provisional(&provisional_part)?
    };

    if num_str.is_empty() {
        Ok(format!("{}/{}", comet_type, provisional))
    } else {
        let number: u32 = num_str.parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid comet number: {}", num_str))
        })?;
        Ok(format!("{}{}/{}", number, comet_type, provisional))
    }
}

fn pack_comet_full(unpacked: &str) -> Result<String> {
    let unpacked = unpacked.trim();

    if let Some(caps) = regex_captures!(r"^(\d*)([PCDXAI])/(-?\d+) (.+)$", unpacked) {
        let number = &caps[1];
        let comet_type = caps[2].chars().next().unwrap();
        let year: i32 = caps[3].parse().map_err(|_| {
            MPCDesignationError::new(format!("Invalid year: {}", &caps[3]))
        })?;
        let prov_part = &caps[4];

        if !COMET_TYPES.contains(&comet_type) {
            return Err(MPCDesignationError::new(format!("Invalid comet type: {}", comet_type)));
        }

        if is_ancient_year(year) {
            if let Some(ancient_caps) = regex_captures!(r"^([A-Z])(\d+)(?:-([A-Z]))?$", prov_part) {
                let half_month = ancient_caps[1].chars().next().unwrap();
                let order_num: u32 = ancient_caps[2].parse().unwrap();
                let fragment = ancient_caps.get(3).map(|m| m.as_str()).unwrap_or("");
                return pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment);
            } else {
                return Err(MPCDesignationError::new(format!("Invalid ancient comet provisional: {}", prov_part)));
            }
        }

        let provisional = format!("{} {}", year, prov_part);

        let provisional_packed = if is_asteroid_style_unpacked(&provisional) {
            pack_provisional(&provisional)?
        } else {
            pack_comet_provisional(&provisional)?
        };

        if number.is_empty() {
            return Ok(format!("{}{}", comet_type, provisional_packed));
        } else {
            let num: u32 = number.parse().map_err(|_| {
                MPCDesignationError::new(format!("Invalid comet number: {}", number))
            })?;
            if num < 1 || num > 9999 {
                return Err(MPCDesignationError::new(format!("Comet number out of range (1-9999): {}", num)));
            }
            return Ok(format!("{:04}{}{}", num, comet_type, provisional_packed));
        }
    }

    Err(MPCDesignationError::new(format!("Invalid unpacked comet designation: {}", unpacked)))
}

// =============================================================================
// Format detection
// =============================================================================

/// Detect the format of a designation
pub fn detect_format(designation: &str) -> Result<FormatInfo> {
    validate_raw_input(designation)?;

    let mut info = FormatInfo {
        format: String::new(),
        designation_type: String::new(),
        subtype: String::new(),
    };

    // Check for packed full comet designation (12 chars with spaces)
    if designation.len() == 12 {
        if regex_captures!(r"^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_full".to_string();
            info.subtype = "comet with provisional designation (12-char)".to_string();
            return Ok(info);
        }
    }

    // Check for packed comet designation (8 chars)
    if designation.len() == 8 {
        if regex_captures!(r"^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$", designation).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_full".to_string();
            info.subtype = "comet with provisional designation (8-char)".to_string();
            return Ok(info);
        }
    }

    // Check for packed comet with 2-letter fragment (9 chars)
    if designation.len() == 9 {
        if regex_captures!(r"^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$", designation).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_full".to_string();
            info.subtype = "comet with provisional designation (9-char, 2-letter fragment)".to_string();
            return Ok(info);
        }
    }

    // Check for packed ancient comet (8 chars)
    if designation.len() == 8 {
        if regex_captures!(r"^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_ancient".to_string();
            info.subtype = "comet with ancient provisional (year < 1000)".to_string();
            return Ok(info);
        }
    }

    // Check for packed BCE comet (8 chars)
    if designation.len() == 8 {
        if regex_captures!(r"^([PCDXAI])([/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$", designation).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_bce".to_string();
            info.subtype = "comet with BCE provisional".to_string();
            return Ok(info);
        }
    }

    let des = designation.trim();
    validate_whitespace(des)?;

    // Check for packed satellite designation
    if des.len() == 8 && des.starts_with('S') {
        if regex_captures!(r"^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$", des).is_some() {
            let planet = des.chars().nth(4).unwrap();
            info.format = "packed".to_string();
            info.designation_type = "satellite".to_string();
            info.subtype = format!("natural satellite ({})", planet_name(planet));
            return Ok(info);
        }
    }

    // Check for packed permanent asteroid
    if des.len() == 5 {
        if des.starts_with('~') {
            if regex_captures!(r"^~[0-9A-Za-z]{4}$", des).is_some() {
                info.format = "packed".to_string();
                info.designation_type = "permanent".to_string();
                info.subtype = "permanent numbered (tilde/base-62, >= 620000)".to_string();
                return Ok(info);
            }
        } else if regex_captures!(r"^\d{5}$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "permanent".to_string();
            info.subtype = "permanent numbered (5-digit, < 100000)".to_string();
            return Ok(info);
        } else if regex_captures!(r"^[A-Za-z][0-9]{4}$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "permanent".to_string();
            if des.chars().next().unwrap().is_ascii_uppercase() {
                info.subtype = "permanent numbered (letter-prefix, 100000-359999)".to_string();
            } else {
                info.subtype = "permanent numbered (letter-prefix, 360000-619999)".to_string();
            }
            return Ok(info);
        }
    }

    // Check for packed provisional asteroid (7 chars)
    if des.len() == 7 {
        if des.starts_with('_') {
            if regex_captures!(r"^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$", des).is_some() {
                info.format = "packed".to_string();
                info.designation_type = "provisional_extended".to_string();
                info.subtype = "provisional (extended format, cycle >=620)".to_string();
                return Ok(info);
            }
        }
        if regex_captures!(r"^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "provisional".to_string();
            info.subtype = "provisional".to_string();
            return Ok(info);
        }
        if des.starts_with("PLS") && regex_captures!(r"^\d{4}$", &des[3..]).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "survey".to_string();
            info.subtype = "survey (Palomar-Leiden)".to_string();
            return Ok(info);
        }
        if regex_captures!(r"^T[123]S\d{4}$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "survey".to_string();
            info.subtype = format!("survey (Trojan T-{})", des.chars().nth(1).unwrap());
            return Ok(info);
        }
    }

    // Check for packed numbered comet (with optional fragment)
    if des.len() == 5 {
        if regex_captures!(r"^[0-9]{4}[PD]$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_numbered".to_string();
            info.subtype = "comet numbered".to_string();
            return Ok(info);
        }
    }

    // Check for packed numbered comet with fragment (6-7 chars: ####Pa or ####Paa)
    if des.len() == 6 || des.len() == 7 {
        if regex_captures!(r"^[0-9]{4}[PD][a-z]{1,2}$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_numbered".to_string();
            let frag_len = des.len() - 5;
            if frag_len == 1 {
                info.subtype = "comet numbered with fragment".to_string();
            } else {
                info.subtype = "comet numbered with 2-letter fragment".to_string();
            }
            return Ok(info);
        }
    }

    // Check for packed comet provisional
    if des.len() == 7 {
        if regex_captures!(r"^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", des).is_some() {
            info.format = "packed".to_string();
            info.designation_type = "comet_provisional".to_string();
            info.subtype = "comet provisional".to_string();
            return Ok(info);
        }
    }

    // --- UNPACKED FORMATS ---

    // Check for unpacked satellite
    if regex_captures!(r"^S/\d{4} ([JSUN]) \d+$", des).is_some() {
        let planet = des.chars().nth(8).unwrap();
        info.format = "unpacked".to_string();
        info.designation_type = "satellite".to_string();
        info.subtype = format!("natural satellite ({})", planet_name(planet));
        return Ok(info);
    }

    // Check for unpacked permanent asteroid
    if regex_captures!(r"^\d+$", des).is_some() {
        info.format = "unpacked".to_string();
        info.designation_type = "permanent".to_string();
        info.subtype = "permanent numbered".to_string();
        return Ok(info);
    }

    // Check for unpacked survey
    if regex_captures!(r"^\d+ (P-L|T-[123])$", des).is_some() {
        info.format = "unpacked".to_string();
        info.designation_type = "survey".to_string();
        if des.contains("P-L") {
            info.subtype = "survey (Palomar-Leiden)".to_string();
        } else {
            info.subtype = format!("survey (Trojan {})", &des[des.len()-3..]);
        }
        return Ok(info);
    }

    // Check for old-style asteroid designation
    if regex_captures!(r"^[AB]\d{3} [A-Z][A-Z]$", des).is_some() {
        info.format = "unpacked".to_string();
        info.designation_type = "provisional".to_string();
        info.subtype = "provisional (old-style pre-1925)".to_string();
        return Ok(info);
    }

    // Check for unpacked provisional asteroid
    if regex_captures!(r"^\d{4} [A-Z][A-Z]\d*$", des).is_some() {
        info.format = "unpacked".to_string();
        info.designation_type = "provisional".to_string();
        info.subtype = "provisional".to_string();
        return Ok(info);
    }

    // Check for unpacked comet with type prefix
    if regex_captures!(r"^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$", des).is_some() {
        info.format = "unpacked".to_string();
        info.designation_type = "comet_full".to_string();
        info.subtype = "comet provisional".to_string();
        return Ok(info);
    }

    // Check for unpacked numbered comet (with optional fragment)
    if let Some(caps) = regex_captures!(r"^(\d+)([PD])(?:-([A-Z]{1,2}))?(?:/[A-Za-z].*)?$", des) {
        info.format = "unpacked".to_string();
        info.designation_type = "comet_numbered".to_string();
        let fragment = caps.get(3).map(|m| m.as_str()).unwrap_or("");
        if !fragment.is_empty() {
            if fragment.len() == 1 {
                info.subtype = "comet numbered with fragment".to_string();
            } else {
                info.subtype = "comet numbered with 2-letter fragment".to_string();
            }
        } else {
            info.subtype = "comet numbered".to_string();
        }
        return Ok(info);
    }

    Err(MPCDesignationError::new(format!("Unable to detect designation format: {}", designation)))
}

// =============================================================================
// Main conversion functions
// =============================================================================

/// Convert a designation with detailed information
pub fn convert(designation: &str) -> Result<ConvertResult> {
    let info = detect_format(designation)?;

    let output = if info.format == "packed" {
        match info.designation_type.as_str() {
            "permanent" => unpack_permanent(designation)?.to_string(),
            "provisional" | "survey" => unpack_provisional(designation)?,
            "provisional_extended" => unpack_extended_provisional(designation)?,
            "comet_numbered" => unpack_comet_numbered(designation)?,
            "comet_provisional" => unpack_comet_provisional(designation)?,
            "comet_full" => unpack_comet_full(designation)?,
            "comet_ancient" | "comet_bce" => unpack_ancient_comet_provisional(designation)?,
            "satellite" => unpack_satellite(designation)?,
            _ => return Err(MPCDesignationError::new(format!("Unknown type: {}", info.designation_type))),
        }
    } else {
        match info.designation_type.as_str() {
            "permanent" => {
                let num: u32 = designation.trim().parse().map_err(|_| {
                    MPCDesignationError::new(format!("Invalid number: {}", designation))
                })?;
                pack_permanent(num)?
            }
            "provisional" | "survey" => pack_provisional(designation)?,
            "comet_numbered" => pack_comet_numbered(designation)?,
            "comet_full" => pack_comet_full(designation)?,
            "satellite" => pack_satellite(designation)?,
            _ => return Err(MPCDesignationError::new(format!("Unknown type: {}", info.designation_type))),
        }
    };

    Ok(ConvertResult {
        input: designation.to_string(),
        output,
        info,
    })
}

/// Convert a designation, returning just the result string
pub fn convert_simple(designation: &str) -> Result<String> {
    Ok(convert(designation)?.output)
}

/// Ensure a designation is in packed format
pub fn pack(designation: &str) -> Result<String> {
    let info = detect_format(designation)?;
    if info.format == "packed" {
        Ok(designation.trim().to_string())
    } else {
        convert_simple(designation)
    }
}

/// Ensure a designation is in unpacked format
pub fn unpack(designation: &str) -> Result<String> {
    let info = detect_format(designation)?;
    if info.format == "unpacked" {
        Ok(designation.trim().to_string())
    } else {
        convert_simple(designation)
    }
}

/// Check if a string is a valid MPC designation
pub fn is_valid_designation(designation: &str) -> bool {
    detect_format(designation).is_ok()
}

// =============================================================================
// Helper Functions for Format Conversion and Fragment Handling
// =============================================================================

/// Convert minimal packed format to 12-character MPC report format.
/// The 12-character format is used in MPC observation records (columns 1-12).
/// For numbered comets with fragments, the fragment letter(s) go in columns 11-12.
///
/// # Examples
/// ```
/// use mpc_designation::to_report_format;
/// assert_eq!(to_report_format("0073Pa").unwrap(), "0073P      a");
/// assert_eq!(to_report_format("00001").unwrap(), "       00001");
/// ```
pub fn to_report_format(minimal: &str) -> Result<String> {
    let minimal = minimal.trim();
    let length = minimal.len();

    let info = detect_format(minimal)?;

    if info.format != "packed" {
        return Err(MPCDesignationError::new(format!(
            "to_report_format requires packed format input: {}", minimal
        )));
    }

    let mut report = [' '; 12];

    match info.designation_type.as_str() {
        "permanent" => {
            // Right-align 5-char designation
            for (i, c) in minimal.chars().enumerate() {
                report[12 - length + i] = c;
            }
        }
        "provisional" | "provisional_extended" | "survey" => {
            // Right-align 7-char designation
            for (i, c) in minimal.chars().enumerate() {
                report[12 - length + i] = c;
            }
        }
        "comet_numbered" => {
            // Numbered comet: first 5 chars (####P), fragment in cols 11-12
            let chars: Vec<char> = minimal.chars().collect();
            if length == 5 {
                // No fragment
                for i in 0..5 {
                    report[i] = chars[i];
                }
            } else if length == 6 {
                // Single-letter fragment
                for i in 0..5 {
                    report[i] = chars[i];
                }
                report[11] = chars[5];
            } else if length == 7 {
                // Two-letter fragment
                for i in 0..5 {
                    report[i] = chars[i];
                }
                report[10] = chars[5];
                report[11] = chars[6];
            }
        }
        "comet_provisional" | "comet_full" | "comet_ancient" | "comet_bce" => {
            // Right-align in 12-char field
            for (i, c) in minimal.chars().enumerate() {
                report[12 - length + i] = c;
            }
        }
        "satellite" => {
            // Right-align 8-char designation
            for (i, c) in minimal.chars().enumerate() {
                report[12 - length + i] = c;
            }
        }
        _ => {
            return Err(MPCDesignationError::new(format!(
                "Unsupported type for report format: {}", info.designation_type
            )));
        }
    }

    Ok(report.iter().collect())
}

/// Convert 12-character MPC report format to minimal packed format.
///
/// # Examples
/// ```
/// use mpc_designation::from_report_format;
/// assert_eq!(from_report_format("0073P      a").unwrap(), "0073Pa");
/// assert_eq!(from_report_format("       00001").unwrap(), "00001");
/// ```
pub fn from_report_format(report: &str) -> Result<String> {
    let mut report = report.to_string();

    if report.len() > 12 {
        return Err(MPCDesignationError::new(format!(
            "Report format too long: {}", report
        )));
    }

    // Pad to 12 chars if shorter
    while report.len() < 12 {
        report.insert(0, ' ');
    }

    let chars: Vec<char> = report.chars().collect();

    // Check for numbered comet with fragment (fragment in cols 11-12)
    // Pattern: ####P or ####D in cols 1-5, spaces in cols 6-10, lowercase in cols 11-12
    let first5: String = chars[0..5].iter().collect();
    let middle: String = chars[5..10].iter().collect();
    let frag1 = chars[10];
    let frag2 = chars[11];

    // Check if this is a numbered comet format
    if regex_captures!(r"^[0-9]{4}[PD]$", &first5).is_some() && middle.trim().is_empty() {
        let mut result = first5;
        if frag1 >= 'a' && frag1 <= 'z' {
            result.push(frag1);
        }
        if frag2 >= 'a' && frag2 <= 'z' {
            result.push(frag2);
        }
        return Ok(result);
    }

    // Standard case: just trim spaces
    Ok(report.trim().to_string())
}

/// Check if a designation has a comet fragment suffix.
/// Works with both packed and unpacked formats.
///
/// # Examples
/// ```
/// use mpc_designation::has_fragment;
/// assert_eq!(has_fragment("73P-A"), true);
/// assert_eq!(has_fragment("73P"), false);
/// assert_eq!(has_fragment("0073Pa"), true);
/// ```
pub fn has_fragment(desig: &str) -> bool {
    let info = match detect_format(desig) {
        Ok(i) => i,
        Err(_) => return false,
    };

    let dtype = &info.designation_type;

    // Only comets can have fragments
    if dtype != "comet_numbered" && dtype != "comet_provisional" && dtype != "comet_full" {
        return false;
    }

    let desig = desig.trim();
    let length = desig.len();

    if info.format == "unpacked" {
        // Look for "-X" or "-XX" at end
        return regex_captures!(r".*-[A-Z]{1,2}$", desig).is_some();
    } else {
        // Packed format
        if dtype == "comet_numbered" {
            // Check for lowercase after P/D (position 5+)
            if length > 5 {
                let c = desig.chars().nth(5).unwrap();
                return c >= 'a' && c <= 'z';
            }
        } else if dtype == "comet_provisional" {
            // 7-char: last char lowercase and not '0'
            let last_char = desig.chars().last().unwrap();
            return last_char >= 'a' && last_char <= 'z' && last_char != '0';
        } else if dtype == "comet_full" {
            let last_char = desig.chars().last().unwrap();
            return last_char >= 'a' && last_char <= 'z' && last_char != '0';
        }
    }
    false
}

/// Extract the fragment suffix from a comet designation.
/// Works with both packed and unpacked formats.
/// Fragment is returned in uppercase (e.g., "A", "AA").
/// Returns empty string if no fragment.
///
/// # Examples
/// ```
/// use mpc_designation::get_fragment;
/// assert_eq!(get_fragment("73P-A").unwrap(), "A");
/// assert_eq!(get_fragment("0073Paa").unwrap(), "AA");
/// assert_eq!(get_fragment("73P").unwrap(), "");
/// ```
pub fn get_fragment(desig: &str) -> Result<String> {
    let info = detect_format(desig)?;
    let dtype = &info.designation_type;

    // Only comets can have fragments
    if dtype != "comet_numbered" && dtype != "comet_provisional" && dtype != "comet_full" {
        return Ok(String::new());
    }

    let desig = desig.trim();
    let length = desig.len();

    if info.format == "unpacked" {
        // Look for "-X" or "-XX" at end
        if let Some(caps) = regex_captures!(r"-([A-Z]{1,2})$", desig) {
            return Ok(caps[1].to_string());
        }
    } else {
        // Packed format
        if dtype == "comet_numbered" {
            // Fragment is lowercase after P/D
            if length == 6 {
                return Ok(desig.chars().nth(5).unwrap().to_uppercase().to_string());
            } else if length == 7 {
                return Ok(desig[5..7].to_uppercase());
            }
        } else if dtype == "comet_provisional" {
            // 7-char: position 6 if lowercase and not '0'
            // 8-char: positions 6-7 if lowercase
            if length == 7 {
                let last_char = desig.chars().nth(6).unwrap();
                if last_char >= 'a' && last_char <= 'z' && last_char != '0' {
                    return Ok(last_char.to_uppercase().to_string());
                }
            } else if length == 8 {
                let frag = &desig[6..8];
                let chars: Vec<char> = frag.chars().collect();
                if chars[0] >= 'a' && chars[0] <= 'z' && chars[1] >= 'a' && chars[1] <= 'z' {
                    return Ok(frag.to_uppercase());
                }
            }
        } else if dtype == "comet_full" {
            // 8-char: position 7 if lowercase and not '0'
            // 9-char: positions 7-8 if lowercase
            if length == 8 {
                let last_char = desig.chars().nth(7).unwrap();
                if last_char >= 'a' && last_char <= 'z' && last_char != '0' {
                    return Ok(last_char.to_uppercase().to_string());
                }
            } else if length == 9 {
                let frag = &desig[7..9];
                let chars: Vec<char> = frag.chars().collect();
                if chars[0] >= 'a' && chars[0] <= 'z' && chars[1] >= 'a' && chars[1] <= 'z' {
                    return Ok(frag.to_uppercase());
                }
            }
        }
    }

    Ok(String::new())
}

/// Get the parent comet designation (without fragment suffix).
/// Works with both packed and unpacked formats.
/// Returns the designation in the same format (packed or unpacked) as input.
///
/// # Examples
/// ```
/// use mpc_designation::get_parent;
/// assert_eq!(get_parent("73P-A").unwrap(), "73P");
/// assert_eq!(get_parent("0073Paa").unwrap(), "0073P");
/// ```
pub fn get_parent(desig: &str) -> Result<String> {
    let info = detect_format(desig)?;
    let dtype = &info.designation_type;

    // Non-comets: return as-is
    if dtype != "comet_numbered" && dtype != "comet_provisional" && dtype != "comet_full" {
        return Ok(desig.trim().to_string());
    }

    let desig = desig.trim();
    let length = desig.len();

    if info.format == "unpacked" {
        // Remove "-X" or "-XX" suffix if present
        if let Some(caps) = regex_captures!(r"^(.+)-[A-Z]{1,2}$", desig) {
            return Ok(caps[1].to_string());
        }
    } else {
        // Packed format
        if dtype == "comet_numbered" {
            // Remove lowercase fragment letters after P/D
            if length > 5 {
                let c = desig.chars().nth(5).unwrap();
                if c >= 'a' && c <= 'z' {
                    return Ok(desig[0..5].to_string());
                }
            }
        } else if dtype == "comet_provisional" {
            // 7-char: replace lowercase fragment with '0'
            // 8-char: replace 2 lowercase with '0', truncate
            if length == 7 {
                let last_char = desig.chars().nth(6).unwrap();
                if last_char >= 'a' && last_char <= 'z' && last_char != '0' {
                    return Ok(format!("{}0", &desig[0..6]));
                }
            } else if length == 8 {
                let c = desig.chars().nth(6).unwrap();
                if c >= 'a' && c <= 'z' {
                    return Ok(format!("{}0", &desig[0..6]));
                }
            }
        } else if dtype == "comet_full" {
            // 8-char: replace fragment with '0'
            // 9-char: replace fragment with '0', truncate
            if length == 8 {
                let last_char = desig.chars().nth(7).unwrap();
                if last_char >= 'a' && last_char <= 'z' && last_char != '0' {
                    return Ok(format!("{}0", &desig[0..7]));
                }
            } else if length == 9 {
                let c = desig.chars().nth(7).unwrap();
                if c >= 'a' && c <= 'z' {
                    return Ok(format!("{}0", &desig[0..7]));
                }
            }
        }
    }

    Ok(desig.to_string())
}

/// Check if two designations refer to the same object.
/// This function normalizes both designations to packed format and compares them,
/// handling different formats (packed/unpacked).
///
/// # Examples
/// ```
/// use mpc_designation::designations_equal;
/// assert_eq!(designations_equal("1995 XA", "J95X00A"), true);
/// assert_eq!(designations_equal("73P-A", "0073Pa"), true);
/// assert_eq!(designations_equal("73P-A", "73P-B"), false);
/// ```
pub fn designations_equal(desig1: &str, desig2: &str) -> bool {
    match (pack(desig1), pack(desig2)) {
        (Ok(p1), Ok(p2)) => p1 == p2,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_permanent_roundtrip() {
        assert_eq!(convert_simple("1").unwrap(), "00001");
        assert_eq!(convert_simple("00001").unwrap(), "1");
        assert_eq!(convert_simple("100001").unwrap(), "A0001");
        assert_eq!(convert_simple("A0001").unwrap(), "100001");
        assert_eq!(convert_simple("620000").unwrap(), "~0000");
        assert_eq!(convert_simple("~0000").unwrap(), "620000");
    }

    #[test]
    fn test_provisional_roundtrip() {
        assert_eq!(convert_simple("1995 XA").unwrap(), "J95X00A");
        assert_eq!(convert_simple("J95X00A").unwrap(), "1995 XA");
        assert_eq!(convert_simple("2024 AB12").unwrap(), "K24A12B");
        assert_eq!(convert_simple("K24A12B").unwrap(), "2024 AB12");
    }

    #[test]
    fn test_comet_roundtrip() {
        assert_eq!(convert_simple("1P").unwrap(), "0001P");
        assert_eq!(convert_simple("0001P").unwrap(), "1P");
        assert_eq!(convert_simple("C/1995 O1").unwrap(), "CJ95O010");
        assert_eq!(convert_simple("CJ95O010").unwrap(), "C/1995 O1");
    }

    #[test]
    fn test_satellite_roundtrip() {
        assert_eq!(convert_simple("S/2019 S 22").unwrap(), "SK19S220");
        assert_eq!(convert_simple("SK19S220").unwrap(), "S/2019 S 22");
    }
}
