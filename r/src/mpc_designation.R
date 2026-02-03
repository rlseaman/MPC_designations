#' MPC Designation Converter
#'
#' Convert between packed and unpacked Minor Planet Center (MPC) designations
#' for asteroids, comets, and natural satellites.
#'
#' Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
#'
#' @name mpc_designation
#' @docType package
NULL

# Package version
MPC_VERSION <- "1.0.0"

# Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
MAX_ASTEROID_NUMBER <- 15396335

# Base-62 character set
BASE62_CHARS <- "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

# Century codes for provisional designations
CENTURY_CODES <- c(
  A = 10, B = 11, C = 12, D = 13, E = 14, F = 15,
  G = 16, H = 17, I = 18, J = 19, K = 20, L = 21
)

# Survey codes
SURVEY_PACKED_TO_UNPACKED <- c(
  PLS = "P-L", T1S = "T-1", T2S = "T-2", T3S = "T-3"
)
SURVEY_UNPACKED_TO_PACKED <- c(
  `P-L` = "PLS", `T-1` = "T1S", `T-2` = "T2S", `T-3` = "T3S"
)

# Comet types
COMET_TYPES <- c("P", "C", "D", "X", "A", "I")
COMET_TYPE_DESCRIPTIONS <- c(
  P = "periodic", C = "non-periodic", D = "defunct",
  X = "uncertain orbit", A = "asteroid with comet designation", I = "interstellar"
)

# Satellite planet codes
SATELLITE_PLANETS <- c("J", "S", "U", "N")
SATELLITE_PLANET_NAMES <- c(
  J = "Jupiter", S = "Saturn", U = "Uranus", N = "Neptune"
)

# =============================================================================
# Error handling
# =============================================================================

#' Stop with MPC designation error
#' @param msg Error message
#' @keywords internal
mpc_error <- function(msg) {
  stop(paste0("MPCDesignationError: ", msg), call. = FALSE)
}

# =============================================================================
# Input validation
# =============================================================================

#' Validate raw input string
#' @param s Input string
#' @keywords internal
validate_raw_input <- function(s) {
  chars <- utf8ToInt(s)
  invalid <- chars[chars < 32 | chars > 126]
  if (length(invalid) > 0) {
    mpc_error(sprintf("Invalid character in designation: '\\x%02x'", invalid[1]))
  }
}

#' Validate whitespace in trimmed string
#' @param s Trimmed string
#' @keywords internal
validate_whitespace <- function(s) {
  chars <- utf8ToInt(s)
  invalid <- chars[chars < 32 | chars > 126]
  if (length(invalid) > 0) {
    mpc_error("Invalid character in designation")
  }
  if (grepl("  ", s, fixed = TRUE)) {
    mpc_error("Consecutive spaces in designation")
  }
}

#' Sanitize a designation string
#' @param designation Input designation
#' @return Trimmed string
#' @keywords internal
sanitize <- function(designation) {
  if (!is.character(designation) || length(designation) != 1) {
    mpc_error("Designation must be a single string")
  }
  validate_raw_input(designation)
  result <- trimws(designation)
  if (nchar(result) == 0) {
    mpc_error("Empty designation")
  }
  result
}

#' Check if letter is valid half-month
#' @param c Single character
#' @return Logical
#' @keywords internal
is_valid_half_month <- function(c) {
  c >= "A" && c <= "Y" && c != "I"
}

# =============================================================================
# Base-62 encoding utilities
# =============================================================================

#' Convert base-62 character to number
#' @param c Single character
#' @return Integer 0-61
#' @keywords internal
base62_to_num <- function(c) {
  idx <- regexpr(c, BASE62_CHARS, fixed = TRUE)[1]
  if (idx < 0) {
    mpc_error(sprintf("Invalid base-62 character: %s", c))
  }
  idx - 1  # 0-indexed
}

#' Convert number to base-62 character
#' @param n Integer 0-61
#' @return Single character
#' @keywords internal
num_to_base62 <- function(n) {

  if (n < 0 || n > 61) {
    mpc_error(sprintf("Number out of base-62 range: %d", n))
  }
  substr(BASE62_CHARS, n + 1, n + 1)
}

#' Convert base-62 string to number
#' @param s Base-62 string
#' @return Integer
#' @keywords internal
base62_string_to_num <- function(s) {
  result <- 0
  for (i in seq_len(nchar(s))) {
    result <- result * 62 + base62_to_num(substr(s, i, i))
  }
  result
}

#' Convert number to base-62 string
#' @param n Integer
#' @param width Output width
#' @return Base-62 string
#' @keywords internal
num_to_base62_string <- function(n, width) {
  result <- character(width)
  for (i in width:1) {
    result[i] <- num_to_base62(n %% 62)
    n <- n %/% 62
  }
  paste0(result, collapse = "")
}

# =============================================================================
# Cycle count encoding for provisional designations
# =============================================================================

#' Decode cycle count from packed format
#' @param encoded 2-character encoded string
#' @return Integer cycle count
#' @keywords internal
decode_cycle_count <- function(encoded) {
  if (nchar(encoded) < 2) {
    mpc_error("Invalid cycle count encoding")
  }

  first <- substr(encoded, 1, 1)
  second <- substr(encoded, 2, 2)

  tens <- if (grepl("^[0-9]$", first)) {
    as.integer(first)
  } else if (grepl("^[A-Z]$", first)) {
    utf8ToInt(first) - utf8ToInt("A") + 10
  } else if (grepl("^[a-z]$", first)) {
    utf8ToInt(first) - utf8ToInt("a") + 36
  } else {
    mpc_error("Invalid cycle count encoding")
  }

  if (!grepl("^[0-9]$", second)) {
    mpc_error("Invalid cycle count encoding")
  }

  tens * 10 + as.integer(second)
}

#' Encode cycle count for packed format
#' @param count Integer 0-619
#' @return 2-character encoded string
#' @keywords internal
encode_cycle_count <- function(count) {
  if (count < 0 || count >= 620) {
    mpc_error(sprintf("Cycle count out of range (0-619): %d", count))
  }

  tens <- count %/% 10
  ones <- count %% 10

  first <- if (tens < 10) {
    as.character(tens)
  } else if (tens < 36) {
    intToUtf8(utf8ToInt("A") + tens - 10)
  } else {
    intToUtf8(utf8ToInt("a") + tens - 36)
  }

  paste0(first, ones)
}

# =============================================================================
# Letter/position utilities for provisional designations
# =============================================================================

#' Convert half-month letter to position
#' @param letter Single uppercase letter
#' @return Integer position 1-25
#' @keywords internal
letter_to_position <- function(letter) {
  if (!grepl("^[A-Z]$", letter)) {
    mpc_error(sprintf("Invalid half-month letter: %s", letter))
  }
  pos <- utf8ToInt(letter) - utf8ToInt("A") + 1
  if (letter > "I") {
    pos <- pos - 1  # Skip I
  }
  pos
}

#' Convert position to half-month letter
#' @param pos Integer 1-25
#' @return Single uppercase letter
#' @keywords internal
position_to_letter <- function(pos) {
  if (pos < 1 || pos > 25) {
    mpc_error(sprintf("Invalid letter position: %d", pos))
  }
  p <- pos
  if (p >= 9) {
    p <- p + 1  # Skip I
  }
  intToUtf8(utf8ToInt("A") + p - 1)
}

# =============================================================================
# Permanent (numbered) asteroid designations
# =============================================================================

#' Unpack a permanent asteroid designation
#' @param packed Packed designation string
#' @return Integer asteroid number
#' @export
unpack_permanent <- function(packed) {
  p <- trimws(packed)
  if (nchar(p) != 5) {
    mpc_error("Invalid packed permanent designation length")
  }

  first <- substr(p, 1, 1)

  # Tilde format (>= 620,000)
  if (first == "~") {
    return(620000 + base62_string_to_num(substr(p, 2, 5)))
  }

  # Simple numeric format (< 100,000)
  if (grepl("^[0-9]$", first)) {
    return(as.integer(p))
  }

  # Extended format with uppercase letter (100,000 - 359,999)
  if (grepl("^[A-Z]$", first)) {
    val <- utf8ToInt(first) - 55  # A=10
    rest <- as.integer(substr(p, 2, 5))
    return(val * 10000 + rest)
  }

  # Extended format with lowercase letter (360,000 - 619,999)
  if (grepl("^[a-z]$", first)) {
    val <- utf8ToInt(first) - 61  # a=36
    rest <- as.integer(substr(p, 2, 5))
    return(val * 10000 + rest)
  }

  mpc_error("Invalid packed permanent designation")
}

#' Pack a permanent asteroid designation
#' @param number Integer asteroid number
#' @return Packed designation string
#' @export
pack_permanent <- function(number) {
  if (!is.numeric(number) || number < 1 || number > MAX_ASTEROID_NUMBER) {
    mpc_error(sprintf("Invalid asteroid number: %s", as.character(number)))
  }

  number <- as.integer(number)

  if (number < 100000) {
    return(sprintf("%05d", number))
  }

  if (number < 620000) {
    div_val <- number %/% 10000
    mod_val <- number %% 10000
    letter <- if (div_val < 36) {
      intToUtf8(div_val + 55)  # A-Z
    } else {
      intToUtf8(div_val + 61)  # a-z
    }
    return(sprintf("%s%04d", letter, mod_val))
  }

  # Tilde + base-62 format
  offset <- number - 620000
  paste0("~", num_to_base62_string(offset, 4))
}

# =============================================================================
# Standard provisional asteroid designations
# =============================================================================

#' Unpack a provisional asteroid designation
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_provisional <- function(packed) {
  p <- trimws(packed)

  # Check for survey designations first
  if (nchar(p) == 7) {
    prefix <- substr(p, 1, 3)
    if (prefix %in% names(SURVEY_PACKED_TO_UNPACKED)) {
      survey <- SURVEY_PACKED_TO_UNPACKED[[prefix]]
      num <- as.integer(substr(p, 4, 7))
      return(sprintf("%d %s", num, survey))
    }
  }

  if (nchar(p) != 7) {
    mpc_error("Invalid packed provisional designation length")
  }

  century <- substr(p, 1, 1)
  year <- substr(p, 2, 3)
  half_month <- substr(p, 4, 4)
  order_encoded <- substr(p, 5, 6)
  second_letter <- substr(p, 7, 7)

  if (!(century %in% names(CENTURY_CODES))) {
    mpc_error(sprintf("Invalid century code: %s", century))
  }

  full_year <- sprintf("%d%s", CENTURY_CODES[[century]], year)
  order_num <- decode_cycle_count(order_encoded)

  if (order_num == 0) {
    sprintf("%s %s%s", full_year, half_month, second_letter)
  } else {
    sprintf("%s %s%s%d", full_year, half_month, second_letter, order_num)
  }
}

#' Pack a provisional asteroid designation
#' @param unpacked Unpacked designation string
#' @return Packed designation string
#' @export
pack_provisional <- function(unpacked) {
  u <- trimws(unpacked)

  # Check for survey designations
  if (grepl("^\\d+ (P-L|T-[123])$", u)) {
    m <- regmatches(u, regexec("^(\\d+) (P-L|T-[123])$", u))[[1]]
    number <- as.integer(m[2])
    survey <- m[3]
    if (number < 1) {
      mpc_error("Survey number must be positive")
    }
    return(sprintf("%s%04d", SURVEY_UNPACKED_TO_PACKED[[survey]], number))
  }

  # Check for old-style designation: "A908 CJ" or "B842 FA"
  if (grepl("^[AB]\\d{3} [A-Z][A-Z]$", u)) {
    m <- regmatches(u, regexec("^[AB](\\d)(\\d{2}) ([A-Z])([A-Z])$", u))[[1]]
    century_digit <- m[2]
    year_short <- m[3]
    half_month <- m[4]
    second_letter <- m[5]

    century_code <- switch(century_digit,
      "8" = "I",
      "9" = "J",
      "0" = "K",
      mpc_error("Invalid century digit in old-style designation")
    )

    return(sprintf("%s%s%s00%s", century_code, year_short, half_month, second_letter))
  }

  # Match standard provisional: "1995 XA" or "1995 XA12"
  if (!grepl("^\\d{4} [A-Z][A-Z]\\d*$", u)) {
    mpc_error(sprintf("Invalid unpacked provisional designation: %s", u))
  }

  m <- regmatches(u, regexec("^(\\d{4}) ([A-Z])([A-Z])(\\d*)$", u))[[1]]
  year <- m[2]
  half_month <- m[3]
  second_letter <- m[4]
  order_str <- m[5]

  if (!is_valid_half_month(half_month)) {
    mpc_error(sprintf("Invalid half-month letter: %s", half_month))
  }

  century <- as.integer(substr(year, 1, 2))
  year_short <- substr(year, 3, 4)

  century_codes_rev <- setNames(names(CENTURY_CODES), CENTURY_CODES)
  if (!(as.character(century) %in% names(century_codes_rev))) {
    mpc_error(sprintf("Invalid century in year: %s", year))
  }

  century_code <- century_codes_rev[[as.character(century)]]
  order_num <- if (nchar(order_str) == 0) 0 else as.integer(order_str)

  # Check if we need extended format
  if (order_num >= 620) {
    return(pack_extended_provisional(as.integer(year), half_month, second_letter, order_num))
  }

  order_encoded <- encode_cycle_count(order_num)
  sprintf("%s%s%s%s%s", century_code, year_short, half_month, order_encoded, second_letter)
}

# =============================================================================
# Extended provisional format (cycle >= 620)
# =============================================================================

#' Pack extended provisional designation
#' @keywords internal
pack_extended_provisional <- function(year, half_month, second_letter, cycle) {
  year_short <- year %% 100
  letter_pos <- letter_to_position(second_letter)
  base_sequence <- (cycle - 620) * 25 + letter_pos - 1
  seq_encoded <- num_to_base62_string(base_sequence, 4)
  year_char <- num_to_base62(year_short)
  sprintf("_%s%s%s", year_char, half_month, seq_encoded)
}

#' Unpack extended provisional designation
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_extended_provisional <- function(packed) {
  p <- trimws(packed)
  if (nchar(p) != 7 || substr(p, 1, 1) != "_") {
    mpc_error("Invalid extended packed provisional")
  }

  year_digit <- substr(p, 2, 2)
  half_month <- substr(p, 3, 3)
  seq_encoded <- substr(p, 4, 7)

  base_sequence <- base62_string_to_num(seq_encoded)
  cycle <- 620 + base_sequence %/% 25
  letter_pos <- (base_sequence %% 25) + 1
  second_letter <- position_to_letter(letter_pos)

  year_val <- base62_to_num(year_digit)
  year <- 2000 + year_val

  sprintf("%d %s%s%d", year, half_month, second_letter, cycle)
}

# =============================================================================
# Comet provisional designations
# =============================================================================

#' Unpack a comet provisional designation
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_comet_provisional <- function(packed) {
  p <- trimws(packed)
  len <- nchar(p)

  if (len != 7 && len != 8) {
    mpc_error("Invalid packed comet provisional designation length")
  }

  century <- substr(p, 1, 1)
  year <- substr(p, 2, 3)
  half_month <- substr(p, 4, 4)
  order_encoded <- substr(p, 5, 6)
  fragment <- if (len == 7) substr(p, 7, 7) else substr(p, 7, 8)

  if (!(century %in% names(CENTURY_CODES))) {
    mpc_error(sprintf("Invalid century code: %s", century))
  }

  full_year <- sprintf("%d%s", CENTURY_CODES[[century]], year)
  order_num <- decode_cycle_count(order_encoded)

  result <- sprintf("%s %s%d", full_year, half_month, order_num)
  if (fragment != "0") {
    result <- sprintf("%s-%s", result, toupper(fragment))
  }

  result
}

#' Pack a comet provisional designation
#' @param unpacked Unpacked designation string
#' @return Packed designation string
#' @export
pack_comet_provisional <- function(unpacked) {
  u <- trimws(unpacked)

  # Match provisional comet: "1995 O1" or "1995 O1-B" or "1930 J1-AA"
  if (!grepl("^\\d{4} [A-Z]\\d+(-[A-Z]{1,2})?$", u)) {
    mpc_error(sprintf("Invalid unpacked comet provisional designation: %s", u))
  }

  m <- regmatches(u, regexec("^(\\d{4}) ([A-Z])(\\d+)(-([A-Z]{1,2}))?$", u))[[1]]
  year <- m[2]
  half_month <- m[3]
  order_str <- m[4]
  fragment <- if (length(m) >= 6 && nchar(m[6]) > 0) m[6] else NULL

  order_num <- as.integer(order_str)
  if (order_num < 1) {
    mpc_error("Comet order number must be positive")
  }

  century <- as.integer(substr(year, 1, 2))
  year_short <- substr(year, 3, 4)

  century_codes_rev <- setNames(names(CENTURY_CODES), CENTURY_CODES)
  if (!(as.character(century) %in% names(century_codes_rev))) {
    mpc_error(sprintf("Invalid century in year: %s", year))
  }

  century_code <- century_codes_rev[[as.character(century)]]
  order_encoded <- encode_cycle_count(order_num)
  fragment_code <- if (is.null(fragment)) "0" else tolower(fragment)

  sprintf("%s%s%s%s%s", century_code, year_short, half_month, order_encoded, fragment_code)
}

# =============================================================================
# Numbered comet designations
# =============================================================================

#' Unpack a numbered comet designation
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_comet_numbered <- function(packed) {
  p <- trimws(packed)

  if (!grepl("^\\d{4}[PD]$", p)) {
    mpc_error("Invalid packed numbered comet designation")
  }

  number <- as.integer(substr(p, 1, 4))
  comet_type <- substr(p, 5, 5)
  sprintf("%d%s", number, comet_type)
}

#' Pack a numbered comet designation
#' @param unpacked Unpacked designation string
#' @return Packed designation string
#' @export
pack_comet_numbered <- function(unpacked) {
  u <- trimws(unpacked)

  # Match "1P" or "354P" or "1P/Halley"
  if (!grepl("^\\d+[PD](/[A-Za-z].*)?$", u)) {
    mpc_error("Invalid unpacked numbered comet designation")
  }

  m <- regmatches(u, regexec("^(\\d+)([PD])", u))[[1]]
  number <- as.integer(m[2])
  comet_type <- m[3]

  if (number < 1 || number > 9999) {
    mpc_error(sprintf("Comet number out of range (1-9999): %d", number))
  }

  sprintf("%04d%s", number, comet_type)
}

# =============================================================================
# Natural satellite designations
# =============================================================================

#' Unpack a satellite designation
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_satellite <- function(packed) {
  p <- trimws(packed)

  if (nchar(p) != 8 || substr(p, 1, 1) != "S") {
    mpc_error("Invalid packed satellite designation")
  }

  century <- substr(p, 2, 2)
  year <- substr(p, 3, 4)
  planet <- substr(p, 5, 5)
  number_encoded <- substr(p, 6, 7)

  if (!(century %in% names(CENTURY_CODES))) {
    mpc_error(sprintf("Invalid century code: %s", century))
  }
  if (!(planet %in% SATELLITE_PLANETS)) {
    mpc_error(sprintf("Invalid planet code: %s", planet))
  }

  full_year <- sprintf("%d%s", CENTURY_CODES[[century]], year)
  number <- decode_cycle_count(number_encoded)

  sprintf("S/%s %s %d", full_year, planet, number)
}

#' Pack a satellite designation
#' @param unpacked Unpacked designation string
#' @return Packed designation string
#' @export
pack_satellite <- function(unpacked) {
  u <- trimws(unpacked)

  if (!grepl("^S/\\d{4} [JSUN] \\d+$", u)) {
    mpc_error("Invalid unpacked satellite designation")
  }

  m <- regmatches(u, regexec("^S/(\\d{4}) ([JSUN]) (\\d+)$", u))[[1]]
  year <- m[2]
  planet <- m[3]
  number <- as.integer(m[4])

  if (number < 1) {
    mpc_error("Satellite number must be positive")
  }

  century <- as.integer(substr(year, 1, 2))
  year_short <- substr(year, 3, 4)

  century_codes_rev <- setNames(names(CENTURY_CODES), CENTURY_CODES)
  if (!(as.character(century) %in% names(century_codes_rev))) {
    mpc_error(sprintf("Invalid century in year: %s", year))
  }

  century_code <- century_codes_rev[[as.character(century)]]
  number_encoded <- encode_cycle_count(number)

  sprintf("S%s%s%s%s0", century_code, year_short, planet, number_encoded)
}

# =============================================================================
# BCE year encoding for ancient comets
# =============================================================================

#' Encode BCE year
#' @keywords internal
encode_bce_year <- function(year) {
  if (year >= 0) {
    mpc_error(sprintf("Not a BCE year: %d", year))
  }

  abs_year <- abs(year)
  code <- 99 - (abs_year %% 100)

  prefix <- if (abs_year < 100) {
    "/"
  } else if (abs_year < 200) {
    "."
  } else if (abs_year < 300) {
    "-"
  } else {
    mpc_error(sprintf("BCE year out of supported range: %d", year))
  }

  list(prefix = prefix, code = sprintf("%02d", code))
}

#' Decode BCE year
#' @keywords internal
decode_bce_year <- function(prefix, code) {
  code_num <- as.integer(code)
  year_part <- 99 - code_num

  if (prefix == "/") {
    -year_part
  } else if (prefix == ".") {
    -(year_part + 100)
  } else if (prefix == "-") {
    -(year_part + 200)
  } else {
    mpc_error(sprintf("Invalid BCE prefix: %s", prefix))
  }
}

# =============================================================================
# Ancient/BCE comet provisional designations
# =============================================================================

#' Pack ancient comet provisional
#' @keywords internal
pack_ancient_comet_provisional <- function(comet_type, year, half_month, order_num, fragment) {
  order_encoded <- encode_cycle_count(order_num)
  fragment_code <- if (is.null(fragment) || nchar(fragment) == 0) "0" else tolower(fragment)

  if (year < 0) {
    bce <- encode_bce_year(year)
    sprintf("%s%s%s%s%s%s", comet_type, bce$prefix, bce$code, half_month, order_encoded, fragment_code)
  } else {
    sprintf("%s%03d%s%s%s", comet_type, year, half_month, order_encoded, fragment_code)
  }
}

#' Unpack ancient comet provisional
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_ancient_comet_provisional <- function(packed) {
  p <- trimws(packed)

  if (nchar(p) != 8) {
    mpc_error("Invalid ancient comet designation length")
  }

  comet_type <- substr(p, 1, 1)
  if (!(comet_type %in% COMET_TYPES)) {
    mpc_error(sprintf("Invalid comet type: %s", comet_type))
  }

  char2 <- substr(p, 2, 2)
  if (char2 %in% c("/", ".", "-")) {
    year <- decode_bce_year(char2, substr(p, 3, 4))
    half_month <- substr(p, 5, 5)
    order_encoded <- substr(p, 6, 7)
    fragment <- substr(p, 8, 8)
  } else {
    year <- as.integer(substr(p, 2, 4))
    half_month <- substr(p, 5, 5)
    order_encoded <- substr(p, 6, 7)
    fragment <- substr(p, 8, 8)
  }

  order_num <- decode_cycle_count(order_encoded)
  result <- sprintf("%s/%d %s%d", comet_type, year, half_month, order_num)

  if (fragment != "0") {
    result <- sprintf("%s-%s", result, toupper(fragment))
  }

  result
}

# =============================================================================
# Helper functions for comet format detection
# =============================================================================

#' Check if provisional uses asteroid-style packing
#' @keywords internal
is_asteroid_style_packed <- function(provisional_part) {
  if (nchar(provisional_part) != 7) return(FALSE)
  last_char <- substr(provisional_part, 7, 7)
  grepl("^[A-Z]$", last_char)
}

#' Check if provisional uses asteroid-style unpacking
#' @keywords internal
is_asteroid_style_unpacked <- function(provisional) {
  if (!grepl("^\\d{4} [A-Z].", provisional)) return(FALSE)
  second_char <- substr(provisional, 7, 7)
  grepl("^[A-Za-z]$", second_char)
}

# =============================================================================
# Full comet designations (with type prefix)
# =============================================================================

#' Unpack a full comet designation
#' @param packed Packed designation string
#' @return Unpacked designation string
#' @export
unpack_comet_full <- function(packed) {
  p <- packed
  len <- nchar(p)

  if (len == 8) {
    comet_type <- substr(p, 1, 1)
    provisional_part <- substr(p, 2, 8)

    if (!(comet_type %in% COMET_TYPES)) {
      mpc_error(sprintf("Invalid comet type: %s", comet_type))
    }

    provisional <- if (is_asteroid_style_packed(provisional_part)) {
      unpack_provisional(provisional_part)
    } else {
      unpack_comet_provisional(provisional_part)
    }

    return(sprintf("%s/%s", comet_type, provisional))
  }

  if (len == 9) {
    comet_type <- substr(p, 1, 1)
    provisional_part <- substr(p, 2, 9)

    if (!(comet_type %in% COMET_TYPES)) {
      mpc_error(sprintf("Invalid comet type: %s", comet_type))
    }

    provisional <- unpack_comet_provisional(provisional_part)
    return(sprintf("%s/%s", comet_type, provisional))
  }

  if (len == 12 || (len < 12 && substr(p, 1, 1) == " ")) {
    while (nchar(p) < 12) {
      p <- paste0(" ", p)
    }

    num_part <- trimws(substr(p, 1, 4))
    comet_type <- substr(p, 5, 5)
    provisional_part <- substr(p, 6, 12)

    if (!(comet_type %in% COMET_TYPES)) {
      mpc_error(sprintf("Invalid comet type: %s", comet_type))
    }

    provisional <- if (is_asteroid_style_packed(provisional_part)) {
      unpack_provisional(provisional_part)
    } else {
      unpack_comet_provisional(provisional_part)
    }

    if (nchar(num_part) == 0) {
      return(sprintf("%s/%s", comet_type, provisional))
    }

    num <- as.integer(num_part)
    return(sprintf("%d%s/%s", num, comet_type, provisional))
  }

  mpc_error("Invalid packed full comet designation length")
}

#' Pack a full comet designation
#' @param unpacked Unpacked designation string
#' @return Packed designation string
#' @export
pack_comet_full <- function(unpacked) {
  u <- trimws(unpacked)

  if (!grepl("^\\d*[PCDXAI]/-?\\d+ .+$", u)) {
    mpc_error(sprintf("Invalid unpacked comet designation: %s", u))
  }

  m <- regmatches(u, regexec("^(\\d*)([PCDXAI])/(-?\\d+) (.+)$", u))[[1]]
  number_str <- m[2]
  comet_type <- m[3]
  year <- as.integer(m[4])
  prov_part <- m[5]

  # Check for ancient or BCE year
  if (year < 1000) {
    if (grepl("^[A-Z]\\d+(-[A-Z])?$", prov_part)) {
      am <- regmatches(prov_part, regexec("^([A-Z])(\\d+)(-([A-Z]))?$", prov_part))[[1]]
      half_month <- am[2]
      order_num <- as.integer(am[3])
      fragment <- if (length(am) >= 5 && nchar(am[5]) > 0) am[5] else NULL
      return(pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment))
    } else {
      mpc_error(sprintf("Invalid ancient comet provisional: %s", prov_part))
    }
  }

  # Modern comet
  provisional <- sprintf("%d %s", year, prov_part)

  provisional_packed <- if (is_asteroid_style_unpacked(provisional)) {
    pack_provisional(provisional)
  } else {
    pack_comet_provisional(provisional)
  }

  if (nchar(number_str) == 0) {
    return(sprintf("%s%s", comet_type, provisional_packed))
  }

  num <- as.integer(number_str)
  if (num < 1 || num > 9999) {
    mpc_error(sprintf("Comet number out of range (1-9999): %d", num))
  }

  sprintf("%04d%s%s", num, comet_type, provisional_packed)
}

# =============================================================================
# Format detection
# =============================================================================

#' Detect the format of a designation
#' @param designation Input designation string
#' @return Named list with format, type, and subtype
#' @export
detect_format <- function(designation) {
  # Validate raw input BEFORE trimming
  validate_raw_input(designation)

  # Check for packed full comet designation BEFORE trimming (12 chars with spaces)
  if (nchar(designation) == 12) {
    if (grepl("^[ 0-9]{4}[PCDXAI][IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", designation)) {
      return(list(format = "packed", type = "comet_full",
                  subtype = "comet with provisional designation (12-char)"))
    }
  }

  # Check for packed comet designation (8 chars)
  if (nchar(designation) == 8 && substr(designation, 1, 1) %in% COMET_TYPES) {
    if (grepl("^[PCDXAI][A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z]$", designation)) {
      return(list(format = "packed", type = "comet_full",
                  subtype = "comet with provisional designation (8-char)"))
    }
  }

  # Check for packed comet with 2-letter fragment (9 chars)
  if (nchar(designation) == 9 && substr(designation, 1, 1) %in% COMET_TYPES) {
    if (grepl("^[PCDXAI][A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2}$", designation)) {
      return(list(format = "packed", type = "comet_full",
                  subtype = "comet with provisional designation (9-char, 2-letter fragment)"))
    }
  }

  # Check for packed ancient comet (8 chars)
  if (nchar(designation) == 8 && substr(designation, 1, 1) %in% COMET_TYPES) {
    if (grepl("^[PCDXAI][0-9]{3}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", designation)) {
      return(list(format = "packed", type = "comet_ancient",
                  subtype = "comet with ancient provisional (year < 1000)"))
    }
  }

  # Check for packed BCE comet (8 chars)
  if (nchar(designation) == 8 && substr(designation, 1, 1) %in% COMET_TYPES) {
    if (grepl("^[PCDXAI][/.\\-][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", designation)) {
      return(list(format = "packed", type = "comet_bce",
                  subtype = "comet with BCE provisional"))
    }
  }

  des <- trimws(designation)

  # Validate whitespace
  validate_whitespace(des)

  # Check for packed satellite designation (8 chars starting with S)
  if (nchar(des) == 8 && substr(des, 1, 1) == "S") {
    if (grepl("^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$", des)) {
      planet <- substr(des, 5, 5)
      planet_name <- SATELLITE_PLANET_NAMES[[planet]]
      return(list(format = "packed", type = "satellite",
                  subtype = sprintf("natural satellite (%s)", planet_name)))
    }
  }

  # Check for packed permanent (numbered) asteroid
  if (nchar(des) == 5) {
    if (substr(des, 1, 1) == "~") {
      if (grepl("^~[0-9A-Za-z]{4}$", des)) {
        return(list(format = "packed", type = "permanent",
                    subtype = "permanent numbered (tilde/base-62, >= 620000)"))
      }
    } else if (grepl("^[0-9]{5}$", des)) {
      return(list(format = "packed", type = "permanent",
                  subtype = "permanent numbered (5-digit, < 100000)"))
    } else if (grepl("^[A-Za-z][0-9]{4}$", des)) {
      first <- substr(des, 1, 1)
      subtype <- if (grepl("^[A-Z]$", first)) {
        "permanent numbered (letter-prefix, 100000-359999)"
      } else {
        "permanent numbered (letter-prefix, 360000-619999)"
      }
      return(list(format = "packed", type = "permanent", subtype = subtype))
    }

    # Check for packed numbered comet
    if (grepl("^[0-9]{4}[PD]$", des)) {
      comet_type <- substr(des, 5, 5)
      type_desc <- COMET_TYPE_DESCRIPTIONS[[comet_type]]
      return(list(format = "packed", type = "comet_numbered",
                  subtype = sprintf("comet numbered %s", type_desc)))
    }
  }

  # Check for packed provisional asteroid (7 chars)
  if (nchar(des) == 7) {
    if (substr(des, 1, 1) == "_") {
      if (grepl("^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$", des)) {
        return(list(format = "packed", type = "provisional_extended",
                    subtype = "provisional (extended format, cycle >=620)"))
      }
    }

    if (grepl("^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$", des)) {
      return(list(format = "packed", type = "provisional", subtype = "provisional"))
    }

    if (grepl("^PLS[0-9]{4}$", des)) {
      return(list(format = "packed", type = "survey", subtype = "survey (Palomar-Leiden)"))
    }

    if (grepl("^T[123]S[0-9]{4}$", des)) {
      return(list(format = "packed", type = "survey",
                  subtype = sprintf("survey (Trojan T-%s)", substr(des, 2, 2))))
    }

    if (grepl("^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$", des)) {
      return(list(format = "packed", type = "comet_provisional", subtype = "comet provisional"))
    }
  }

  # --- UNPACKED FORMATS ---

  # Check for unpacked satellite
  if (grepl("^S/\\d{4} [JSUN] \\d+$", des)) {
    m <- regmatches(des, regexec("^S/\\d{4} ([JSUN]) \\d+$", des))[[1]]
    planet <- m[2]
    planet_name <- SATELLITE_PLANET_NAMES[[planet]]
    return(list(format = "unpacked", type = "satellite",
                subtype = sprintf("natural satellite (%s)", planet_name)))
  }

  # Check for unpacked permanent (numbered) asteroid
  if (grepl("^[0-9]+$", des)) {
    return(list(format = "unpacked", type = "permanent", subtype = "permanent numbered"))
  }

  # Check for unpacked survey designation
  if (grepl("^\\d+ (P-L|T-[123])$", des)) {
    m <- regmatches(des, regexec("^\\d+ (P-L|T-[123])$", des))[[1]]
    survey <- m[2]
    subtype <- if (survey == "P-L") {
      "survey (Palomar-Leiden)"
    } else {
      sprintf("survey (Trojan %s)", survey)
    }
    return(list(format = "unpacked", type = "survey", subtype = subtype))
  }

  # Check for old-style asteroid designation
  if (grepl("^[AB]\\d{3} [A-Z][A-Z]$", des)) {
    return(list(format = "unpacked", type = "provisional",
                subtype = "provisional (old-style pre-1925)"))
  }

  # Check for unpacked provisional asteroid
  if (grepl("^\\d{4} [A-Z][A-Z]\\d*$", des)) {
    return(list(format = "unpacked", type = "provisional", subtype = "provisional"))
  }

  # Check for unpacked comet with type prefix
  if (grepl("^\\d*[PCDXAI]/-?\\d+ [A-Z][A-Z0-9]+(-[A-Z]{1,2})?$", des)) {
    m <- regmatches(des, regexec("^(\\d*)([PCDXAI])/(-?\\d+) [A-Z][A-Z0-9]+", des))[[1]]
    num <- m[2]
    ctype <- m[3]
    year <- as.integer(m[4])

    year_desc <- if (year < 0) {
      "BCE"
    } else if (year < 1000) {
      "ancient"
    } else {
      ""
    }

    type_desc <- COMET_TYPE_DESCRIPTIONS[[ctype]]

    subtype <- if (nchar(num) > 0) {
      if (nchar(year_desc) > 0) {
        sprintf("comet numbered with %s provisional (%s)", year_desc, type_desc)
      } else {
        sprintf("comet numbered with provisional (%s)", type_desc)
      }
    } else {
      if (nchar(year_desc) > 0) {
        sprintf("comet %s provisional (%s)", year_desc, type_desc)
      } else {
        sprintf("comet provisional (%s)", type_desc)
      }
    }

    return(list(format = "unpacked", type = "comet_full", subtype = subtype))
  }

  # Check for unpacked numbered periodic comet
  if (grepl("^\\d+[PD](/[A-Za-z].*)?$", des)) {
    m <- regmatches(des, regexec("^\\d+([PD])", des))[[1]]
    comet_type <- m[2]
    type_desc <- COMET_TYPE_DESCRIPTIONS[[comet_type]]
    return(list(format = "unpacked", type = "comet_numbered",
                subtype = sprintf("comet numbered %s", type_desc)))
  }

  mpc_error(sprintf("Unable to detect designation format: %s", designation))
}

# =============================================================================
# Main conversion functions
# =============================================================================

#' Convert a designation between packed and unpacked formats
#' @param designation Input designation string
#' @return Named list with input, output, and info
#' @export
convert <- function(designation) {
  info <- detect_format(designation)

  output <- if (info$format == "packed") {
    if (info$type == "permanent") {
      as.character(unpack_permanent(designation))
    } else if (info$type %in% c("provisional", "survey")) {
      unpack_provisional(designation)
    } else if (info$type == "provisional_extended") {
      unpack_extended_provisional(designation)
    } else if (info$type == "comet_numbered") {
      unpack_comet_numbered(designation)
    } else if (info$type == "comet_provisional") {
      unpack_comet_provisional(designation)
    } else if (info$type == "comet_full") {
      unpack_comet_full(designation)
    } else if (info$type %in% c("comet_ancient", "comet_bce")) {
      unpack_ancient_comet_provisional(designation)
    } else if (info$type == "satellite") {
      unpack_satellite(designation)
    } else {
      mpc_error(sprintf("Unknown type: %s", info$type))
    }
  } else {  # unpacked
    if (info$type == "permanent") {
      num <- suppressWarnings(as.integer(trimws(designation)))
      if (is.na(num)) {
        mpc_error(sprintf("Invalid asteroid number (overflow): %s", trimws(designation)))
      }
      if (num < 1 || num > MAX_ASTEROID_NUMBER) {
        mpc_error(sprintf("Invalid asteroid number: %d", num))
      }
      pack_permanent(num)
    } else if (info$type %in% c("provisional", "survey")) {
      pack_provisional(designation)
    } else if (info$type == "comet_numbered") {
      pack_comet_numbered(designation)
    } else if (info$type == "comet_full") {
      pack_comet_full(designation)
    } else if (info$type == "satellite") {
      pack_satellite(designation)
    } else {
      mpc_error(sprintf("Unknown type: %s", info$type))
    }
  }

  list(input = designation, output = output, info = info)
}

#' Convert a designation and return just the output string
#' @param designation Input designation string
#' @return Converted designation string
#' @export
convert_simple <- function(designation) {
  convert(designation)$output
}

#' Ensure a designation is in packed format
#' @param designation Input designation string
#' @return Packed designation string
#' @export
pack <- function(designation) {
  info <- detect_format(designation)
  if (info$format == "packed") {
    return(trimws(designation))
  }
  convert(designation)$output
}

#' Ensure a designation is in unpacked format
#' @param designation Input designation string
#' @return Unpacked designation string
#' @export
unpack <- function(designation) {
  info <- detect_format(designation)
  if (info$format == "unpacked") {
    return(trimws(designation))
  }
  convert(designation)$output
}

#' Check if a string is a valid MPC designation
#' @param designation Input string
#' @return Logical TRUE/FALSE
#' @export
is_valid_designation <- function(designation) {
  if (!is.character(designation) || length(designation) != 1 || nchar(designation) == 0) {
    return(FALSE)
  }
  tryCatch({
    detect_format(designation)
    TRUE
  }, error = function(e) {
    FALSE
  })
}
