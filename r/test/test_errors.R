#!/usr/bin/env Rscript
#
# test_errors.R - Error handling tests for MPC designation converter
#
# Reads test-data/error_test_cases.csv (columns: category, subcategory,
# input, expected_error, description). Rows with expected_error == "valid"
# must convert successfully; all other rows must raise an error.
# Escape sequences in the input column (\t, \n, \r, \f, \v, \0, \\, \xNN)
# are decoded before testing.
#

# Source the library - find it relative to this script
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
if (length(script_path) == 0) {
  script_dir <- "test"
} else {
  script_dir <- dirname(script_path)
}
source(file.path(script_dir, "..", "src", "mpc_designation.R"))

# Find the error test cases file
test_data_file <- file.path(script_dir, "..", "..", "test-data", "error_test_cases.csv")

if (!file.exists(test_data_file)) {
  cat(sprintf("Error: Test data file not found: %s\n", test_data_file), file = stderr())
  quit(status = 1)
}

# Expected skips: R character strings cannot contain embedded NUL bytes,
# so these inputs cannot be constructed at all.
expected_skips <- c(
  "invalid_char/null_byte"  = "R strings cannot contain NUL bytes",
  "edge_case/null_middle"   = "R strings cannot contain NUL bytes"
)

# Decode escape sequences into a raw byte vector, then into a string
unescape_input <- function(s) {
  chars <- strsplit(s, "")[[1]]
  out <- raw(0)
  i <- 1
  n <- length(chars)
  simple <- c(n = 10L, r = 13L, t = 9L, f = 12L, v = 11L, "0" = 0L, "\\" = 92L)
  while (i <= n) {
    ch <- chars[i]
    if (ch == "\\" && i < n) {
      nxt <- chars[i + 1]
      if (nxt == "x" && i + 3 <= n) {
        out <- c(out, as.raw(strtoi(paste0(chars[i + 2], chars[i + 3]), 16L)))
        i <- i + 4
        next
      } else if (nxt %in% names(simple)) {
        out <- c(out, as.raw(simple[[nxt]]))
        i <- i + 2
        next
      }
    }
    out <- c(out, charToRaw(ch))
    i <- i + 1
  }
  out
}

cat("=== Error Handling Tests ===\n\n")

lines <- readLines(test_data_file, warn = FALSE)

passed <- 0
failed <- 0
skipped <- 0

for (line in lines) {
  line <- trimws(line)
  if (nchar(line) == 0 || startsWith(line, "#") || startsWith(line, "category,")) next

  parts <- strsplit(line, ",", fixed = TRUE)[[1]]
  if (length(parts) < 5) next
  category <- parts[1]
  subcategory <- parts[2]
  expected <- parts[4]
  description <- paste(parts[5:length(parts)], collapse = ",")

  key <- paste0(category, "/", subcategory)
  if (key %in% names(expected_skips)) {
    skipped <- skipped + 1
    cat(sprintf("SKIP [%s]: %s\n", key, expected_skips[[key]]))
    next
  }

  bytes <- unescape_input(parts[3])
  if (any(bytes == as.raw(0))) {
    skipped <- skipped + 1
    cat(sprintf("SKIP [%s]: input contains NUL byte (not representable in R)\n", key))
    next
  }
  input <- rawToChar(bytes)

  err_msg <- NULL
  output <- tryCatch(convert_simple(input), error = function(e) {
    err_msg <<- conditionMessage(e)
    NULL
  })
  got_error <- !is.null(err_msg)

  if (expected == "valid") {
    if (!got_error) {
      passed <- passed + 1
    } else {
      failed <- failed + 1
      cat(sprintf("FAIL [%s]: '%s'\n", key, description))
      cat("      Expected: valid conversion\n")
      cat(sprintf("      Got:      %s\n", err_msg))
    }
  } else {
    if (got_error) {
      passed <- passed + 1
    } else {
      failed <- failed + 1
      cat(sprintf("FAIL [%s]: '%s'\n", key, description))
      cat(sprintf("      Expected: error (%s)\n", expected))
      cat(sprintf("      Got:      '%s' (success)\n", output))
    }
  }
}

cat("\n=== Error Test Results ===\n")
cat(sprintf("Total:  %d\n", passed + failed + skipped))
cat(sprintf("Passed: %d\n", passed))
cat(sprintf("Failed: %d\n", failed))
cat(sprintf("Skipped: %d\n", skipped))

if (failed > 0) {
  quit(status = 1)
} else {
  cat("\nAll error tests passed!\n")
}
