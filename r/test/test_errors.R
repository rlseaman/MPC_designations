#!/usr/bin/env Rscript
#
# test_errors.R - Error handling tests for MPC designation converter
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

cat("=== Error Handling Tests ===\n\n")

# Read CSV
lines <- readLines(test_data_file)

# Skip header
lines <- lines[-1]

passed <- 0
failed <- 0
errors <- character(0)

for (line in lines) {
  # Parse CSV (handle quoted fields)
  if (grepl('^"', line)) {
    # Quoted field
    m <- regmatches(line, regexec('^"([^"]*)",(.*)$', line))[[1]]
    if (length(m) >= 3) {
      input <- m[2]
      expected_error <- m[3]
    } else {
      next
    }
  } else {
    parts <- strsplit(line, ",", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    input <- parts[1]
    expected_error <- parts[2]
  }

  # Test that it produces an error
  result <- tryCatch({
    convert_simple(input)
    "NO_ERROR"
  }, error = function(e) {
    "ERROR"
  })

  if (result == "ERROR") {
    passed <- passed + 1
  } else {
    failed <- failed + 1
    if (length(errors) < 10) {
      errors <- c(errors, sprintf("  '%s' should error but got: %s", input, result))
    }
  }
}

cat(sprintf("Passed: %d / %d\n", passed, passed + failed))
cat(sprintf("Failed: %d\n", failed))

if (failed > 0) {
  cat("\nFailures:\n")
  for (err in errors) {
    cat(sprintf("%s\n", err))
  }
  quit(status = 1)
} else {
  cat("\nAll error tests passed!\n")
}
