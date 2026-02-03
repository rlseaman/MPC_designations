#!/usr/bin/env Rscript
#
# test_csv.R - CSV test suite for MPC designation converter
#
# Tests against the full 2M+ conversion test cases
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

# Find the test data file
test_data_file <- file.path(script_dir, "..", "..", "test-data", "prov_unpack_to_pack.csv.gz")

if (!file.exists(test_data_file)) {
  cat(sprintf("Error: Test data file not found: %s\n", test_data_file), file = stderr())
  quit(status = 1)
}

cat("Loading test data...\n")

# Read gzipped CSV
con <- gzfile(test_data_file, "rt")
lines <- readLines(con)
close(con)

# Skip header
lines <- lines[-1]
total <- length(lines)

cat(sprintf("Testing %d conversions...\n", total))

passed <- 0
failed <- 0
errors <- character(0)

start_time <- Sys.time()

for (i in seq_along(lines)) {
  parts <- strsplit(lines[i], ",")[[1]]
  if (length(parts) < 2) next

  unpacked <- parts[1]
  expected_packed <- parts[2]

  result <- tryCatch({
    pack(unpacked)
  }, error = function(e) {
    NULL
  })

  if (!is.null(result) && result == expected_packed) {
    passed <- passed + 1
  } else {
    failed <- failed + 1
    if (length(errors) < 10) {
      actual <- if (is.null(result)) "ERROR" else result
      errors <- c(errors, sprintf("  %s -> %s (expected %s)", unpacked, actual, expected_packed))
    }
  }

  # Progress update every 100k
  if (i %% 100000 == 0) {
    cat(sprintf("  Progress: %d / %d (%.1f%%)\n", i, total, 100 * i / total))
  }
}

end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n")
cat(sprintf("=== Results ===\n"))
cat(sprintf("Passed: %d / %d\n", passed, total))
cat(sprintf("Failed: %d\n", failed))
time_ms <- round(elapsed * 1000)
rate <- total / elapsed
cat(sprintf("Time: %dms (%.0f entries/sec)\n", time_ms, rate))

if (failed > 0) {
  cat("\nFirst failures:\n")
  for (err in errors) {
    cat(sprintf("%s\n", err))
  }
  quit(status = 1)
} else {
  cat("\nAll tests passed!\n")
}
