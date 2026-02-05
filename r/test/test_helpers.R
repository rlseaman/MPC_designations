#!/usr/bin/env Rscript
#
# test_helpers.R - Test helper functions in MPC designation converter
#
# Tests format conversion (minimal <-> 12-char report format),
# fragment extraction, and designation comparison functions.
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

passed <- 0
failed <- 0

test_to_report <- function(input_val, expected, desc) {
  result <- tryCatch({
    to_report_format(input_val)
  }, error = function(e) {
    sprintf("ERROR: %s", conditionMessage(e))
  })

  if (result == expected) {
    cat(sprintf('  PASS: to_report_format("%s") -> "%s"\n', input_val, result))
    passed <<- passed + 1
  } else {
    cat(sprintf('  FAIL: to_report_format("%s"): expected "%s", got "%s"\n', input_val, expected, result))
    failed <<- failed + 1
  }
}

test_from_report <- function(input_val, expected, desc) {
  result <- tryCatch({
    from_report_format(input_val)
  }, error = function(e) {
    sprintf("ERROR: %s", conditionMessage(e))
  })

  if (result == expected) {
    cat(sprintf('  PASS: from_report_format("%s") -> "%s"\n', input_val, result))
    passed <<- passed + 1
  } else {
    cat(sprintf('  FAIL: from_report_format("%s"): expected "%s", got "%s"\n', input_val, expected, result))
    failed <<- failed + 1
  }
}

test_has_fragment <- function(input_val, expected, desc) {
  result <- has_fragment(input_val)
  if (result == expected) {
    cat(sprintf('  PASS: has_fragment("%s") -> %s\n', input_val, result))
    passed <<- passed + 1
  } else {
    cat(sprintf('  FAIL: has_fragment("%s"): expected %s, got %s\n', input_val, expected, result))
    failed <<- failed + 1
  }
}

test_get_fragment <- function(input_val, expected, desc) {
  result <- tryCatch({
    get_fragment(input_val)
  }, error = function(e) {
    sprintf("ERROR: %s", conditionMessage(e))
  })

  if (result == expected) {
    cat(sprintf('  PASS: get_fragment("%s") -> "%s"\n', input_val, result))
    passed <<- passed + 1
  } else {
    cat(sprintf('  FAIL: get_fragment("%s"): expected "%s", got "%s"\n', input_val, expected, result))
    failed <<- failed + 1
  }
}

test_get_parent <- function(input_val, expected, desc) {
  result <- tryCatch({
    get_parent(input_val)
  }, error = function(e) {
    sprintf("ERROR: %s", conditionMessage(e))
  })

  if (result == expected) {
    cat(sprintf('  PASS: get_parent("%s") -> "%s"\n', input_val, result))
    passed <<- passed + 1
  } else {
    cat(sprintf('  FAIL: get_parent("%s"): expected "%s", got "%s"\n', input_val, expected, result))
    failed <<- failed + 1
  }
}

test_equal <- function(d1, d2, expected, desc) {
  result <- designations_equal(d1, d2)
  if (result == expected) {
    cat(sprintf('  PASS: designations_equal("%s", "%s") -> %s\n', d1, d2, result))
    passed <<- passed + 1
  } else {
    cat(sprintf('  FAIL: designations_equal("%s", "%s"): expected %s, got %s\n', d1, d2, expected, result))
    failed <<- failed + 1
  }
}

cat("=== MPC Designation Helper Function Tests (R) ===\n\n")

# Test to_report_format
cat("--- to_report_format ---\n")

# Numbered asteroids
test_to_report("00001", "       00001", "Numbered asteroid 1")
test_to_report("00433", "       00433", "Numbered asteroid 433")
test_to_report("99999", "       99999", "Numbered asteroid 99999")
test_to_report("A0000", "       A0000", "Numbered asteroid 100000")
test_to_report("~0000", "       ~0000", "Numbered asteroid 620000")

# Provisional asteroids
test_to_report("J95X00A", "     J95X00A", "Provisional 1995 XA")
test_to_report("K24A12B", "     K24A12B", "Provisional 2024 AB12")

# Survey designations
test_to_report("PLS2040", "     PLS2040", "Survey P-L")
test_to_report("T3S3141", "     T3S3141", "Survey T-3")

# Numbered comets
test_to_report("0001P", "0001P       ", "Comet 1P")
test_to_report("0073P", "0073P       ", "Comet 73P")

# Numbered comets with fragments
test_to_report("0073Pa", "0073P      a", "Comet 73P-A")
test_to_report("0073Pb", "0073P      b", "Comet 73P-B")
test_to_report("0073Paa", "0073P     aa", "Comet 73P-AA")
test_to_report("0073Paz", "0073P     az", "Comet 73P-AZ")
test_to_report("0073Pzz", "0073P     zz", "Comet 73P-ZZ")

# Provisional comets
test_to_report("CJ95O010", "    CJ95O010", "Comet C/1995 O1")
test_to_report("DJ93F020", "    DJ93F020", "Comet D/1993 F2")
test_to_report("DJ93F02a", "    DJ93F02a", "Comet D/1993 F2-A")

# Test from_report_format
cat("\n--- from_report_format ---\n")

# Numbered asteroids
test_from_report("       00001", "00001", "Numbered asteroid 1")
test_from_report("       00433", "00433", "Numbered asteroid 433")
test_from_report("       A0000", "A0000", "Numbered asteroid 100000")

# Provisional asteroids
test_from_report("     J95X00A", "J95X00A", "Provisional 1995 XA")

# Numbered comets
test_from_report("0073P       ", "0073P", "Comet 73P")

# Numbered comets with fragments
test_from_report("0073P      a", "0073Pa", "Comet 73P-A")
test_from_report("0073P     aa", "0073Paa", "Comet 73P-AA")
test_from_report("0073P     az", "0073Paz", "Comet 73P-AZ")

# Provisional comets
test_from_report("    CJ95O010", "CJ95O010", "Comet C/1995 O1")

# Test has_fragment
cat("\n--- has_fragment ---\n")

# Unpacked with fragments
test_has_fragment("73P-A", TRUE, "Unpacked numbered comet with fragment")
test_has_fragment("73P-AA", TRUE, "Unpacked numbered comet with 2-letter fragment")
test_has_fragment("D/1993 F2-A", TRUE, "Unpacked provisional comet with fragment")
test_has_fragment("P/1930 J1-AA", TRUE, "Unpacked provisional comet with 2-letter fragment")

# Unpacked without fragments
test_has_fragment("73P", FALSE, "Unpacked numbered comet no fragment")
test_has_fragment("C/1995 O1", FALSE, "Unpacked provisional comet no fragment")

# Packed with fragments
test_has_fragment("0073Pa", TRUE, "Packed numbered comet with fragment")
test_has_fragment("0073Paa", TRUE, "Packed numbered comet with 2-letter fragment")
test_has_fragment("DJ93F02a", TRUE, "Packed provisional comet with fragment")

# Packed without fragments
test_has_fragment("0073P", FALSE, "Packed numbered comet no fragment")
test_has_fragment("CJ95O010", FALSE, "Packed provisional comet no fragment")

# Non-comets
test_has_fragment("1995 XA", FALSE, "Asteroid no fragment")
test_has_fragment("00001", FALSE, "Numbered asteroid")

# Test get_fragment
cat("\n--- get_fragment ---\n")

# Unpacked with fragments
test_get_fragment("73P-A", "A", "Unpacked single fragment")
test_get_fragment("73P-AA", "AA", "Unpacked 2-letter fragment")
test_get_fragment("73P-I", "I", "Unpacked fragment I")
test_get_fragment("D/1993 F2-B", "B", "Unpacked provisional fragment")
test_get_fragment("P/1930 J1-AZ", "AZ", "Unpacked provisional 2-letter")

# Unpacked without fragments
test_get_fragment("73P", "", "Unpacked no fragment")
test_get_fragment("C/1995 O1", "", "Unpacked provisional no fragment")

# Packed with fragments
test_get_fragment("0073Pa", "A", "Packed single fragment")
test_get_fragment("0073Paa", "AA", "Packed 2-letter fragment")
test_get_fragment("0073Pi", "I", "Packed fragment I")
test_get_fragment("DJ93F02b", "B", "Packed provisional fragment")

# Packed without fragments
test_get_fragment("0073P", "", "Packed no fragment")
test_get_fragment("CJ95O010", "", "Packed provisional no fragment")

# Test get_parent
cat("\n--- get_parent ---\n")

# Unpacked with fragments
test_get_parent("73P-A", "73P", "Unpacked single fragment")
test_get_parent("73P-AA", "73P", "Unpacked 2-letter fragment")
test_get_parent("D/1993 F2-B", "D/1993 F2", "Unpacked provisional fragment")
test_get_parent("P/1930 J1-AA", "P/1930 J1", "Unpacked provisional 2-letter")

# Unpacked without fragments
test_get_parent("73P", "73P", "Unpacked no fragment")
test_get_parent("C/1995 O1", "C/1995 O1", "Unpacked provisional no fragment")

# Packed with fragments
test_get_parent("0073Pa", "0073P", "Packed single fragment")
test_get_parent("0073Paa", "0073P", "Packed 2-letter fragment")

# Packed without fragments
test_get_parent("0073P", "0073P", "Packed no fragment")

# Non-comets (should return as-is)
test_get_parent("1995 XA", "1995 XA", "Asteroid")
test_get_parent("00001", "00001", "Numbered asteroid")

# Test designations_equal
cat("\n--- designations_equal ---\n")

# Same designation, different formats
test_equal("1995 XA", "J95X00A", TRUE, "Provisional packed/unpacked")
test_equal("73P", "0073P", TRUE, "Numbered comet packed/unpacked")
test_equal("73P-A", "0073Pa", TRUE, "Comet with fragment packed/unpacked")
test_equal("73P-AA", "0073Paa", TRUE, "Comet with 2-letter fragment")
test_equal("1", "00001", TRUE, "Numbered asteroid")
test_equal("C/1995 O1", "CJ95O010", TRUE, "Provisional comet")

# Different designations
test_equal("1995 XA", "1995 XB", FALSE, "Different provisional")
test_equal("73P-A", "73P-B", FALSE, "Different fragments")
test_equal("73P", "74P", FALSE, "Different comet numbers")
test_equal("1", "2", FALSE, "Different asteroid numbers")

# Same designation (both packed or both unpacked)
test_equal("1995 XA", "1995 XA", TRUE, "Same unpacked")
test_equal("J95X00A", "J95X00A", TRUE, "Same packed")

# Summary
cat("\n==================================================\n")
cat(sprintf("Total: %d, Passed: %d, Failed: %d\n", passed + failed, passed, failed))

if (failed > 0) {
  quit(status = 1)
}
