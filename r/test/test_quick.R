#!/usr/bin/env Rscript
#
# test_quick.R - Quick validation tests for MPC designation converter
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

# Test counter
tests_passed <- 0
tests_failed <- 0

test <- function(name, expr) {
  result <- tryCatch({
    if (expr) {
      tests_passed <<- tests_passed + 1
      cat(sprintf("  PASS: %s\n", name))
      TRUE
    } else {
      tests_failed <<- tests_failed + 1
      cat(sprintf("  FAIL: %s\n", name))
      FALSE
    }
  }, error = function(e) {
    tests_failed <<- tests_failed + 1
    cat(sprintf("  FAIL: %s - %s\n", name, conditionMessage(e)))
    FALSE
  })
}

cat("=== Quick Validation Tests ===\n\n")

# Permanent asteroid tests
cat("Permanent asteroids:\n")
test("pack 1 -> 00001", pack_permanent(1) == "00001")
test("pack 99999 -> 99999", pack_permanent(99999) == "99999")
test("pack 100000 -> A0000", pack_permanent(100000) == "A0000")
test("pack 100001 -> A0001", pack_permanent(100001) == "A0001")
test("pack 360000 -> a0000", pack_permanent(360000) == "a0000")
test("pack 620000 -> ~0000", pack_permanent(620000) == "~0000")
test("unpack 00001 -> 1", unpack_permanent("00001") == 1)
test("unpack A0001 -> 100001", unpack_permanent("A0001") == 100001)
test("unpack ~0000 -> 620000", unpack_permanent("~0000") == 620000)
cat("\n")

# Provisional asteroid tests
cat("Provisional asteroids:\n")
test("pack 1995 XA -> J95X00A", pack_provisional("1995 XA") == "J95X00A")
test("pack 2024 AB1 -> K24A01B", pack_provisional("2024 AB1") == "K24A01B")
test("pack 2040 P-L -> PLS2040", pack_provisional("2040 P-L") == "PLS2040")
test("unpack J95X00A -> 1995 XA", unpack_provisional("J95X00A") == "1995 XA")
test("unpack K24A01B -> 2024 AB1", unpack_provisional("K24A01B") == "2024 AB1")
test("unpack PLS2040 -> 2040 P-L", unpack_provisional("PLS2040") == "2040 P-L")
cat("\n")

# Extended provisional tests
cat("Extended provisional:\n")
test("pack 2024 AB631 -> _OA004S", pack_provisional("2024 AB631") == "_OA004S")
test("pack 2024 AA631 -> _OA004R", pack_provisional("2024 AA631") == "_OA004R")
test("unpack _OA004S -> 2024 AB631", unpack_extended_provisional("_OA004S") == "2024 AB631")
test("unpack _OA004R -> 2024 AA631", unpack_extended_provisional("_OA004R") == "2024 AA631")
cat("\n")

# Comet tests
cat("Comets:\n")
test("pack 1P -> 0001P", pack_comet_numbered("1P") == "0001P")
test("pack C/1995 O1 -> CJ95O010", pack_comet_full("C/1995 O1") == "CJ95O010")
test("pack D/1993 F2-B -> DJ93F02b", pack_comet_full("D/1993 F2-B") == "DJ93F02b")
test("unpack 0001P -> 1P", unpack_comet_numbered("0001P") == "1P")
test("unpack CJ95O010 -> C/1995 O1", unpack_comet_full("CJ95O010") == "C/1995 O1")
test("unpack DJ93F02b -> D/1993 F2-B", unpack_comet_full("DJ93F02b") == "D/1993 F2-B")
cat("\n")

# Satellite tests
cat("Satellites:\n")
test("pack S/2019 S 22 -> SK19S220", pack_satellite("S/2019 S 22") == "SK19S220")
test("pack S/2003 J 2 -> SK03J020", pack_satellite("S/2003 J 2") == "SK03J020")
test("unpack SK19S220 -> S/2019 S 22", unpack_satellite("SK19S220") == "S/2019 S 22")
test("unpack SK03J020 -> S/2003 J 2", unpack_satellite("SK03J020") == "S/2003 J 2")
cat("\n")

# High-level API tests
cat("High-level API:\n")
test("convert_simple 1995 XA -> J95X00A", convert_simple("1995 XA") == "J95X00A")
test("convert_simple J95X00A -> 1995 XA", convert_simple("J95X00A") == "1995 XA")
test("pack already packed", pack("J95X00A") == "J95X00A")
test("unpack already unpacked", unpack("1995 XA") == "1995 XA")
test("is_valid_designation valid", is_valid_designation("1995 XA"))
test("is_valid_designation invalid", !is_valid_designation("invalid"))
cat("\n")

# Format detection tests
cat("Format detection:\n")
info <- detect_format("1995 XA")
test("detect unpacked provisional", info$format == "unpacked" && info$type == "provisional")
info <- detect_format("J95X00A")
test("detect packed provisional", info$format == "packed" && info$type == "provisional")
info <- detect_format("1")
test("detect unpacked permanent", info$format == "unpacked" && info$type == "permanent")
info <- detect_format("00001")
test("detect packed permanent", info$format == "packed" && info$type == "permanent")
cat("\n")

# Summary
cat(sprintf("=== Results: %d passed, %d failed ===\n", tests_passed, tests_failed))

if (tests_failed > 0) {
  quit(status = 1)
}
