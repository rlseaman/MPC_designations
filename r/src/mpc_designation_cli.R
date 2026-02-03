#!/usr/bin/env Rscript
#
# mpc_designation_cli.R - Command-line interface for MPC designation converter
#
# Usage: Rscript mpc_designation_cli.R [options] designation [designation ...]
#
# Options:
#   -v, --verbose   Show detailed information about the conversion
#   -h, --help      Show this help message
#

# Get the directory where this script is located
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
if (length(script_path) == 0) {
  script_dir <- "src"
} else {
  script_dir <- dirname(script_path)
}

# Source the library
source(file.path(script_dir, "mpc_designation.R"))

print_usage <- function() {
  cat("Usage: Rscript mpc_designation_cli.R [-v|--verbose] <designation> [designation ...]\n")
  cat("\n")
  cat("Convert between packed and unpacked MPC designations.\n")
  cat("Auto-detects the input format and converts to the other.\n")
  cat("\n")
  cat("Options:\n")
  cat("  -v, --verbose   Show detailed information about the conversion\n")
  cat("  -h, --help      Show this help message\n")
  cat("\n")
  cat("Examples:\n")
  cat("  Rscript mpc_designation_cli.R 00001             -> 1\n")
  cat("  Rscript mpc_designation_cli.R 1                 -> 00001\n")
  cat("  Rscript mpc_designation_cli.R J95X00A           -> 1995 XA\n")
  cat("  Rscript mpc_designation_cli.R '1995 XA'         -> J95X00A\n")
  cat("  Rscript mpc_designation_cli.R 'C/1995 O1'       -> CJ95O010\n")
  cat("  Rscript mpc_designation_cli.R 1P                -> 0001P\n")
  cat("  Rscript mpc_designation_cli.R 'D/1993 F2-B'     -> DJ93F02b (fragment B)\n")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    print_usage()
    quit(status = 1)
  }

  verbose <- FALSE
  designations <- character(0)

  for (arg in args) {
    if (arg %in% c("-v", "--verbose")) {
      verbose <- TRUE
    } else if (arg %in% c("-h", "--help")) {
      print_usage()
      quit(status = 0)
    } else {
      designations <- c(designations, arg)
    }
  }

  if (length(designations) == 0) {
    print_usage()
    quit(status = 1)
  }

  multiple <- length(designations) > 1

  for (des in designations) {
    result <- tryCatch({
      convert(des)
    }, error = function(e) {
      cat(sprintf("Error: %s\n", conditionMessage(e)), file = stderr())
      quit(status = 1)
    })

    info <- result$info
    output <- result$output

    if (verbose) {
      cat(sprintf("  Input:    %s\n", des))
      cat(sprintf("  Detected: %s format, %s\n", info$format, info$subtype))
      action <- if (info$format == "packed") {
        "unpacking to human-readable form"
      } else {
        "packing to MPC compact form"
      }
      cat(sprintf("  Action:   %s\n", action))
      cat(sprintf("  Output:   %s\n", output))
      if (multiple) cat("\n")
    } else if (multiple) {
      cat(sprintf("%s -> %s\n", des, output))
    } else {
      cat(sprintf("%s\n", output))
    }
  }
}

# Run if executed directly
if (!interactive()) {
  main()
}
