// CLI for MPC designation converter
package main

import (
	"fmt"
	"os"

	"github.com/rlseaman/mpc_designations/go/mpc"
)

func printUsage() {
	fmt.Fprintln(os.Stderr, `Usage: mpc_designation [-v|--verbose] <designation> [designation ...]

Convert between packed and unpacked MPC designations.
Auto-detects the input format and converts to the other.

Options:
  -v, --verbose   Show detailed information about the conversion
  --version       Show version information

Examples:
  mpc_designation 00001             -> 1
  mpc_designation 1                 -> 00001
  mpc_designation J95X00A           -> 1995 XA
  mpc_designation '1995 XA'         -> J95X00A
  mpc_designation 'C/1995 O1'       -> CJ95O010
  mpc_designation 1P                -> 0001P`)
}

func main() {
	args := os.Args[1:]

	if len(args) == 0 {
		printUsage()
		os.Exit(1)
	}

	verbose := false
	var designations []string

	for _, arg := range args {
		switch arg {
		case "-v", "--verbose":
			verbose = true
		case "-h", "--help":
			printUsage()
			os.Exit(0)
		case "--version":
			fmt.Printf("mpc_designation %s\n", mpc.Version)
			os.Exit(0)
		default:
			designations = append(designations, arg)
		}
	}

	if len(designations) == 0 {
		printUsage()
		os.Exit(1)
	}

	multiple := len(designations) > 1

	for _, des := range designations {
		result, err := mpc.Convert(des)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}

		if verbose {
			fmt.Printf("  Input:    %s\n", des)
			fmt.Printf("  Detected: %s format, %s\n", result.Info.Format, result.Info.Subtype)
			action := "unpacking to human-readable form"
			if result.Info.Format == mpc.FormatUnpacked {
				action = "packing to MPC compact form"
			}
			fmt.Printf("  Action:   %s\n", action)
			fmt.Printf("  Output:   %s\n", result.Output)
			if multiple {
				fmt.Println()
			}
		} else if multiple {
			fmt.Printf("%s -> %s\n", des, result.Output)
		} else {
			fmt.Println(result.Output)
		}
	}
}
