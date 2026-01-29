// Test MPC designation error handling
package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/rlseaman/mpc_designations/go/mpc"
)

// unescapeInput converts escape sequences in test input
func unescapeInput(input string) string {
	result := input
	result = strings.ReplaceAll(result, "\\t", "\t")
	result = strings.ReplaceAll(result, "\\n", "\n")
	result = strings.ReplaceAll(result, "\\r", "\r")
	result = strings.ReplaceAll(result, "\\f", "\f")
	result = strings.ReplaceAll(result, "\\v", "\v")

	// Handle \x00 style escapes
	for i := 0; i < len(result)-3; i++ {
		if result[i] == '\\' && result[i+1] == 'x' {
			hexStr := result[i+2 : i+4]
			var val int
			if _, err := fmt.Sscanf(hexStr, "%x", &val); err == nil {
				result = result[:i] + string(rune(val)) + result[i+4:]
			}
		}
	}

	return result
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: test_errors <error_test_cases.csv>")
		os.Exit(1)
	}

	csvFile := os.Args[1]
	file, err := os.Open(csvFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Cannot open file: %s\n", csvFile)
		os.Exit(1)
	}
	defer file.Close()

	var totalTests, passedTests, failedTests int

	fmt.Println("=== MPC Designation Error Tests ===")
	fmt.Println()

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()

		// Skip empty lines, comments, and header
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, "category,") {
			continue
		}

		parts := strings.SplitN(line, ",", 5)
		if len(parts) < 5 {
			continue
		}

		category := parts[0]
		subcategory := parts[1]
		rawInput := parts[2]
		expectedError := parts[3]
		description := parts[4]

		input := unescapeInput(rawInput)

		totalTests++

		result, err := mpc.Convert(input)
		gotError := err != nil
		var errorType string
		if gotError {
			errStr := err.Error()
			if strings.Contains(errStr, "out of range") {
				errorType = "range"
			} else {
				errorType = "format"
			}
		}

		testID := fmt.Sprintf("%s/%s", category, subcategory)

		if expectedError == "valid" {
			// Should succeed
			if gotError {
				failedTests++
				fmt.Printf("FAIL [%s]: '%s'\n", testID, description)
				fmt.Printf("      Expected: valid conversion\n")
				fmt.Printf("      Got:      error (%s)\n", errorType)
			} else {
				passedTests++
			}
		} else {
			// Should fail
			if !gotError {
				failedTests++
				fmt.Printf("FAIL [%s]: '%s'\n", testID, description)
				fmt.Printf("      Expected: error (%s)\n", expectedError)
				fmt.Printf("      Got:      '%s' (success)\n", result.Output)
			} else {
				passedTests++
			}
		}
	}

	fmt.Println()
	fmt.Println("=== Error Test Results ===")
	fmt.Printf("Total:  %d\n", totalTests)
	fmt.Printf("Passed: %d\n", passedTests)
	fmt.Printf("Failed: %d\n", failedTests)

	if failedTests > 0 {
		os.Exit(1)
	}
}
