// Test MPC designation conversion against CSV test data
package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/rlseaman/mpc_designations/go/mpc"
)

type testError struct {
	unpacked string
	got      string
	expected string
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: test_csv <csv_file>")
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
	var errors []testError
	const maxErrors = 100

	startTime := time.Now()

	scanner := bufio.NewScanner(file)

	// Skip header
	if scanner.Scan() {
		// Header skipped
	}

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		parts := strings.SplitN(line, ",", 3)
		if len(parts) < 2 {
			continue
		}

		unpacked := parts[0]
		expectedPacked := parts[1]

		totalTests++

		gotPacked, err := mpc.ConvertSimple(unpacked)
		if err != nil {
			failedTests++
			if len(errors) < maxErrors {
				errors = append(errors, testError{unpacked, fmt.Sprintf("ERROR: %v", err), expectedPacked})
			}
		} else if gotPacked != expectedPacked {
			failedTests++
			if len(errors) < maxErrors {
				errors = append(errors, testError{unpacked, gotPacked, expectedPacked})
			}
		} else {
			passedTests++
		}

		if totalTests%100000 == 0 {
			fmt.Printf("Processed %d entries...\n", totalTests)
		}
	}

	elapsed := time.Since(startTime)
	elapsedMs := elapsed.Milliseconds()
	rate := float64(totalTests) / elapsed.Seconds()

	fmt.Println()
	fmt.Println("=== Test Results ===")
	fmt.Printf("Total:  %d\n", totalTests)
	fmt.Printf("Passed: %d\n", passedTests)
	fmt.Printf("Failed: %d\n", failedTests)
	fmt.Printf("Time:   %dms (%.1f entries/sec)\n", elapsedMs, rate)

	if failedTests > 0 && len(errors) > 0 {
		fmt.Printf("\n=== First %d failures ===\n", len(errors))
		for _, e := range errors {
			fmt.Printf("%-25s %-15s %-15s\n", e.unpacked, e.got, e.expected)
		}
	}

	if failedTests > 0 {
		os.Exit(1)
	}
}
