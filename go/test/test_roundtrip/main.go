// Test MPC designation conversion with bidirectional timing and round-trip verification
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
	input    string
	got      string
	expected string
	phase    string
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: test_roundtrip <csv_file>")
		os.Exit(1)
	}

	csvFile := os.Args[1]

	// First pass: Load all test data into memory
	file, err := os.Open(csvFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Cannot open file: %s\n", csvFile)
		os.Exit(1)
	}

	var unpackedList []string
	var packedList []string

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		// Skip header
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
		unpackedList = append(unpackedList, parts[0])
		packedList = append(packedList, parts[1])
	}
	file.Close()

	totalTests := len(unpackedList)
	fmt.Printf("Loaded %d test cases\n\n", totalTests)

	var errors []testError
	const maxErrors = 20

	// ========== Phase 1: Pack (unpacked → packed) ==========
	fmt.Println("=== Phase 1: Pack (unpacked → packed) ===")
	var packPassed, packFailed int
	startPack := time.Now()

	for i := 0; i < totalTests; i++ {
		unpacked := unpackedList[i]
		expectedPacked := packedList[i]

		gotPacked, err := mpc.ConvertSimple(unpacked)
		if err != nil {
			packFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{unpacked, fmt.Sprintf("ERROR: %v", err), expectedPacked, "pack"})
			}
		} else if gotPacked != expectedPacked {
			packFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{unpacked, gotPacked, expectedPacked, "pack"})
			}
		} else {
			packPassed++
		}
	}

	packElapsed := time.Since(startPack)
	packRate := float64(totalTests) / packElapsed.Seconds()
	fmt.Printf("Passed: %d\n", packPassed)
	fmt.Printf("Failed: %d\n", packFailed)
	fmt.Printf("Time:   %dms (%.1f entries/sec)\n\n", packElapsed.Milliseconds(), packRate)

	// ========== Phase 2: Unpack (packed → unpacked) ==========
	fmt.Println("=== Phase 2: Unpack (packed → unpacked) ===")
	var unpackPassed, unpackFailed int
	startUnpack := time.Now()

	for i := 0; i < totalTests; i++ {
		packed := packedList[i]
		expectedUnpacked := unpackedList[i]

		gotUnpacked, err := mpc.ConvertSimple(packed)
		if err != nil {
			unpackFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{packed, fmt.Sprintf("ERROR: %v", err), expectedUnpacked, "unpack"})
			}
		} else if gotUnpacked != expectedUnpacked {
			unpackFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{packed, gotUnpacked, expectedUnpacked, "unpack"})
			}
		} else {
			unpackPassed++
		}
	}

	unpackElapsed := time.Since(startUnpack)
	unpackRate := float64(totalTests) / unpackElapsed.Seconds()
	fmt.Printf("Passed: %d\n", unpackPassed)
	fmt.Printf("Failed: %d\n", unpackFailed)
	fmt.Printf("Time:   %dms (%.1f entries/sec)\n\n", unpackElapsed.Milliseconds(), unpackRate)

	// ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
	fmt.Println("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===")
	var rtUnpackedPassed, rtUnpackedFailed int
	startRTUnpacked := time.Now()

	for i := 0; i < totalTests; i++ {
		original := unpackedList[i]

		// Pack it
		packed, err := mpc.ConvertSimple(original)
		if err != nil {
			rtUnpackedFailed++
			continue
		}

		// Unpack it back
		unpacked, err := mpc.ConvertSimple(packed)
		if err != nil {
			rtUnpackedFailed++
			continue
		}

		// Verify
		if unpacked != original {
			rtUnpackedFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{original, fmt.Sprintf("%s → %s", packed, unpacked), original, "rt-unp"})
			}
		} else {
			rtUnpackedPassed++
		}
	}

	rtUnpackedElapsed := time.Since(startRTUnpacked)
	rtUnpackedRate := float64(totalTests) / rtUnpackedElapsed.Seconds()
	fmt.Printf("Passed: %d\n", rtUnpackedPassed)
	fmt.Printf("Failed: %d\n", rtUnpackedFailed)
	fmt.Printf("Time:   %dms (%.1f entries/sec)\n\n", rtUnpackedElapsed.Milliseconds(), rtUnpackedRate)

	// ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
	fmt.Println("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===")
	errors = nil // Reset to show phase 4 errors
	var rtPackedPassed, rtPackedFailed int
	startRTPacked := time.Now()

	for i := 0; i < totalTests; i++ {
		original := packedList[i]

		// Unpack it
		unpacked, err := mpc.ConvertSimple(original)
		if err != nil {
			rtPackedFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{original, fmt.Sprintf("ERROR: %v", err), original, "rt-pak"})
			}
			continue
		}

		// Pack it back
		repacked, err := mpc.ConvertSimple(unpacked)
		if err != nil {
			rtPackedFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{original, fmt.Sprintf("ERROR: %v", err), original, "rt-pak"})
			}
			continue
		}

		// Verify
		if repacked != original {
			rtPackedFailed++
			if len(errors) < maxErrors {
				errors = append(errors, testError{original, fmt.Sprintf("%s → %s", unpacked, repacked), original, "rt-pak"})
			}
		} else {
			rtPackedPassed++
		}
	}

	rtPackedElapsed := time.Since(startRTPacked)
	rtPackedRate := float64(totalTests) / rtPackedElapsed.Seconds()
	fmt.Printf("Passed: %d\n", rtPackedPassed)
	fmt.Printf("Failed: %d\n", rtPackedFailed)
	fmt.Printf("Time:   %dms (%.1f entries/sec)\n\n", rtPackedElapsed.Milliseconds(), rtPackedRate)

	// ========== Summary ==========
	fmt.Println("=== Summary ===")
	fmt.Printf("%-30s %10s %18s %12s\n", "Phase", "Time (ms)", "Rate (entries/sec)", "Status")
	fmt.Printf("%-30s %10s %18s %12s\n", "------------------------------", "----------", "------------------", "------------")
	fmt.Printf("%-30s %10d %18.1f %12s\n", "Pack", packElapsed.Milliseconds(), packRate, statusStr(packFailed))
	fmt.Printf("%-30s %10d %18.1f %12s\n", "Unpack", unpackElapsed.Milliseconds(), unpackRate, statusStr(unpackFailed))
	fmt.Printf("%-30s %10d %18.1f %12s\n", "Unpacked RT: unpack(pack(x))=x", rtUnpackedElapsed.Milliseconds(), rtUnpackedRate, statusStr(rtUnpackedFailed))
	fmt.Printf("%-30s %10d %18.1f %12s\n", "Packed RT: pack(unpack(y))=y", rtPackedElapsed.Milliseconds(), rtPackedRate, statusStr(rtPackedFailed))
	fmt.Println()

	// Show errors if any
	if len(errors) > 0 {
		fmt.Printf("=== First %d errors ===\n", len(errors))
		fmt.Printf("%-8s %-25s %-20s %-20s\n", "Phase", "Input", "Got", "Expected")
		fmt.Printf("%-8s %-25s %-20s %-20s\n", "--------", "-------------------------", "--------------------", "--------------------")
		for _, e := range errors {
			fmt.Printf("%-8s %-25s %-20s %-20s\n", e.phase, e.input, e.got, e.expected)
		}
	}

	// Exit with error only if pack or packed RT failed
	totalFailed := packFailed + rtPackedFailed
	if totalFailed > 0 {
		os.Exit(1)
	}
}

func statusStr(failed int) string {
	if failed == 0 {
		return "PASS"
	}
	return fmt.Sprintf("FAIL (%d)", failed)
}
