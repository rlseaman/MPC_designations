#!/usr/bin/env php
<?php
/**
 * Test mpc_designation.php error handling.
 *
 * Tests various classes of invalid input to ensure proper error detection.
 * Reads test cases from error_test_cases.csv.
 *
 * Usage: php test_errors.php [error_test_cases.csv]
 */

declare(strict_types=1);

require_once __DIR__ . '/../src/MPCDesignation.php';

use MPC\MPCDesignation;
use MPC\MPCDesignationException;

function unescapeString(string $s): string {
    // Handle \xNN hex escapes first
    $s = preg_replace_callback('/\\\\x([0-9a-fA-F]{2})/', function($matches) {
        return chr((int)hexdec($matches[1]));
    }, $s);

    // Handle standard escapes
    $s = str_replace('\\n', "\n", $s);
    $s = str_replace('\\r', "\r", $s);
    $s = str_replace('\\t', "\t", $s);
    $s = str_replace('\\f', "\f", $s);
    $s = str_replace('\\v', "\v", $s);
    $s = str_replace('\\0', "\0", $s);
    $s = str_replace('\\\\', '\\', $s);

    return $s;
}

function runErrorTests(string $csvFile): bool {
    $total = 0;
    $passed = 0;
    $failed = 0;

    echo "=== MPC Designation Error Tests ===\n\n";

    $fp = fopen($csvFile, 'r');
    if ($fp === false) {
        fwrite(STDERR, "Error: Cannot open file: $csvFile\n");
        return false;
    }

    while (($line = fgets($fp)) !== false) {
        $line = trim($line);

        // Skip empty lines and comments
        if ($line === '' || str_starts_with($line, '#')) {
            continue;
        }

        // Skip header
        if (str_starts_with($line, 'category,')) {
            continue;
        }

        // Parse CSV (simple split, no quotes handling needed for this data)
        $parts = explode(',', $line, 5);
        if (count($parts) < 5) {
            continue;
        }

        $category = $parts[0];
        $subcategory = $parts[1];
        $inputStr = $parts[2];
        $expectedError = $parts[3];
        $description = $parts[4];

        // Unescape the input string
        $inputStr = unescapeString($inputStr);

        $total++;

        // Run the test
        $gotError = false;
        $errorMsg = '';
        $output = '';

        try {
            $output = MPCDesignation::convertSimple($inputStr);
        } catch (MPCDesignationException $e) {
            $gotError = true;
            $errorMsg = $e->getMessage();
        } catch (\Exception $e) {
            $gotError = true;
            $errorMsg = $e->getMessage();
        }

        $testPassed = false;

        if ($expectedError === 'valid') {
            // Expect success
            if (!$gotError) {
                $testPassed = true;
            } else {
                echo "FAIL [$category/$subcategory]: '$description'\n";
                echo "      Expected: valid conversion\n";
                echo "      Got:      $errorMsg\n";
                $failed++;
            }
        } else {
            // Expect error
            if ($gotError) {
                $testPassed = true;
            } else {
                echo "FAIL [$category/$subcategory]: '$description'\n";
                echo "      Expected: error ($expectedError)\n";
                echo "      Got:      '$output' (success)\n";
                $failed++;
            }
        }

        if ($testPassed) {
            $passed++;
        }
    }

    fclose($fp);

    echo "\n=== Error Test Results ===\n";
    echo "Total:  $total\n";
    echo "Passed: $passed\n";
    echo "Failed: $failed\n";

    return $failed === 0;
}

function main(array $argv): int {
    $csvFile = $argv[1] ?? 'error_test_cases.csv';

    $success = runErrorTests($csvFile);
    return $success ? 0 : 1;
}

exit(main($argv));
