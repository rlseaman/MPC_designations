#!/usr/bin/env php
<?php
/**
 * Test mpc_designation.php against CSV file of known conversions.
 *
 * Usage: php test_csv.php <csv_file> [max_errors]
 */

declare(strict_types=1);

require_once __DIR__ . '/../src/MPCDesignation.php';

use MPC\MPCDesignation;
use MPC\MPCDesignationException;

function runTests(string $csvFile, int $maxErrors = 100): bool {
    $total = 0;
    $passed = 0;
    $failed = 0;
    $errors = [];

    $startTime = microtime(true);

    $fp = fopen($csvFile, 'r');
    if ($fp === false) {
        fwrite(STDERR, "Error: Cannot open file: $csvFile\n");
        return false;
    }

    // Skip header line
    fgets($fp);

    while (($line = fgets($fp)) !== false) {
        $line = trim($line);
        if ($line === '') {
            continue;
        }

        // Skip header if encountered
        if (str_starts_with($line, 'unpacked')) {
            continue;
        }

        $total++;
        $parts = explode(',', $line);
        $unpacked = $parts[0];
        $expectedPacked = $parts[1];

        try {
            $gotPacked = MPCDesignation::convertSimple($unpacked);
        } catch (MPCDesignationException $e) {
            $failed++;
            if (count($errors) < $maxErrors) {
                $errors[] = [$unpacked, "ERROR: " . $e->getMessage(), $expectedPacked];
            }
            continue;
        }

        if ($gotPacked !== $expectedPacked) {
            $failed++;
            if (count($errors) < $maxErrors) {
                $errors[] = [$unpacked, $gotPacked, $expectedPacked];
            }
        } else {
            $passed++;
        }

        // Progress indicator every 100,000 entries
        if ($total % 100000 === 0) {
            echo "Processed $total entries...\n";
        }
    }

    fclose($fp);

    $elapsed = microtime(true) - $startTime;

    echo "\n";
    echo "=== Test Results ===\n";
    echo "Total:  $total\n";
    echo "Passed: $passed\n";
    echo "Failed: $failed\n";
    $elapsedMs = $elapsed * 1000;
    $rate = $total / $elapsed;
    echo sprintf("Time:   %.0fms (%.1f entries/sec)\n", $elapsedMs, $rate);
    echo "\n";

    if ($failed > 0) {
        echo sprintf("=== First %d failures ===\n", count($errors));
        echo sprintf("%-25s %-15s %-15s\n", 'Input', 'Got', 'Expected');
        echo str_repeat('-', 60) . "\n";
        foreach ($errors as [$inp, $got, $expected]) {
            echo sprintf("%-25s %-15s %-15s\n", $inp, $got, $expected);
        }
    }

    return $failed === 0;
}

function main(array $argv): int {
    if (count($argv) < 2) {
        fwrite(STDERR, "Usage: php test_csv.php <csv_file> [max_errors]\n");
        return 1;
    }

    $csvFile = $argv[1];
    $maxErrors = isset($argv[2]) ? (int)$argv[2] : 100;

    $success = runTests($csvFile, $maxErrors);
    return $success ? 0 : 1;
}

exit(main($argv));
