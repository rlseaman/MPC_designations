#!/usr/bin/env php
<?php
/**
 * Test mpc_designation.php roundtrip conversion.
 *
 * Tests that pack(unpack(x)) == x for packed designations,
 * and unpack(pack(x)) == x for unpacked designations.
 *
 * Usage: php test_roundtrip.php <csv_file> [max_errors]
 */

declare(strict_types=1);

require_once __DIR__ . '/../src/MPCDesignation.php';

use MPC\MPCDesignation;
use MPC\MPCDesignationException;

function runRoundtripTests(string $csvFile, int $maxErrors = 100): bool {
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
        $packed = $parts[1];

        // Test packed -> unpacked -> packed roundtrip
        try {
            $toUnpacked = MPCDesignation::unpack($packed);
            $backToPacked = MPCDesignation::pack($toUnpacked);

            if ($backToPacked !== $packed) {
                $failed++;
                if (count($errors) < $maxErrors) {
                    $errors[] = ["packed roundtrip", $packed, $toUnpacked, $backToPacked, $packed];
                }
                continue;
            }
        } catch (MPCDesignationException $e) {
            $failed++;
            if (count($errors) < $maxErrors) {
                $errors[] = ["packed roundtrip", $packed, "ERROR", $e->getMessage(), $packed];
            }
            continue;
        }

        // Test unpacked -> packed -> unpacked roundtrip
        // Note: old-style designations like "A908 CJ" convert to modern "1908 CJ"
        try {
            $toPacked = MPCDesignation::pack($unpacked);
            $backToUnpacked = MPCDesignation::unpack($toPacked);

            // For old-style designations, the unpacked form changes
            // Check if it's an old-style designation
            $isOldStyle = preg_match('/^[AB]\d{3} [A-Z][A-Z]$/', $unpacked);

            if (!$isOldStyle && $backToUnpacked !== $unpacked) {
                $failed++;
                if (count($errors) < $maxErrors) {
                    $errors[] = ["unpacked roundtrip", $unpacked, $toPacked, $backToUnpacked, $unpacked];
                }
                continue;
            }

            // For old-style, verify the modern form roundtrips
            if ($isOldStyle) {
                $repackedModern = MPCDesignation::pack($backToUnpacked);
                if ($repackedModern !== $toPacked) {
                    $failed++;
                    if (count($errors) < $maxErrors) {
                        $errors[] = ["old-style modern roundtrip", $unpacked, $backToUnpacked, $repackedModern, $toPacked];
                    }
                    continue;
                }
            }
        } catch (MPCDesignationException $e) {
            $failed++;
            if (count($errors) < $maxErrors) {
                $errors[] = ["unpacked roundtrip", $unpacked, "ERROR", $e->getMessage(), $unpacked];
            }
            continue;
        }

        $passed++;

        // Progress indicator every 100,000 entries
        if ($total % 100000 === 0) {
            echo "Processed $total entries...\n";
        }
    }

    fclose($fp);

    $elapsed = microtime(true) - $startTime;

    echo "\n";
    echo "=== Roundtrip Test Results ===\n";
    echo "Total:  $total\n";
    echo "Passed: $passed\n";
    echo "Failed: $failed\n";
    $elapsedMs = $elapsed * 1000;
    $rate = $total / $elapsed;
    echo sprintf("Time:   %.0fms (%.1f entries/sec)\n", $elapsedMs, $rate);
    echo "\n";

    if ($failed > 0) {
        echo sprintf("=== First %d failures ===\n", count($errors));
        echo sprintf("%-20s %-20s %-15s %-20s %-15s\n", 'Test', 'Input', 'Step1', 'Step2', 'Expected');
        echo str_repeat('-', 90) . "\n";
        foreach ($errors as [$test, $input, $step1, $step2, $expected]) {
            echo sprintf("%-20s %-20s %-15s %-20s %-15s\n", $test, $input, $step1, $step2, $expected);
        }
    }

    return $failed === 0;
}

function main(array $argv): int {
    if (count($argv) < 2) {
        fwrite(STDERR, "Usage: php test_roundtrip.php <csv_file> [max_errors]\n");
        return 1;
    }

    $csvFile = $argv[1];
    $maxErrors = isset($argv[2]) ? (int)$argv[2] : 100;

    $success = runRoundtripTests($csvFile, $maxErrors);
    return $success ? 0 : 1;
}

exit(main($argv));
