#!/usr/bin/env php
<?php
/**
 * Test MPC designation with bidirectional timing and round-trip verification.
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
 *
 * Usage: php test_roundtrip.php <csv_file>
 */

declare(strict_types=1);

require_once __DIR__ . '/../src/MPCDesignation.php';

use MPC\MPCDesignation;
use MPC\MPCDesignationException;

function countLines(string $csvFile): int {
    $count = 0;
    $fp = fopen($csvFile, 'r');
    fgets($fp); // Skip header
    while (fgets($fp) !== false) {
        $count++;
    }
    fclose($fp);
    return $count;
}

function main(array $argv): int {
    if (count($argv) < 2) {
        fwrite(STDERR, "Usage: php test_roundtrip.php <csv_file>\n");
        return 1;
    }

    $csvFile = $argv[1];
    $total = countLines($csvFile);
    echo "Loaded $total test cases\n\n";

    // Phase 1: Pack (unpacked -> packed)
    echo "=== Phase 1: Pack (unpacked -> packed) ===\n";
    $packPassed = 0;
    $packFailed = 0;
    $startTime = microtime(true);

    $fp = fopen($csvFile, 'r');
    fgets($fp); // Skip header
    while (($line = fgets($fp)) !== false) {
        $line = trim($line);
        if ($line === '') continue;
        $parts = explode(',', $line, 2);
        if (count($parts) !== 2) continue;

        try {
            $result = MPCDesignation::pack($parts[0]);
            if ($result === $parts[1]) {
                $packPassed++;
            } else {
                $packFailed++;
            }
        } catch (MPCDesignationException $e) {
            $packFailed++;
        }
    }
    fclose($fp);

    $elapsed = (microtime(true) - $startTime) * 1000;
    $rate = $total / ($elapsed / 1000);
    echo "Passed: $packPassed\n";
    echo "Failed: $packFailed\n";
    echo sprintf("Time:   %.0fms (%.1f entries/sec)\n\n", $elapsed, $rate);

    // Phase 2: Unpack (packed -> unpacked)
    echo "=== Phase 2: Unpack (packed -> unpacked) ===\n";
    $unpackPassed = 0;
    $unpackFailed = 0;
    $startTime = microtime(true);

    $fp = fopen($csvFile, 'r');
    fgets($fp);
    while (($line = fgets($fp)) !== false) {
        $line = trim($line);
        if ($line === '') continue;
        $parts = explode(',', $line, 2);
        if (count($parts) !== 2) continue;

        try {
            $result = MPCDesignation::unpack($parts[1]);
            if ($result === $parts[0]) {
                $unpackPassed++;
            } else {
                $unpackFailed++;
            }
        } catch (MPCDesignationException $e) {
            $unpackFailed++;
        }
    }
    fclose($fp);

    $elapsed = (microtime(true) - $startTime) * 1000;
    $rate = $total / ($elapsed / 1000);
    echo "Passed: $unpackPassed\n";
    echo "Failed: $unpackFailed\n";
    echo sprintf("Time:   %.0fms (%.1f entries/sec)\n\n", $elapsed, $rate);

    // Phase 3: Unpacked round-trip: unpack(pack(x)) = x
    echo "=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===\n";
    $rtUnpackedPassed = 0;
    $rtUnpackedFailed = 0;
    $startTime = microtime(true);

    $fp = fopen($csvFile, 'r');
    fgets($fp);
    while (($line = fgets($fp)) !== false) {
        $line = trim($line);
        if ($line === '') continue;
        $parts = explode(',', $line, 2);
        if (count($parts) !== 2) continue;

        try {
            $packed = MPCDesignation::pack($parts[0]);
            $back = MPCDesignation::unpack($packed);
            if ($back === $parts[0]) {
                $rtUnpackedPassed++;
            } else {
                $rtUnpackedFailed++;
            }
        } catch (MPCDesignationException $e) {
            $rtUnpackedFailed++;
        }
    }
    fclose($fp);

    $elapsed = (microtime(true) - $startTime) * 1000;
    $rate = $total / ($elapsed / 1000);
    echo "Passed: $rtUnpackedPassed\n";
    echo "Failed: $rtUnpackedFailed\n";
    echo sprintf("Time:   %.0fms (%.1f entries/sec)\n\n", $elapsed, $rate);

    // Phase 4: Packed round-trip: pack(unpack(y)) = y
    echo "=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===\n";
    $rtPackedPassed = 0;
    $rtPackedFailed = 0;
    $startTime = microtime(true);

    $fp = fopen($csvFile, 'r');
    fgets($fp);
    while (($line = fgets($fp)) !== false) {
        $line = trim($line);
        if ($line === '') continue;
        $parts = explode(',', $line, 2);
        if (count($parts) !== 2) continue;

        try {
            $unpacked = MPCDesignation::unpack($parts[1]);
            $back = MPCDesignation::pack($unpacked);
            if ($back === $parts[1]) {
                $rtPackedPassed++;
            } else {
                $rtPackedFailed++;
            }
        } catch (MPCDesignationException $e) {
            $rtPackedFailed++;
        }
    }
    fclose($fp);

    $elapsed = (microtime(true) - $startTime) * 1000;
    $rate = $total / ($elapsed / 1000);
    echo "Passed: $rtPackedPassed\n";
    echo "Failed: $rtPackedFailed\n";
    echo sprintf("Time:   %.0fms (%.1f entries/sec)\n\n", $elapsed, $rate);

    // Summary
    echo "=== Summary ===\n";
    echo "Pack:       " . ($packFailed === 0 ? "PASS" : "FAIL ($packFailed)") . "\n";
    echo "Unpack:     " . ($unpackFailed === 0 ? "PASS" : "FAIL ($unpackFailed)") . "\n";
    echo "Unpacked RT: " . ($rtUnpackedFailed === 0 ? "PASS" : "FAIL ($rtUnpackedFailed)") . "\n";
    echo "Packed RT:   " . ($rtPackedFailed === 0 ? "PASS" : "FAIL ($rtPackedFailed)") . "\n";

    if ($packFailed > 0 || $rtPackedFailed > 0) {
        return 1;
    }
    return 0;
}

exit(main($argv));
