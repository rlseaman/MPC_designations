#!/usr/bin/env node
/**
 * Test MPC designation with bidirectional timing and round-trip verification.
 */

import * as fs from 'fs';
import * as readline from 'readline';
import { pack, unpack } from '../src/mpc_designation';

interface TestCase {
    unpacked: string;
    packed: string;
}

async function loadTestCases(csvFile: string): Promise<TestCase[]> {
    const testCases: TestCase[] = [];
    const fileStream = fs.createReadStream(csvFile);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let isFirstLine = true;

    for await (const line of rl) {
        if (isFirstLine) {
            isFirstLine = false;
            continue;
        }

        const trimmedLine = line.trim();
        if (!trimmedLine) continue;

        const comma = trimmedLine.indexOf(',');
        if (comma < 0) continue;

        testCases.push({
            unpacked: trimmedLine.substring(0, comma),
            packed: trimmedLine.substring(comma + 1)
        });
    }

    return testCases;
}

async function main(): Promise<void> {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.error('Usage: test_roundtrip <csv_file>');
        process.exit(1);
    }

    const csvFile = args[0];
    const testCases = await loadTestCases(csvFile);
    console.log(`Loaded ${testCases.length} test cases`);
    console.log();

    // Phase 1: Pack (unpacked -> packed)
    console.log('=== Phase 1: Pack (unpacked -> packed) ===');
    let packPassed = 0;
    let packFailed = 0;
    let startTime = Date.now();

    for (const tc of testCases) {
        try {
            const result = pack(tc.unpacked);
            if (result === tc.packed) {
                packPassed++;
            } else {
                packFailed++;
            }
        } catch {
            packFailed++;
        }
    }

    let elapsed = Date.now() - startTime;
    let rate = testCases.length / (elapsed / 1000);
    console.log(`Passed: ${packPassed}`);
    console.log(`Failed: ${packFailed}`);
    console.log(`Time:   ${elapsed}ms (${rate.toFixed(1)} entries/sec)`);
    console.log();

    // Phase 2: Unpack (packed -> unpacked)
    console.log('=== Phase 2: Unpack (packed -> unpacked) ===');
    let unpackPassed = 0;
    let unpackFailed = 0;
    startTime = Date.now();

    for (const tc of testCases) {
        try {
            const result = unpack(tc.packed);
            if (result === tc.unpacked) {
                unpackPassed++;
            } else {
                unpackFailed++;
            }
        } catch {
            unpackFailed++;
        }
    }

    elapsed = Date.now() - startTime;
    rate = testCases.length / (elapsed / 1000);
    console.log(`Passed: ${unpackPassed}`);
    console.log(`Failed: ${unpackFailed}`);
    console.log(`Time:   ${elapsed}ms (${rate.toFixed(1)} entries/sec)`);
    console.log();

    // Phase 3: Unpacked round-trip: unpack(pack(x)) = x
    console.log('=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===');
    let rtUnpackedPassed = 0;
    let rtUnpackedFailed = 0;
    startTime = Date.now();

    for (const tc of testCases) {
        try {
            const packed = pack(tc.unpacked);
            const back = unpack(packed);
            if (back === tc.unpacked) {
                rtUnpackedPassed++;
            } else {
                rtUnpackedFailed++;
            }
        } catch {
            rtUnpackedFailed++;
        }
    }

    elapsed = Date.now() - startTime;
    rate = testCases.length / (elapsed / 1000);
    console.log(`Passed: ${rtUnpackedPassed}`);
    console.log(`Failed: ${rtUnpackedFailed}`);
    console.log(`Time:   ${elapsed}ms (${rate.toFixed(1)} entries/sec)`);
    console.log();

    // Phase 4: Packed round-trip: pack(unpack(y)) = y
    console.log('=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===');
    let rtPackedPassed = 0;
    let rtPackedFailed = 0;
    startTime = Date.now();

    for (const tc of testCases) {
        try {
            const unpacked = unpack(tc.packed);
            const back = pack(unpacked);
            if (back === tc.packed) {
                rtPackedPassed++;
            } else {
                rtPackedFailed++;
            }
        } catch {
            rtPackedFailed++;
        }
    }

    elapsed = Date.now() - startTime;
    rate = testCases.length / (elapsed / 1000);
    console.log(`Passed: ${rtPackedPassed}`);
    console.log(`Failed: ${rtPackedFailed}`);
    console.log(`Time:   ${elapsed}ms (${rate.toFixed(1)} entries/sec)`);
    console.log();

    // Summary
    console.log('=== Summary ===');
    console.log(`Pack:       ${packFailed === 0 ? 'PASS' : `FAIL (${packFailed})`}`);
    console.log(`Unpack:     ${unpackFailed === 0 ? 'PASS' : `FAIL (${unpackFailed})`}`);
    console.log(`Unpacked RT: ${rtUnpackedFailed === 0 ? 'PASS' : `FAIL (${rtUnpackedFailed})`}`);
    console.log(`Packed RT:   ${rtPackedFailed === 0 ? 'PASS' : `FAIL (${rtPackedFailed})`}`);

    if (packFailed > 0 || rtPackedFailed > 0) {
        process.exit(1);
    }
}

main().catch(err => {
    console.error(err);
    process.exit(1);
});
