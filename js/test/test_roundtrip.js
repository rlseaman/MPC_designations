#!/usr/bin/env node
/**
 * test_roundtrip.js - Bidirectional testing with round-trip verification
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
 *
 * Usage: node test_roundtrip.js <csv_file>
 */

'use strict';

const fs = require('fs');
const readline = require('readline');
const path = require('path');

const { convertSimple, MPCDesignationError } = require('../src/mpc_designation');

const MAX_ERRORS = 20;

async function loadTestData(csvFile) {
    const unpackedList = [];
    const packedList = [];

    const fileStream = fs.createReadStream(csvFile);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let isFirstLine = true;

    for await (const line of rl) {
        if (isFirstLine) {
            isFirstLine = false;
            continue; // Skip header
        }

        const trimmedLine = line.trim();
        if (!trimmedLine) {
            continue;
        }

        const parts = trimmedLine.split(',');
        if (parts.length >= 2) {
            unpackedList.push(parts[0]);
            packedList.push(parts[1]);
        }
    }

    return { unpackedList, packedList };
}

function statusStr(failed) {
    return failed === 0 ? 'PASS' : `FAIL (${failed})`;
}

async function runRoundtripTests(csvFile) {
    const { unpackedList, packedList } = await loadTestData(csvFile);
    const total = unpackedList.length;
    console.log(`Loaded ${total} test cases\n`);

    let errors = [];

    // ========== Phase 1: Pack (unpacked -> packed) ==========
    console.log('=== Phase 1: Pack (unpacked -> packed) ===');
    let packPassed = 0;
    let packFailed = 0;
    let startTime = Date.now();

    for (let i = 0; i < total; i++) {
        const unpacked = unpackedList[i];
        const expected = packedList[i];

        let got;
        try {
            got = convertSimple(unpacked);
        } catch (e) {
            packFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'pack', input: unpacked, got: `ERROR: ${e.message}`, expected });
            }
            continue;
        }

        if (got === expected) {
            packPassed++;
        } else {
            packFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'pack', input: unpacked, got, expected });
            }
        }
    }

    const packTime = Date.now() - startTime;
    const packRate = total / (packTime / 1000);
    console.log(`Passed: ${packPassed}`);
    console.log(`Failed: ${packFailed}`);
    console.log(`Time:   ${packTime}ms (${packRate.toFixed(1)} entries/sec)\n`);

    // ========== Phase 2: Unpack (packed -> unpacked) ==========
    console.log('=== Phase 2: Unpack (packed -> unpacked) ===');
    let unpackPassed = 0;
    let unpackFailed = 0;
    startTime = Date.now();

    for (let i = 0; i < total; i++) {
        const packed = packedList[i];
        const expected = unpackedList[i];

        let got;
        try {
            got = convertSimple(packed);
        } catch (e) {
            unpackFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'unpack', input: packed, got: `ERROR: ${e.message}`, expected });
            }
            continue;
        }

        if (got === expected) {
            unpackPassed++;
        } else {
            unpackFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'unpack', input: packed, got, expected });
            }
        }
    }

    const unpackTime = Date.now() - startTime;
    const unpackRate = total / (unpackTime / 1000);
    console.log(`Passed: ${unpackPassed}`);
    console.log(`Failed: ${unpackFailed}`);
    console.log(`Time:   ${unpackTime}ms (${unpackRate.toFixed(1)} entries/sec)\n`);

    // ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    console.log('=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===');
    let rtUnpackedPassed = 0;
    let rtUnpackedFailed = 0;
    startTime = Date.now();

    for (let i = 0; i < total; i++) {
        const original = unpackedList[i];

        let packed, unpacked;
        try {
            packed = convertSimple(original);
            unpacked = convertSimple(packed);
        } catch (e) {
            rtUnpackedFailed++;
            continue;
        }

        if (unpacked === original) {
            rtUnpackedPassed++;
        } else {
            rtUnpackedFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'rt-unp', input: original, got: `${packed} -> ${unpacked}`, expected: original });
            }
        }
    }

    const rtUnpackedTime = Date.now() - startTime;
    const rtUnpackedRate = total / (rtUnpackedTime / 1000);
    console.log(`Passed: ${rtUnpackedPassed}`);
    console.log(`Failed: ${rtUnpackedFailed}`);
    console.log(`Time:   ${rtUnpackedTime}ms (${rtUnpackedRate.toFixed(1)} entries/sec)\n`);

    // ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    console.log('=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===');
    errors = []; // Reset to show phase 4 errors
    let rtPackedPassed = 0;
    let rtPackedFailed = 0;
    startTime = Date.now();

    for (let i = 0; i < total; i++) {
        const original = packedList[i];

        let unpacked, repacked;
        try {
            unpacked = convertSimple(original);
            repacked = convertSimple(unpacked);
        } catch (e) {
            rtPackedFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'rt-pak', input: original, got: `ERROR: ${e.message}`, expected: original });
            }
            continue;
        }

        if (repacked === original) {
            rtPackedPassed++;
        } else {
            rtPackedFailed++;
            if (errors.length < MAX_ERRORS) {
                errors.push({ phase: 'rt-pak', input: original, got: `${unpacked} -> ${repacked}`, expected: original });
            }
        }
    }

    const rtPackedTime = Date.now() - startTime;
    const rtPackedRate = total / (rtPackedTime / 1000);
    console.log(`Passed: ${rtPackedPassed}`);
    console.log(`Failed: ${rtPackedFailed}`);
    console.log(`Time:   ${rtPackedTime}ms (${rtPackedRate.toFixed(1)} entries/sec)\n`);

    // ========== Summary ==========
    console.log('=== Summary ===');
    console.log(`${'Phase'.padEnd(30)} ${'Time (ms)'.padStart(10)} ${'Rate (entries/sec)'.padStart(18)} ${'Status'.padStart(12)}`);
    console.log(`${'-'.repeat(30)} ${'-'.repeat(10)} ${'-'.repeat(18)} ${'-'.repeat(12)}`);
    console.log(`${'Pack'.padEnd(30)} ${packTime.toString().padStart(10)} ${packRate.toFixed(1).padStart(18)} ${statusStr(packFailed).padStart(12)}`);
    console.log(`${'Unpack'.padEnd(30)} ${unpackTime.toString().padStart(10)} ${unpackRate.toFixed(1).padStart(18)} ${statusStr(unpackFailed).padStart(12)}`);
    console.log(`${'Unpacked RT: unpack(pack(x))=x'.padEnd(30)} ${rtUnpackedTime.toString().padStart(10)} ${rtUnpackedRate.toFixed(1).padStart(18)} ${statusStr(rtUnpackedFailed).padStart(12)}`);
    console.log(`${'Packed RT: pack(unpack(y))=y'.padEnd(30)} ${rtPackedTime.toString().padStart(10)} ${rtPackedRate.toFixed(1).padStart(18)} ${statusStr(rtPackedFailed).padStart(12)}`);
    console.log();

    // Show errors
    if (errors.length > 0) {
        console.log(`=== First ${errors.length} errors ===`);
        console.log(`${'Phase'.padEnd(8)} ${'Input'.padEnd(25)} ${'Got'.padEnd(20)} ${'Expected'.padEnd(20)}`);
        console.log(`${'-'.repeat(8)} ${'-'.repeat(25)} ${'-'.repeat(20)} ${'-'.repeat(20)}`);
        for (const e of errors) {
            console.log(`${e.phase.padEnd(8)} ${e.input.padEnd(25)} ${e.got.padEnd(20)} ${e.expected.padEnd(20)}`);
        }
    }

    // Exit with error only if pack or packed RT failed
    const totalFailed = packFailed + rtPackedFailed;
    return totalFailed === 0;
}

async function main() {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.error('Usage: node test_roundtrip.js <csv_file>');
        process.exit(1);
    }

    const csvFile = args[0];
    const success = await runRoundtripTests(csvFile);
    process.exit(success ? 0 : 1);
}

main().catch(err => {
    console.error(err);
    process.exit(1);
});
