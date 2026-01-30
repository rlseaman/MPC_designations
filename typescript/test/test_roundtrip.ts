#!/usr/bin/env node
/**
 * Test MPC designation round-trip conversions (pack -> unpack -> pack).
 */

import * as fs from 'fs';
import * as readline from 'readline';
import { convertSimple } from '../src/mpc_designation';

interface TestError {
    input: string;
    message: string;
}

async function runTests(csvFile: string, maxErrors: number = 100): Promise<boolean> {
    let total = 0;
    let passed = 0;
    let failed = 0;
    const errors: TestError[] = [];

    const startTime = Date.now();

    console.log('=== MPC Designation Round-Trip Tests (TypeScript) ===');
    console.log();

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

        total++;
        const parts = trimmedLine.split(',');
        const unpacked = parts[0];
        const packed = parts[1];

        try {
            // Test round-trip: unpacked -> packed -> unpacked -> packed
            const packed1 = convertSimple(unpacked);
            const unpacked1 = convertSimple(packed1);
            const packed2 = convertSimple(unpacked1);

            if (packed1 !== packed2) {
                failed++;
                if (errors.length < maxErrors) {
                    errors.push({
                        input: unpacked,
                        message: `unpacked -> '${packed1}' -> '${unpacked1}' -> '${packed2}' (packed1 != packed2)`
                    });
                }
            } else if (packed1 !== packed) {
                failed++;
                if (errors.length < maxErrors) {
                    errors.push({
                        input: unpacked,
                        message: `Expected packed: '${packed}', Got: '${packed1}'`
                    });
                }
            } else {
                passed++;
            }
        } catch (e) {
            failed++;
            if (errors.length < maxErrors) {
                const message = e instanceof Error ? e.message : String(e);
                errors.push({ input: unpacked, message: `Error: ${message}` });
            }
        }

        // Progress indicator every 100,000 entries
        if (total % 100000 === 0) {
            process.stdout.write(`Processed ${total} entries...\n`);
        }
    }

    const elapsed = Date.now() - startTime;

    console.log();
    console.log('=== Round-Trip Test Results ===');
    console.log(`Total:  ${total}`);
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);
    console.log(`Time:   ${elapsed}ms (${(total / (elapsed / 1000)).toFixed(1)} entries/sec)`);
    console.log();

    if (failed > 0) {
        console.log(`=== First ${errors.length} failures ===`);
        for (const { input, message } of errors) {
            console.log(`FAIL: roundtrip('${input}')`);
            console.log(`      ${message}`);
        }
    }

    return failed === 0;
}

async function main(): Promise<void> {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.error('Usage: test_roundtrip <csv_file> [max_errors]');
        process.exit(1);
    }

    const csvFile = args[0];
    const maxErrors = args.length > 1 ? parseInt(args[1], 10) : 100;

    const success = await runTests(csvFile, maxErrors);
    process.exit(success ? 0 : 1);
}

main().catch(err => {
    console.error(err);
    process.exit(1);
});
