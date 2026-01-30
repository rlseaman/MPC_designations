#!/usr/bin/env node
/**
 * Test mpc_designation against CSV file of known conversions.
 */

import * as fs from 'fs';
import * as readline from 'readline';
import { convertSimple } from '../src/mpc_designation';

interface TestError {
    input: string;
    got: string;
    expected: string;
}

async function runTests(csvFile: string, maxErrors: number = 100): Promise<boolean> {
    let total = 0;
    let passed = 0;
    let failed = 0;
    const errors: TestError[] = [];

    const startTime = Date.now();

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
        const expectedPacked = parts[1];

        let gotPacked: string;
        try {
            gotPacked = convertSimple(unpacked);
        } catch (e) {
            failed++;
            if (errors.length < maxErrors) {
                const message = e instanceof Error ? e.message : String(e);
                errors.push({ input: unpacked, got: `ERROR: ${message}`, expected: expectedPacked });
            }
            continue;
        }

        if (gotPacked !== expectedPacked) {
            failed++;
            if (errors.length < maxErrors) {
                errors.push({ input: unpacked, got: gotPacked, expected: expectedPacked });
            }
        } else {
            passed++;
        }

        // Progress indicator every 100,000 entries
        if (total % 100000 === 0) {
            process.stdout.write(`Processed ${total} entries...\n`);
        }
    }

    const elapsed = Date.now() - startTime;

    console.log();
    console.log('=== Test Results ===');
    console.log(`Total:  ${total}`);
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);
    console.log(`Time:   ${elapsed}ms (${(total / (elapsed / 1000)).toFixed(1)} entries/sec)`);
    console.log();

    if (failed > 0) {
        console.log(`=== First ${errors.length} failures ===`);
        console.log(`${'Input'.padEnd(25)} ${'Got'.padEnd(15)} ${'Expected'.padEnd(15)}`);
        console.log('-'.repeat(60));
        for (const { input, got, expected } of errors) {
            console.log(`${input.padEnd(25)} ${got.padEnd(15)} ${expected.padEnd(15)}`);
        }
    }

    return failed === 0;
}

async function main(): Promise<void> {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.error('Usage: test_csv <csv_file> [max_errors]');
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
