#!/usr/bin/env node
/**
 * Test MPC designation error handling.
 */

import * as fs from 'fs';
import * as readline from 'readline';
import { convert } from '../src/mpc_designation';

/**
 * Convert escape sequences in test input.
 */
function unescapeInput(input: string): string {
    let result = input
        .replace(/\\t/g, '\t')
        .replace(/\\n/g, '\n')
        .replace(/\\r/g, '\r')
        .replace(/\\f/g, '\f')
        .replace(/\\v/g, '\v');

    // Handle \x00 style escapes
    const sb: string[] = [];
    let i = 0;
    while (i < result.length) {
        if (i + 3 < result.length && result[i] === '\\' && result[i + 1] === 'x') {
            const hexStr = result.substring(i + 2, i + 4);
            const value = parseInt(hexStr, 16);
            if (!isNaN(value)) {
                sb.push(String.fromCharCode(value));
                i += 4;
            } else {
                sb.push(result[i]);
                i++;
            }
        } else {
            sb.push(result[i]);
            i++;
        }
    }

    return sb.join('');
}

async function runTests(csvFile: string): Promise<boolean> {
    let total = 0;
    let passed = 0;
    let failed = 0;

    console.log('=== MPC Designation Error Tests (TypeScript) ===');
    console.log();

    const fileStream = fs.createReadStream(csvFile);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    for await (const line of rl) {
        // Skip empty lines, comments, and header
        if (!line || line.startsWith('#') || line.startsWith('category,')) {
            continue;
        }

        // Parse CSV line
        const parts = line.split(',');
        if (parts.length < 5) {
            continue;
        }

        const category = parts[0];
        const subcategory = parts[1];
        const rawInput = parts[2];
        const expectedError = parts[3];
        const description = parts[4];

        const input = unescapeInput(rawInput);
        total++;

        let gotError = false;
        let errorType = '';
        let resultOutput = '';

        try {
            const result = convert(input);
            resultOutput = result.output;
        } catch (e) {
            gotError = true;
            const errStr = e instanceof Error ? e.message : String(e);
            if (errStr.includes('out of range') || errStr.includes('Invalid asteroid number')) {
                errorType = 'range';
            } else {
                errorType = 'format';
            }
        }

        const testId = `${category}/${subcategory}`;

        if (expectedError === 'valid') {
            // Should succeed
            if (gotError) {
                failed++;
                console.log(`FAIL [${testId}]: '${description}'`);
                console.log(`      Expected: valid conversion`);
                console.log(`      Got:      error (${errorType})`);
            } else {
                passed++;
            }
        } else {
            // Should fail
            if (!gotError) {
                failed++;
                console.log(`FAIL [${testId}]: '${description}'`);
                console.log(`      Expected: error (${expectedError})`);
                console.log(`      Got:      '${resultOutput}' (success)`);
            } else {
                passed++;
            }
        }
    }

    console.log();
    console.log('=== Error Test Results ===');
    console.log(`Total:  ${total}`);
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);

    return failed === 0;
}

async function main(): Promise<void> {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.error('Usage: test_errors <error_test_cases.csv>');
        process.exit(1);
    }

    const csvFile = args[0];
    const success = await runTests(csvFile);
    process.exit(success ? 0 : 1);
}

main().catch(err => {
    console.error(err);
    process.exit(1);
});
