#!/usr/bin/env node
/**
 * Test mpc_designation.js error handling.
 *
 * Tests various classes of invalid input to ensure proper error detection.
 * Reads test cases from error_test_cases.csv.
 *
 * Usage: node test_errors.js <csv_file>
 */

'use strict';

const fs = require('fs');
const readline = require('readline');

const { convertSimple, MPCDesignationError } = require('../src/mpc_designation');

/**
 * Parse escape sequences in a string.
 */
function unescapeString(s) {
    // Handle \xNN hex escapes
    s = s.replace(/\\x([0-9a-fA-F]{2})/g, (_, hex) => String.fromCharCode(parseInt(hex, 16)));

    // Handle standard escapes
    s = s.replace(/\\n/g, '\n');
    s = s.replace(/\\r/g, '\r');
    s = s.replace(/\\t/g, '\t');
    s = s.replace(/\\f/g, '\f');
    s = s.replace(/\\v/g, '\v');
    s = s.replace(/\\0/g, '\0');
    s = s.replace(/\\\\/g, '\\');

    return s;
}

async function runErrorTests(csvFile) {
    let total = 0;
    let passed = 0;
    let failed = 0;

    console.log('=== MPC Designation Error Tests ===\n');

    const fileStream = fs.createReadStream(csvFile);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    for await (const line of rl) {
        const trimmedLine = line.trim();

        // Skip empty lines and comments
        if (!trimmedLine || trimmedLine.startsWith('#')) {
            continue;
        }

        // Skip header
        if (trimmedLine.startsWith('category,')) {
            continue;
        }

        // Parse CSV (simple split)
        const parts = trimmedLine.split(',');
        if (parts.length < 5) {
            continue;
        }

        const category = parts[0];
        const subcategory = parts[1];
        const inputStr = unescapeString(parts[2]);
        const expectedError = parts[3];
        const description = parts.slice(4).join(','); // In case description has commas

        total++;

        // Run the test
        let gotError = false;
        let errorMsg = '';
        let output = '';

        try {
            output = convertSimple(inputStr);
        } catch (e) {
            gotError = true;
            errorMsg = e.message || String(e);
        }

        let testPassed = false;

        if (expectedError === 'valid') {
            // Expect success
            if (!gotError) {
                testPassed = true;
            } else {
                console.log(`FAIL [${category}/${subcategory}]: '${description}'`);
                console.log(`      Expected: valid conversion`);
                console.log(`      Got:      ${errorMsg}`);
                failed++;
            }
        } else {
            // Expect error
            if (gotError) {
                testPassed = true;
            } else {
                console.log(`FAIL [${category}/${subcategory}]: '${description}'`);
                console.log(`      Expected: error (${expectedError})`);
                console.log(`      Got:      '${output}' (success)`);
                failed++;
            }
        }

        if (testPassed) {
            passed++;
        }
    }

    console.log('\n=== Error Test Results ===');
    console.log(`Total:  ${total}`);
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);

    return failed === 0;
}

async function main() {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.error('Usage: node test_errors.js <csv_file>');
        process.exit(1);
    }

    const csvFile = args[0];

    try {
        const success = await runErrorTests(csvFile);
        process.exit(success ? 0 : 1);
    } catch (err) {
        console.error(`Error: ${err.message}`);
        process.exit(1);
    }
}

main();
