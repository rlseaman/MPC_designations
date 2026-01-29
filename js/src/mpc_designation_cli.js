#!/usr/bin/env node
/**
 * mpc_designation_cli.js - Command-line interface for MPC designation conversion
 *
 * Usage: node mpc_designation_cli.js [-v|--verbose] <designation> [designation ...]
 */

'use strict';

const { convert, MPCDesignationError } = require('./mpc_designation');

function printUsage() {
    console.error('Usage: mpc_designation_cli.js [-v|--verbose] <designation> [designation ...]');
    console.error('');
    console.error('Convert between packed and unpacked MPC designations.');
    console.error('Auto-detects the input format and converts to the other.');
    console.error('');
    console.error('Options:');
    console.error('  -v, --verbose   Show detailed information about the conversion');
    console.error('');
    console.error('Examples:');
    console.error('  mpc_designation_cli.js 00001             -> 1');
    console.error('  mpc_designation_cli.js 1                 -> 00001');
    console.error('  mpc_designation_cli.js J95X00A           -> 1995 XA');
    console.error("  mpc_designation_cli.js '1995 XA'         -> J95X00A");
    console.error("  mpc_designation_cli.js 'C/1995 O1'       -> CJ95O010");
    console.error('  mpc_designation_cli.js 1P                -> 0001P');
}

function main() {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        printUsage();
        process.exit(1);
    }

    let verbose = false;
    const designations = [];

    for (const arg of args) {
        if (arg === '-v' || arg === '--verbose') {
            verbose = true;
        } else if (arg === '-h' || arg === '--help') {
            printUsage();
            process.exit(0);
        } else {
            designations.push(arg);
        }
    }

    if (designations.length === 0) {
        printUsage();
        process.exit(1);
    }

    const multiple = designations.length > 1;

    for (const des of designations) {
        try {
            const result = convert(des);
            const info = result.info;
            const output = result.output;

            if (verbose) {
                console.log(`  Input:    ${des}`);
                console.log(`  Detected: ${info.format} format, ${info.subtype}`);
                const action = info.format === 'packed'
                    ? 'unpacking to human-readable form'
                    : 'packing to MPC compact form';
                console.log(`  Action:   ${action}`);
                console.log(`  Output:   ${output}`);
                if (multiple) {
                    console.log();
                }
            } else if (multiple) {
                console.log(`${des} -> ${output}`);
            } else {
                console.log(output);
            }
        } catch (e) {
            if (e instanceof MPCDesignationError) {
                console.error(`Error: ${e.message}`);
            } else {
                console.error(`Error: ${e}`);
            }
            process.exit(1);
        }
    }
}

main();
