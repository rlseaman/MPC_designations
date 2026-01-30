#!/usr/bin/env node
/**
 * Command-line interface for MPC designation conversion.
 */

import { convert, VERSION, MPCDesignationError } from './mpc_designation';

function printUsage(): void {
    console.log('Usage: mpc_designation [options] <designation>');
    console.log();
    console.log('Convert between packed and unpacked MPC designations.');
    console.log();
    console.log('Options:');
    console.log('  -v, --verbose   Show detailed format information');
    console.log('  -h, --help      Show this help message');
    console.log('  --version       Show version');
    console.log();
    console.log('Examples:');
    console.log("  mpc_designation '1995 XA'     # Convert to packed: J95X00A");
    console.log('  mpc_designation J95X00A       # Convert to unpacked: 1995 XA');
    console.log("  mpc_designation -v '1P'       # Show verbose output");
}

function main(): void {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        printUsage();
        process.exit(1);
    }

    let verbose = false;
    let designation: string | null = null;

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg === '-h' || arg === '--help') {
            printUsage();
            process.exit(0);
        } else if (arg === '--version') {
            console.log(`mpc_designation ${VERSION}`);
            process.exit(0);
        } else if (arg === '-v' || arg === '--verbose') {
            verbose = true;
        } else {
            // Collect remaining args as the designation
            designation = args.slice(i).join(' ');
            break;
        }
    }

    if (!designation) {
        console.error('Error: No designation provided');
        printUsage();
        process.exit(1);
    }

    try {
        const result = convert(designation);

        if (verbose) {
            console.log(`Input:   ${result.input}`);
            console.log(`Output:  ${result.output}`);
            console.log(`Format:  ${result.info.format}`);
            console.log(`Type:    ${result.info.type}`);
            console.log(`Subtype: ${result.info.subtype}`);
        } else {
            console.log(result.output);
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

main();
