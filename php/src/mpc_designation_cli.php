#!/usr/bin/env php
<?php
/**
 * CLI for MPC designation converter
 */

declare(strict_types=1);

require_once __DIR__ . '/MPCDesignation.php';

use MPC\MPCDesignation;
use MPC\MPCDesignationException;

function printUsage(): void {
    $usage = <<<EOF
Usage: php mpc_designation_cli.php [-v|--verbose] <designation> [designation ...]

Convert between packed and unpacked MPC designations.
Auto-detects the input format and converts to the other.

Options:
  -v, --verbose   Show detailed information about the conversion
  --version       Show version information

Examples:
  php mpc_designation_cli.php 00001             -> 1
  php mpc_designation_cli.php 1                 -> 00001
  php mpc_designation_cli.php J95X00A           -> 1995 XA
  php mpc_designation_cli.php '1995 XA'         -> J95X00A
  php mpc_designation_cli.php 'C/1995 O1'       -> CJ95O010
  php mpc_designation_cli.php 1P                -> 0001P
EOF;
    fwrite(STDERR, $usage . "\n");
}

function main(array $argv): int {
    $args = array_slice($argv, 1);

    if (count($args) === 0) {
        printUsage();
        return 1;
    }

    $verbose = false;
    $designations = [];

    foreach ($args as $arg) {
        switch ($arg) {
            case '-v':
            case '--verbose':
                $verbose = true;
                break;
            case '-h':
            case '--help':
                printUsage();
                return 0;
            case '--version':
                echo "mpc_designation " . MPCDesignation::VERSION . "\n";
                return 0;
            default:
                $designations[] = $arg;
                break;
        }
    }

    if (count($designations) === 0) {
        printUsage();
        return 1;
    }

    $multiple = count($designations) > 1;

    foreach ($designations as $des) {
        try {
            $result = MPCDesignation::convert($des);
            $info = $result->info;

            if ($verbose) {
                echo "  Input:    $des\n";
                echo "  Detected: {$info->format} format, {$info->subtype}\n";
                $action = $info->format === 'packed'
                    ? 'unpacking to human-readable form'
                    : 'packing to MPC compact form';
                echo "  Action:   $action\n";
                echo "  Output:   {$result->output}\n";
                if ($multiple) {
                    echo "\n";
                }
            } elseif ($multiple) {
                echo "$des -> {$result->output}\n";
            } else {
                echo "{$result->output}\n";
            }
        } catch (MPCDesignationException $e) {
            fwrite(STDERR, "Error: " . $e->getMessage() . "\n");
            return 1;
        }
    }

    return 0;
}

exit(main($argv));
