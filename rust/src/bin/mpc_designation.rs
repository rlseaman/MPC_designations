//! MPC Designation CLI
//!
//! Command-line interface for converting MPC designations.

use std::env;
use std::process;
use mpc_designation::convert;

fn print_usage() {
    eprintln!("Usage: mpc_designation [-v|--verbose] <designation> [designation ...]");
    eprintln!();
    eprintln!("Convert between packed and unpacked MPC designations.");
    eprintln!("Auto-detects the input format and converts to the other.");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -v, --verbose   Show detailed information about the conversion");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  mpc_designation 00001             -> 1");
    eprintln!("  mpc_designation 1                 -> 00001");
    eprintln!("  mpc_designation J95X00A           -> 1995 XA");
    eprintln!("  mpc_designation '1995 XA'         -> J95X00A");
    eprintln!("  mpc_designation 'C/1995 O1'       -> CJ95O010");
    eprintln!("  mpc_designation 1P                -> 0001P");
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() {
        print_usage();
        process::exit(1);
    }

    let mut verbose = false;
    let mut designations = Vec::new();

    for arg in &args {
        match arg.as_str() {
            "-v" | "--verbose" => verbose = true,
            "-h" | "--help" => {
                print_usage();
                process::exit(0);
            }
            _ => designations.push(arg.clone()),
        }
    }

    if designations.is_empty() {
        print_usage();
        process::exit(1);
    }

    let multiple = designations.len() > 1;

    for des in &designations {
        match convert(des) {
            Ok(result) => {
                if verbose {
                    println!("  Input:    {}", des);
                    println!("  Detected: {} format, {}", result.info.format, result.info.subtype);
                    let action = if result.info.format == "packed" {
                        "unpacking to human-readable form"
                    } else {
                        "packing to MPC compact form"
                    };
                    println!("  Action:   {}", action);
                    println!("  Output:   {}", result.output);
                    if multiple {
                        println!();
                    }
                } else if multiple {
                    println!("{} -> {}", des, result.output);
                } else {
                    println!("{}", result.output);
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                process::exit(1);
            }
        }
    }
}
