//! Error Test Runner
//!
//! Test mpc_designation error handling against CSV file of invalid inputs.
//!
//! Usage: test_errors <csv_file>

use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::process;
use mpc_designation::convert_simple;

/// Parse escape sequences in a string.
fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('x') => {
                    // Handle \xNN hex escape
                    let hex: String = chars.by_ref().take(2).collect();
                    if hex.len() == 2 {
                        if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                            result.push(byte as char);
                            continue;
                        }
                    }
                    result.push_str("\\x");
                    result.push_str(&hex);
                }
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('0') => result.push('\0'),
                Some('\\') => result.push('\\'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: test_errors <csv_file>");
        process::exit(1);
    }

    let csv_file = &args[1];

    let file = File::open(csv_file).unwrap_or_else(|e| {
        eprintln!("Cannot open file {}: {}", csv_file, e);
        process::exit(1);
    });

    let reader = BufReader::new(file);

    let mut total = 0u64;
    let mut passed = 0u64;
    let mut failed = 0u64;

    println!("=== MPC Designation Error Tests ===\n");

    for line_result in reader.lines() {
        let line = match line_result {
            Ok(l) => l,
            Err(_) => continue,
        };

        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Skip header
        if line.starts_with("category,") {
            continue;
        }

        // Parse CSV (simple split)
        let parts: Vec<&str> = line.splitn(5, ',').collect();
        if parts.len() < 5 {
            continue;
        }

        let category = parts[0];
        let subcategory = parts[1];
        let input_str = unescape_string(parts[2]);
        let expected_error = parts[3];
        let description = parts[4];

        total += 1;

        // Run the test
        let result = convert_simple(&input_str);
        let got_error = result.is_err();

        let test_passed = if expected_error == "valid" {
            // Expect success
            if got_error {
                println!("FAIL [{}/{}]: '{}'", category, subcategory, description);
                println!("      Expected: valid conversion");
                println!("      Got:      {}", result.unwrap_err());
                failed += 1;
                false
            } else {
                true
            }
        } else {
            // Expect error
            if got_error {
                true
            } else {
                println!("FAIL [{}/{}]: '{}'", category, subcategory, description);
                println!("      Expected: error ({})", expected_error);
                println!("      Got:      '{}' (success)", result.unwrap());
                failed += 1;
                false
            }
        };

        if test_passed {
            passed += 1;
        }
    }

    println!("\n=== Error Test Results ===");
    println!("Total:  {}", total);
    println!("Passed: {}", passed);
    println!("Failed: {}", failed);

    process::exit(if failed > 0 { 1 } else { 0 });
}
