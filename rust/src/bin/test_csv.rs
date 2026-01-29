//! CSV Test Runner
//!
//! Test mpc_designation against CSV file of known conversions.

use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::process;
use std::time::Instant;
use mpc_designation::convert_simple;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: test_csv <csv_file> [max_errors]");
        process::exit(1);
    }

    let csv_file = &args[1];
    let max_errors: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(100);

    let file = File::open(csv_file).unwrap_or_else(|e| {
        eprintln!("Cannot open file {}: {}", csv_file, e);
        process::exit(1);
    });

    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // Skip header
    let _ = lines.next();

    let mut total = 0u64;
    let mut passed = 0u64;
    let mut failed = 0u64;
    let mut errors: Vec<(String, String, String)> = Vec::new();

    let start_time = Instant::now();

    for line_result in lines {
        let line = match line_result {
            Ok(l) => l,
            Err(_) => continue,
        };

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        total += 1;

        let parts: Vec<&str> = line.splitn(3, ',').collect();
        if parts.len() < 2 {
            continue;
        }

        let unpacked = parts[0];
        let expected_packed = parts[1];

        match convert_simple(unpacked) {
            Ok(got_packed) => {
                if got_packed == expected_packed {
                    passed += 1;
                } else {
                    failed += 1;
                    if errors.len() < max_errors {
                        errors.push((unpacked.to_string(), got_packed, expected_packed.to_string()));
                    }
                }
            }
            Err(e) => {
                failed += 1;
                if errors.len() < max_errors {
                    errors.push((unpacked.to_string(), format!("ERROR: {}", e), expected_packed.to_string()));
                }
            }
        }

        if total % 100000 == 0 {
            println!("Processed {} entries...", total);
        }
    }

    let elapsed = start_time.elapsed();
    let elapsed_ms = elapsed.as_millis();
    let rate = if elapsed_ms > 0 {
        (total as f64) / (elapsed_ms as f64 / 1000.0)
    } else {
        0.0
    };

    println!();
    println!("=== Test Results ===");
    println!("Total:  {}", total);
    println!("Passed: {}", passed);
    println!("Failed: {}", failed);
    println!("Time:   {}ms ({:.1} entries/sec)", elapsed_ms, rate);
    println!();

    if !errors.is_empty() {
        println!("=== First {} failures ===", errors.len());
        println!("{:<25} {:<15} {:<15}", "Input", "Got", "Expected");
        println!("{}", "-".repeat(60));
        for (inp, got, expected) in &errors {
            println!("{:<25} {:<15} {:<15}", inp, got, expected);
        }
    }

    process::exit(if failed > 0 { 1 } else { 0 });
}
