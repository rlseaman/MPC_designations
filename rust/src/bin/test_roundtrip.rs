//! Round-trip Test Runner
//!
//! Bidirectional testing with round-trip verification.

use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::process;
use std::time::Instant;
use mpc_designation::convert_simple;

const MAX_ERRORS: usize = 20;

struct TestError {
    phase: String,
    input: String,
    got: String,
    expected: String,
}

fn status_str(failed: u64) -> String {
    if failed == 0 {
        "PASS".to_string()
    } else {
        format!("FAIL ({})", failed)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: test_roundtrip <csv_file>");
        process::exit(1);
    }

    let csv_file = &args[1];

    // Load test data
    let file = File::open(csv_file).unwrap_or_else(|e| {
        eprintln!("Cannot open file {}: {}", csv_file, e);
        process::exit(1);
    });

    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // Skip header
    let _ = lines.next();

    let mut unpacked_list: Vec<String> = Vec::new();
    let mut packed_list: Vec<String> = Vec::new();

    for line_result in lines {
        let line = match line_result {
            Ok(l) => l,
            Err(_) => continue,
        };

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let parts: Vec<&str> = line.splitn(3, ',').collect();
        if parts.len() >= 2 {
            unpacked_list.push(parts[0].to_string());
            packed_list.push(parts[1].to_string());
        }
    }

    let total = unpacked_list.len();
    println!("Loaded {} test cases\n", total);

    let mut errors: Vec<TestError> = Vec::new();

    // ========== Phase 1: Pack (unpacked -> packed) ==========
    println!("=== Phase 1: Pack (unpacked -> packed) ===");
    let mut pack_passed = 0u64;
    let mut pack_failed = 0u64;
    let start_time = Instant::now();

    for i in 0..total {
        let unpacked = &unpacked_list[i];
        let expected = &packed_list[i];

        match convert_simple(unpacked) {
            Ok(got) => {
                if got == *expected {
                    pack_passed += 1;
                } else {
                    pack_failed += 1;
                    if errors.len() < MAX_ERRORS {
                        errors.push(TestError {
                            phase: "pack".to_string(),
                            input: unpacked.clone(),
                            got,
                            expected: expected.clone(),
                        });
                    }
                }
            }
            Err(e) => {
                pack_failed += 1;
                if errors.len() < MAX_ERRORS {
                    errors.push(TestError {
                        phase: "pack".to_string(),
                        input: unpacked.clone(),
                        got: format!("ERROR: {}", e),
                        expected: expected.clone(),
                    });
                }
            }
        }
    }

    let pack_time = start_time.elapsed().as_millis();
    let pack_rate = if pack_time > 0 { (total as f64) / (pack_time as f64 / 1000.0) } else { 0.0 };
    println!("Passed: {}", pack_passed);
    println!("Failed: {}", pack_failed);
    println!("Time:   {}ms ({:.1} entries/sec)\n", pack_time, pack_rate);

    // ========== Phase 2: Unpack (packed -> unpacked) ==========
    println!("=== Phase 2: Unpack (packed -> unpacked) ===");
    let mut unpack_passed = 0u64;
    let mut unpack_failed = 0u64;
    let start_time = Instant::now();

    for i in 0..total {
        let packed = &packed_list[i];
        let expected = &unpacked_list[i];

        match convert_simple(packed) {
            Ok(got) => {
                if got == *expected {
                    unpack_passed += 1;
                } else {
                    unpack_failed += 1;
                    if errors.len() < MAX_ERRORS {
                        errors.push(TestError {
                            phase: "unpack".to_string(),
                            input: packed.clone(),
                            got,
                            expected: expected.clone(),
                        });
                    }
                }
            }
            Err(e) => {
                unpack_failed += 1;
                if errors.len() < MAX_ERRORS {
                    errors.push(TestError {
                        phase: "unpack".to_string(),
                        input: packed.clone(),
                        got: format!("ERROR: {}", e),
                        expected: expected.clone(),
                    });
                }
            }
        }
    }

    let unpack_time = start_time.elapsed().as_millis();
    let unpack_rate = if unpack_time > 0 { (total as f64) / (unpack_time as f64 / 1000.0) } else { 0.0 };
    println!("Passed: {}", unpack_passed);
    println!("Failed: {}", unpack_failed);
    println!("Time:   {}ms ({:.1} entries/sec)\n", unpack_time, unpack_rate);

    // ========== Phase 3: Unpacked round-trip ==========
    println!("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===");
    let mut rt_unpacked_passed = 0u64;
    let mut rt_unpacked_failed = 0u64;
    let start_time = Instant::now();

    for i in 0..total {
        let original = &unpacked_list[i];

        let result = convert_simple(original).and_then(|packed| convert_simple(&packed));

        match result {
            Ok(unpacked) => {
                if unpacked == *original {
                    rt_unpacked_passed += 1;
                } else {
                    rt_unpacked_failed += 1;
                }
            }
            Err(_) => {
                rt_unpacked_failed += 1;
            }
        }
    }

    let rt_unpacked_time = start_time.elapsed().as_millis();
    let rt_unpacked_rate = if rt_unpacked_time > 0 { (total as f64) / (rt_unpacked_time as f64 / 1000.0) } else { 0.0 };
    println!("Passed: {}", rt_unpacked_passed);
    println!("Failed: {}", rt_unpacked_failed);
    println!("Time:   {}ms ({:.1} entries/sec)\n", rt_unpacked_time, rt_unpacked_rate);

    // ========== Phase 4: Packed round-trip ==========
    println!("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===");
    errors.clear();
    let mut rt_packed_passed = 0u64;
    let mut rt_packed_failed = 0u64;
    let start_time = Instant::now();

    for i in 0..total {
        let original = &packed_list[i];

        let result = convert_simple(original).and_then(|unpacked| convert_simple(&unpacked));

        match result {
            Ok(repacked) => {
                if repacked == *original {
                    rt_packed_passed += 1;
                } else {
                    rt_packed_failed += 1;
                    if errors.len() < MAX_ERRORS {
                        let unpacked = convert_simple(original).unwrap_or_default();
                        errors.push(TestError {
                            phase: "rt-pak".to_string(),
                            input: original.clone(),
                            got: format!("{} -> {}", unpacked, repacked),
                            expected: original.clone(),
                        });
                    }
                }
            }
            Err(e) => {
                rt_packed_failed += 1;
                if errors.len() < MAX_ERRORS {
                    errors.push(TestError {
                        phase: "rt-pak".to_string(),
                        input: original.clone(),
                        got: format!("ERROR: {}", e),
                        expected: original.clone(),
                    });
                }
            }
        }
    }

    let rt_packed_time = start_time.elapsed().as_millis();
    let rt_packed_rate = if rt_packed_time > 0 { (total as f64) / (rt_packed_time as f64 / 1000.0) } else { 0.0 };
    println!("Passed: {}", rt_packed_passed);
    println!("Failed: {}", rt_packed_failed);
    println!("Time:   {}ms ({:.1} entries/sec)\n", rt_packed_time, rt_packed_rate);

    // ========== Summary ==========
    println!("=== Summary ===");
    println!("{:<30} {:>10} {:>18} {:>12}", "Phase", "Time (ms)", "Rate (entries/sec)", "Status");
    println!("{:<30} {:>10} {:>18} {:>12}", "-".repeat(30), "-".repeat(10), "-".repeat(18), "-".repeat(12));
    println!("{:<30} {:>10} {:>18.1} {:>12}", "Pack", pack_time, pack_rate, status_str(pack_failed));
    println!("{:<30} {:>10} {:>18.1} {:>12}", "Unpack", unpack_time, unpack_rate, status_str(unpack_failed));
    println!("{:<30} {:>10} {:>18.1} {:>12}", "Unpacked RT: unpack(pack(x))=x", rt_unpacked_time, rt_unpacked_rate, status_str(rt_unpacked_failed));
    println!("{:<30} {:>10} {:>18.1} {:>12}", "Packed RT: pack(unpack(y))=y", rt_packed_time, rt_packed_rate, status_str(rt_packed_failed));
    println!();

    // Show errors
    if !errors.is_empty() {
        println!("=== First {} errors ===", errors.len());
        println!("{:<8} {:<25} {:<20} {:<20}", "Phase", "Input", "Got", "Expected");
        println!("{:<8} {:<25} {:<20} {:<20}", "-".repeat(8), "-".repeat(25), "-".repeat(20), "-".repeat(20));
        for e in &errors {
            println!("{:<8} {:<25} {:<20} {:<20}", e.phase, e.input, e.got, e.expected);
        }
    }

    // Exit with error only if pack or packed RT failed
    let total_failed = pack_failed + rt_packed_failed;
    process::exit(if total_failed > 0 { 1 } else { 0 });
}
