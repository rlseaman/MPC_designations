//! Helper Function Test Runner
//!
//! Test mpc_designation helper functions.
//!
//! Usage: test_helpers

use std::process;
use mpc_designation::{
    to_report_format, from_report_format,
    has_fragment, get_fragment, get_parent,
    designations_equal,
};

struct TestStats {
    passed: u32,
    failed: u32,
}

impl TestStats {
    fn new() -> Self {
        TestStats { passed: 0, failed: 0 }
    }

    fn pass(&mut self) {
        self.passed += 1;
    }

    fn fail(&mut self) {
        self.failed += 1;
    }

    fn total(&self) -> u32 {
        self.passed + self.failed
    }
}

fn test_to_report(stats: &mut TestStats, input: &str, expected: &str, _desc: &str) {
    match to_report_format(input) {
        Ok(result) => {
            if result == expected {
                println!("  PASS: to_report_format(\"{}\") -> \"{}\"", input, result);
                stats.pass();
            } else {
                println!("  FAIL: to_report_format(\"{}\"): expected \"{}\", got \"{}\"", input, expected, result);
                stats.fail();
            }
        }
        Err(e) => {
            println!("  FAIL: to_report_format(\"{}\"): error - {}", input, e);
            stats.fail();
        }
    }
}

fn test_from_report(stats: &mut TestStats, input: &str, expected: &str, _desc: &str) {
    match from_report_format(input) {
        Ok(result) => {
            if result == expected {
                println!("  PASS: from_report_format(\"{}\") -> \"{}\"", input, result);
                stats.pass();
            } else {
                println!("  FAIL: from_report_format(\"{}\"): expected \"{}\", got \"{}\"", input, expected, result);
                stats.fail();
            }
        }
        Err(e) => {
            println!("  FAIL: from_report_format(\"{}\"): error - {}", input, e);
            stats.fail();
        }
    }
}

fn test_has_fragment(stats: &mut TestStats, input: &str, expected: bool, _desc: &str) {
    let result = has_fragment(input);
    if result == expected {
        println!("  PASS: has_fragment(\"{}\") -> {}", input, result);
        stats.pass();
    } else {
        println!("  FAIL: has_fragment(\"{}\"): expected {}, got {}", input, expected, result);
        stats.fail();
    }
}

fn test_get_fragment(stats: &mut TestStats, input: &str, expected: &str, _desc: &str) {
    match get_fragment(input) {
        Ok(result) => {
            if result == expected {
                println!("  PASS: get_fragment(\"{}\") -> \"{}\"", input, result);
                stats.pass();
            } else {
                println!("  FAIL: get_fragment(\"{}\"): expected \"{}\", got \"{}\"", input, expected, result);
                stats.fail();
            }
        }
        Err(e) => {
            println!("  FAIL: get_fragment(\"{}\"): error - {}", input, e);
            stats.fail();
        }
    }
}

fn test_get_parent(stats: &mut TestStats, input: &str, expected: &str, _desc: &str) {
    match get_parent(input) {
        Ok(result) => {
            if result == expected {
                println!("  PASS: get_parent(\"{}\") -> \"{}\"", input, result);
                stats.pass();
            } else {
                println!("  FAIL: get_parent(\"{}\"): expected \"{}\", got \"{}\"", input, expected, result);
                stats.fail();
            }
        }
        Err(e) => {
            println!("  FAIL: get_parent(\"{}\"): error - {}", input, e);
            stats.fail();
        }
    }
}

fn test_equal(stats: &mut TestStats, d1: &str, d2: &str, expected: bool, _desc: &str) {
    let result = designations_equal(d1, d2);
    if result == expected {
        println!("  PASS: designations_equal(\"{}\", \"{}\") -> {}", d1, d2, result);
        stats.pass();
    } else {
        println!("  FAIL: designations_equal(\"{}\", \"{}\"): expected {}, got {}", d1, d2, expected, result);
        stats.fail();
    }
}

fn main() {
    let mut stats = TestStats::new();

    println!("=== MPC Designation Helper Function Tests (Rust) ===");
    println!();

    // Test to_report_format
    println!("--- to_report_format ---");

    // Numbered asteroids
    test_to_report(&mut stats, "00001", "       00001", "Numbered asteroid 1");
    test_to_report(&mut stats, "00433", "       00433", "Numbered asteroid 433");
    test_to_report(&mut stats, "99999", "       99999", "Numbered asteroid 99999");
    test_to_report(&mut stats, "A0000", "       A0000", "Numbered asteroid 100000");
    test_to_report(&mut stats, "~0000", "       ~0000", "Numbered asteroid 620000");

    // Provisional asteroids
    test_to_report(&mut stats, "J95X00A", "     J95X00A", "Provisional 1995 XA");
    test_to_report(&mut stats, "K24A12B", "     K24A12B", "Provisional 2024 AB12");

    // Survey designations
    test_to_report(&mut stats, "PLS2040", "     PLS2040", "Survey P-L");
    test_to_report(&mut stats, "T3S3141", "     T3S3141", "Survey T-3");

    // Numbered comets
    test_to_report(&mut stats, "0001P", "0001P       ", "Comet 1P");
    test_to_report(&mut stats, "0073P", "0073P       ", "Comet 73P");

    // Numbered comets with fragments
    test_to_report(&mut stats, "0073Pa", "0073P      a", "Comet 73P-A");
    test_to_report(&mut stats, "0073Pb", "0073P      b", "Comet 73P-B");
    test_to_report(&mut stats, "0073Paa", "0073P     aa", "Comet 73P-AA");
    test_to_report(&mut stats, "0073Paz", "0073P     az", "Comet 73P-AZ");
    test_to_report(&mut stats, "0073Pzz", "0073P     zz", "Comet 73P-ZZ");

    // Provisional comets
    test_to_report(&mut stats, "CJ95O010", "    CJ95O010", "Comet C/1995 O1");
    test_to_report(&mut stats, "DJ93F020", "    DJ93F020", "Comet D/1993 F2");
    test_to_report(&mut stats, "DJ93F02a", "    DJ93F02a", "Comet D/1993 F2-A");

    // Test from_report_format
    println!();
    println!("--- from_report_format ---");

    // Numbered asteroids
    test_from_report(&mut stats, "       00001", "00001", "Numbered asteroid 1");
    test_from_report(&mut stats, "       00433", "00433", "Numbered asteroid 433");
    test_from_report(&mut stats, "       A0000", "A0000", "Numbered asteroid 100000");

    // Provisional asteroids
    test_from_report(&mut stats, "     J95X00A", "J95X00A", "Provisional 1995 XA");

    // Numbered comets
    test_from_report(&mut stats, "0073P       ", "0073P", "Comet 73P");

    // Numbered comets with fragments
    test_from_report(&mut stats, "0073P      a", "0073Pa", "Comet 73P-A");
    test_from_report(&mut stats, "0073P     aa", "0073Paa", "Comet 73P-AA");
    test_from_report(&mut stats, "0073P     az", "0073Paz", "Comet 73P-AZ");

    // Provisional comets
    test_from_report(&mut stats, "    CJ95O010", "CJ95O010", "Comet C/1995 O1");

    // Test has_fragment
    println!();
    println!("--- has_fragment ---");

    // Unpacked with fragments
    test_has_fragment(&mut stats, "73P-A", true, "Unpacked numbered comet with fragment");
    test_has_fragment(&mut stats, "73P-AA", true, "Unpacked numbered comet with 2-letter fragment");
    test_has_fragment(&mut stats, "D/1993 F2-A", true, "Unpacked provisional comet with fragment");
    test_has_fragment(&mut stats, "P/1930 J1-AA", true, "Unpacked provisional comet with 2-letter fragment");

    // Unpacked without fragments
    test_has_fragment(&mut stats, "73P", false, "Unpacked numbered comet no fragment");
    test_has_fragment(&mut stats, "C/1995 O1", false, "Unpacked provisional comet no fragment");

    // Packed with fragments
    test_has_fragment(&mut stats, "0073Pa", true, "Packed numbered comet with fragment");
    test_has_fragment(&mut stats, "0073Paa", true, "Packed numbered comet with 2-letter fragment");
    test_has_fragment(&mut stats, "DJ93F02a", true, "Packed provisional comet with fragment");

    // Packed without fragments
    test_has_fragment(&mut stats, "0073P", false, "Packed numbered comet no fragment");
    test_has_fragment(&mut stats, "CJ95O010", false, "Packed provisional comet no fragment");

    // Non-comets
    test_has_fragment(&mut stats, "1995 XA", false, "Asteroid no fragment");
    test_has_fragment(&mut stats, "00001", false, "Numbered asteroid");

    // Test get_fragment
    println!();
    println!("--- get_fragment ---");

    // Unpacked with fragments
    test_get_fragment(&mut stats, "73P-A", "A", "Unpacked single fragment");
    test_get_fragment(&mut stats, "73P-AA", "AA", "Unpacked 2-letter fragment");
    test_get_fragment(&mut stats, "73P-I", "I", "Unpacked fragment I");
    test_get_fragment(&mut stats, "D/1993 F2-B", "B", "Unpacked provisional fragment");
    test_get_fragment(&mut stats, "P/1930 J1-AZ", "AZ", "Unpacked provisional 2-letter");

    // Unpacked without fragments
    test_get_fragment(&mut stats, "73P", "", "Unpacked no fragment");
    test_get_fragment(&mut stats, "C/1995 O1", "", "Unpacked provisional no fragment");

    // Packed with fragments
    test_get_fragment(&mut stats, "0073Pa", "A", "Packed single fragment");
    test_get_fragment(&mut stats, "0073Paa", "AA", "Packed 2-letter fragment");
    test_get_fragment(&mut stats, "0073Pi", "I", "Packed fragment I");
    test_get_fragment(&mut stats, "DJ93F02b", "B", "Packed provisional fragment");

    // Packed without fragments
    test_get_fragment(&mut stats, "0073P", "", "Packed no fragment");
    test_get_fragment(&mut stats, "CJ95O010", "", "Packed provisional no fragment");

    // Test get_parent
    println!();
    println!("--- get_parent ---");

    // Unpacked with fragments
    test_get_parent(&mut stats, "73P-A", "73P", "Unpacked single fragment");
    test_get_parent(&mut stats, "73P-AA", "73P", "Unpacked 2-letter fragment");
    test_get_parent(&mut stats, "D/1993 F2-B", "D/1993 F2", "Unpacked provisional fragment");
    test_get_parent(&mut stats, "P/1930 J1-AA", "P/1930 J1", "Unpacked provisional 2-letter");

    // Unpacked without fragments
    test_get_parent(&mut stats, "73P", "73P", "Unpacked no fragment");
    test_get_parent(&mut stats, "C/1995 O1", "C/1995 O1", "Unpacked provisional no fragment");

    // Packed with fragments
    test_get_parent(&mut stats, "0073Pa", "0073P", "Packed single fragment");
    test_get_parent(&mut stats, "0073Paa", "0073P", "Packed 2-letter fragment");

    // Packed without fragments
    test_get_parent(&mut stats, "0073P", "0073P", "Packed no fragment");

    // Non-comets (should return as-is)
    test_get_parent(&mut stats, "1995 XA", "1995 XA", "Asteroid");
    test_get_parent(&mut stats, "00001", "00001", "Numbered asteroid");

    // Test designations_equal
    println!();
    println!("--- designations_equal ---");

    // Same designation, different formats
    test_equal(&mut stats, "1995 XA", "J95X00A", true, "Provisional packed/unpacked");
    test_equal(&mut stats, "73P", "0073P", true, "Numbered comet packed/unpacked");
    test_equal(&mut stats, "73P-A", "0073Pa", true, "Comet with fragment packed/unpacked");
    test_equal(&mut stats, "73P-AA", "0073Paa", true, "Comet with 2-letter fragment");
    test_equal(&mut stats, "1", "00001", true, "Numbered asteroid");
    test_equal(&mut stats, "C/1995 O1", "CJ95O010", true, "Provisional comet");

    // Different designations
    test_equal(&mut stats, "1995 XA", "1995 XB", false, "Different provisional");
    test_equal(&mut stats, "73P-A", "73P-B", false, "Different fragments");
    test_equal(&mut stats, "73P", "74P", false, "Different comet numbers");
    test_equal(&mut stats, "1", "2", false, "Different asteroid numbers");

    // Same designation (both packed or both unpacked)
    test_equal(&mut stats, "1995 XA", "1995 XA", true, "Same unpacked");
    test_equal(&mut stats, "J95X00A", "J95X00A", true, "Same packed");

    // Summary
    println!();
    println!("==================================================");
    println!("Total: {}, Passed: {}, Failed: {}", stats.total(), stats.passed, stats.failed);
    process::exit(if stats.failed > 0 { 1 } else { 0 });
}
