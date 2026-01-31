#!/usr/bin/env python3
"""
Chain Test: Connect implementations from slowest to fastest.

Each implementation alternates between pack and unpack operations,
passing output to the next implementation in the chain. Data is randomly
shuffled between each step to test robustness.

Verifies that after all transformations, the final output matches
the original input (round-trip integrity).

Usage:
    python3 scripts/chain_test.py                    # Full test (2M+ entries)
    python3 scripts/chain_test.py --sample 10000     # Quick test with 10k entries
    python3 scripts/chain_test.py --skip-preflight   # Skip implementation checks
    python3 scripts/chain_test.py --no-shuffle       # Disable random shuffling
    python3 scripts/chain_test.py --fast             # Skip slowest implementations

Runtime Notes:
    JVM/CLR/JIT languages (Java, Kotlin, C#, Julia) have extreme startup overhead
    when invoked as subprocesses. In benchmarks they're fast, but subprocess
    invocation requires full JIT compilation on each call:

    - Julia: ~1 entry/sec (JIT compilation on each invocation)
    - C#:    ~3 entries/sec (CLR startup overhead)
    - Java/Kotlin: ~7-8 entries/sec (JVM startup overhead)

    For 2M entries through all 20 implementations, expect multi-hour runtime.
    Use --sample or --fast for quicker testing.
"""

import subprocess
import sys
import os
import time
import random
from pathlib import Path

# Force unbuffered output
sys.stdout.reconfigure(line_buffering=True)
sys.stderr.reconfigure(line_buffering=True)

# Base directory
BASE_DIR = Path(__file__).parent.parent

# Implementations with extreme subprocess overhead (JVM/CLR/JIT startup)
SLOW_IMPLEMENTATIONS = {"Java", "Kotlin", "C#", "Julia"}

# Implementations ordered from slowest to fastest (pack direction benchmarks)
# Excludes Bash and Forth
IMPLEMENTATIONS = [
    # (name, command) - all CLIs auto-detect pack vs unpack based on input format
    ("Tcl",
     ["tclsh", "src/mpc_designation_cli.tcl"],
     ["tclsh", "src/mpc_designation_cli.tcl"]),
    ("Ruby",
     ["ruby", "src/mpc_designation_cli.rb"],
     ["ruby", "src/mpc_designation_cli.rb"]),
    ("Perl",
     ["perl", "-Isrc", "-MMPC::Designation=convert_simple", "-e", "print convert_simple($_) . qq{\\n} for @ARGV"],
     ["perl", "-Isrc", "-MMPC::Designation=convert_simple", "-e", "print convert_simple($_) . qq{\\n} for @ARGV"]),
    ("C++",
     ["./mpc_designation"],
     ["./mpc_designation"]),
    ("Swift",
     ["./mpc_designation"],
     ["./mpc_designation"]),
    ("Python",
     ["python3", "-c", "import sys; sys.path.insert(0,'src'); from mpc_designation import convert_simple; [print(convert_simple(a)) for a in sys.argv[1:]]"],
     ["python3", "-c", "import sys; sys.path.insert(0,'src'); from mpc_designation import convert_simple; [print(convert_simple(a)) for a in sys.argv[1:]]"]),
    ("AWK",  # Uses stdin
     ["awk", "-f", "src/mpc_designation.awk", "-f", "src/mpc_designation_main.awk"],
     ["awk", "-f", "src/mpc_designation.awk", "-f", "src/mpc_designation_main.awk"]),
    ("Haskell",
     ["./build/mpc_designation"],
     ["./build/mpc_designation"]),
    ("Java",
     ["java", "-cp", "classes", "mpc.MPCDesignationCLI"],
     ["java", "-cp", "classes", "mpc.MPCDesignationCLI"]),
    ("PHP",
     ["php", "src/mpc_designation_cli.php"],
     ["php", "src/mpc_designation_cli.php"]),
    ("Kotlin",
     ["kotlin", "-cp", "build/mpc_designation.jar", "mpc.MainKt"],
     ["kotlin", "-cp", "build/mpc_designation.jar", "mpc.MainKt"]),
    ("Fortran",
     ["./build/mpc_designation_cli"],
     ["./build/mpc_designation_cli"]),
    ("C#",
     ["dotnet", "run", "--configuration", "Release", "--no-build", "--"],
     ["dotnet", "run", "--configuration", "Release", "--no-build", "--"]),
    ("Julia",
     ["julia", "src/mpc_designation_cli.jl"],
     ["julia", "src/mpc_designation_cli.jl"]),
    ("Rust",
     ["./target/release/mpc_designation"],
     ["./target/release/mpc_designation"]),
    ("JavaScript",
     ["node", "src/mpc_designation_cli.js"],
     ["node", "src/mpc_designation_cli.js"]),
    ("TypeScript",
     ["node", "dist/src/mpc_designation_cli.js"],
     ["node", "dist/src/mpc_designation_cli.js"]),
    ("C",
     ["./mpc_designation"],
     ["./mpc_designation"]),
    ("Nim",
     ["./mpc_designation"],
     ["./mpc_designation"]),
    ("Go",
     ["./mpc_designation"],
     ["./mpc_designation"]),
]

# Directory mapping
IMPL_DIRS = {
    "Tcl": "tcl",
    "Ruby": "ruby",
    "Perl": "perl",
    "C++": "cpp",
    "Swift": "swift",
    "Python": "python",
    "AWK": "awk",
    "Haskell": "haskell",
    "Java": "java",
    "PHP": "php",
    "Kotlin": "kotlin",
    "Fortran": "fortran",
    "C#": "csharp",
    "Julia": "julia",
    "Rust": "rust",
    "JavaScript": "js",
    "TypeScript": "typescript",
    "C": "c",
    "Nim": "nim",
    "Go": "go",
}


def run_command(cmd, cwd, input_data=None):
    """Run a command and return stdout."""
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            input=input_data,
            capture_output=True,
            text=True,
            timeout=600
        )
        return result.stdout.strip(), result.returncode
    except subprocess.TimeoutExpired:
        return None, -1
    except Exception as e:
        return str(e), -1


# Implementations that read from stdin instead of command-line args
STDIN_IMPLEMENTATIONS = {"AWK"}


def test_single(name, cmd, cwd, test_input):
    """Test a single implementation with one input."""
    if name in STDIN_IMPLEMENTATIONS:
        # Use stdin
        output, rc = run_command(cmd, cwd, input_data=test_input + "\n")
    else:
        # Use command-line args
        full_cmd = cmd + [test_input]
        output, rc = run_command(full_cmd, cwd)
    return output, rc == 0


def preflight_check():
    """Verify all implementations are ready."""
    print("=" * 60)
    print("PREFLIGHT CHECK")
    print("=" * 60)
    print()

    all_ready = True
    test_unpacked = "1995 XA"
    test_packed = "J95X00A"

    for name, pack_cmd, unpack_cmd in IMPLEMENTATIONS:
        impl_dir = BASE_DIR / IMPL_DIRS[name]

        # Test pack
        packed_out, pack_ok = test_single(name, pack_cmd, impl_dir, test_unpacked)
        # Test unpack
        unpacked_out, unpack_ok = test_single(name, unpack_cmd, impl_dir, test_packed)

        if pack_ok and unpack_ok and packed_out == test_packed and unpacked_out == test_unpacked:
            status = "✅ Ready"
        else:
            status = "❌ FAILED"
            all_ready = False
            if not pack_ok:
                status += f" (pack error)"
            elif packed_out != test_packed:
                status += f" (pack: got '{packed_out}')"
            if not unpack_ok:
                status += f" (unpack error)"
            elif unpacked_out != test_unpacked:
                status += f" (unpack: got '{unpacked_out}')"

        print(f"  {name:12} {status}")

    print()
    return all_ready


def load_test_data():
    """Load unpacked designations from CSV."""
    csv_path = BASE_DIR / "test-data" / "prov_unpack_to_pack.csv"

    if not csv_path.exists():
        # Try to decompress
        gz_path = csv_path.with_suffix(".csv.gz")
        if gz_path.exists():
            print(f"Decompressing {gz_path}...")
            subprocess.run(["gunzip", "-k", str(gz_path)], check=True)

    designations = []
    with open(csv_path, 'r') as f:
        next(f)  # Skip header
        for line in f:
            parts = line.strip().split(',')
            if len(parts) >= 1:
                designations.append(parts[0])

    return designations


def parse_output_line(line):
    """Parse output line, handling 'input -> output' format."""
    if ' -> ' in line:
        # Format: "input -> output"
        return line.split(' -> ', 1)[1]
    return line


# Implementations that don't handle multiple args well (process one at a time)
SINGLE_ARG_IMPLEMENTATIONS = {"Fortran", "TypeScript"}


def process_batch(name, cmd, cwd, inputs, batch_size=500):
    """Process inputs through an implementation in batches."""
    outputs = []
    use_stdin = name in STDIN_IMPLEMENTATIONS
    use_single = name in SINGLE_ARG_IMPLEMENTATIONS

    for i in range(0, len(inputs), batch_size):
        batch = inputs[i:i + batch_size]

        try:
            if use_stdin:
                # Pipe input via stdin (one per line)
                input_data = '\n'.join(batch) + '\n'
                result = subprocess.run(
                    cmd,
                    cwd=cwd,
                    input=input_data,
                    capture_output=True,
                    text=True,
                    timeout=300
                )
                if result.returncode == 0:
                    batch_outputs = [parse_output_line(line) for line in result.stdout.strip().split('\n')]
                    outputs.extend(batch_outputs)
                else:
                    # Try one-by-one
                    for item in batch:
                        out, _ = run_command(cmd, cwd, input_data=item + "\n")
                        outputs.append(parse_output_line(out) if out else "ERROR")

            elif use_single:
                # Process one at a time for implementations that don't handle multiple args
                for item in batch:
                    out, rc = run_command(cmd + [item], cwd)
                    outputs.append(parse_output_line(out) if out else "ERROR")

            else:
                # Pass as command-line arguments
                full_cmd = cmd + batch
                result = subprocess.run(
                    full_cmd,
                    cwd=cwd,
                    capture_output=True,
                    text=True,
                    timeout=300
                )

                if result.returncode != 0:
                    # Try one-by-one for failed batch
                    for item in batch:
                        out, _ = run_command(cmd + [item], cwd)
                        outputs.append(parse_output_line(out) if out else "ERROR")
                else:
                    batch_outputs = [parse_output_line(line) for line in result.stdout.strip().split('\n')]
                    outputs.extend(batch_outputs)

        except Exception as e:
            print(f"    Error in {name}: {e}")
            outputs.extend(["ERROR"] * len(batch))

        # Progress indicator
        if (i + batch_size) % 500000 == 0:
            print(f"    {name}: Processed {min(i + batch_size, len(inputs)):,} entries...")

    return outputs


def run_chain_test(sample_size=0, shuffle=True):
    """Run the full chain test."""
    print("=" * 60)
    print("CHAIN TEST: Slowest → Fastest")
    if shuffle:
        print("(with random shuffling at each step)")
    print("=" * 60)
    print()

    # Load test data
    print("Loading test data...")
    original_inputs = load_test_data()

    if sample_size > 0:
        original_inputs = original_inputs[:sample_size]

    total_entries = len(original_inputs)
    print(f"Loaded {total_entries:,} designations")
    print()

    # Current data starts as unpacked
    current_data = original_inputs.copy()
    is_packed = False  # Start with unpacked data

    overall_start = time.time()
    step_times = []

    # Process through each implementation
    for i, (name, pack_cmd, unpack_cmd) in enumerate(IMPLEMENTATIONS):
        impl_dir = BASE_DIR / IMPL_DIRS[name]

        # Shuffle data before each step (tests robustness)
        if shuffle:
            random.shuffle(current_data)

        # Alternate: if current is unpacked, pack it; if packed, unpack it
        if is_packed:
            operation = "unpack"
            cmd = unpack_cmd
        else:
            operation = "pack"
            cmd = pack_cmd

        elapsed_so_far = time.time() - overall_start
        print(f"[{i+1:2}/{len(IMPLEMENTATIONS)}] {name:12} ({operation:6}) @ {elapsed_so_far:7.1f}s ...")

        step_start = time.time()
        current_data = process_batch(name, cmd, impl_dir, current_data)
        step_time = time.time() - step_start
        step_times.append((name, operation, step_time))

        # Verify count preserved
        if len(current_data) != total_entries:
            print(f"    ⚠️  Entry count changed: {len(current_data):,} (expected {total_entries:,})")

        rate = total_entries / step_time if step_time > 0 else 0
        print(f"    ✓ {step_time:6.1f}s ({rate:>10,.0f} entries/sec)")

        # Toggle state
        is_packed = not is_packed

    overall_time = time.time() - overall_start

    print()
    print("=" * 60)
    print("STEP SUMMARY")
    print("=" * 60)
    print()
    print(f"{'Step':<4} {'Implementation':<12} {'Op':<8} {'Time':>8} {'Rate':>14}")
    print("-" * 50)
    for idx, (name, op, t) in enumerate(step_times):
        rate = total_entries / t if t > 0 else 0
        print(f"{idx+1:<4} {name:<12} {op:<8} {t:>7.1f}s {rate:>13,.0f}/s")
    print("-" * 50)
    print(f"{'Total':<25} {overall_time:>7.1f}s")
    print()

    print("=" * 60)
    print("VERIFICATION")
    print("=" * 60)
    print()

    # After 20 implementations alternating, we should end up with:
    # - Started unpacked, 20 operations = even number = back to unpacked
    # Since we shuffled, sort both lists to compare (values should match)

    original_sorted = sorted(original_inputs)
    final_sorted = sorted(current_data)

    matches = sum(1 for a, b in zip(original_sorted, final_sorted) if a == b)
    mismatches = []

    for i, (orig, final) in enumerate(zip(original_sorted, final_sorted)):
        if orig != final and len(mismatches) < 10:
            mismatches.append((i, orig, final))

    print(f"Total entries:     {total_entries:,}")
    print(f"Final count:       {len(current_data):,}")
    print(f"Matches:           {matches:,}")
    print(f"Mismatches:        {total_entries - matches:,}")
    print(f"Total time:        {overall_time:.1f}s")
    print(f"Average rate:      {total_entries * len(IMPLEMENTATIONS) / overall_time:,.0f} conversions/sec")
    print()

    if mismatches:
        print("First mismatches (sorted order):")
        for idx, orig, final in mismatches[:10]:
            print(f"  [{idx}] '{orig}' → '{final}'")
        print()

    if matches == total_entries and len(current_data) == total_entries:
        print("✅ CHAIN TEST PASSED: All entries preserved through 20 implementations!")
        return 0
    else:
        print("❌ CHAIN TEST FAILED: Data integrity issue detected")
        return 1


def main():
    import argparse
    parser = argparse.ArgumentParser(description='Chain test for MPC designation converters')
    parser.add_argument('--sample', type=int, default=0,
                        help='Use only first N entries (0 = all)')
    parser.add_argument('--skip-preflight', action='store_true',
                        help='Skip preflight check')
    parser.add_argument('--no-shuffle', action='store_true',
                        help='Disable random shuffling between steps')
    parser.add_argument('--fast', action='store_true',
                        help='Skip slow implementations (Java, Kotlin, C#, Julia)')
    args = parser.parse_args()

    # Filter implementations if --fast
    global IMPLEMENTATIONS
    if args.fast:
        IMPLEMENTATIONS = [(n, p, u) for n, p, u in IMPLEMENTATIONS if n not in SLOW_IMPLEMENTATIONS]

    print()
    print("MPC Designation Chain Test")
    print(f"Connecting {len(IMPLEMENTATIONS)} implementations from slowest to fastest")
    if args.sample > 0:
        print(f"(Using sample of {args.sample:,} entries)")
    if not args.no_shuffle:
        print("(with random shuffling between steps)")
    if args.fast:
        print("(skipping slow implementations: Java, Kotlin, C#, Julia)")
    print()

    # Preflight
    if not args.skip_preflight:
        if not preflight_check():
            print("❌ Preflight check failed. Please build all implementations first.")
            print("   Run: make all")
            return 1
        print("✅ All implementations ready")
        print()

    # Run chain test
    return run_chain_test(sample_size=args.sample, shuffle=not args.no_shuffle)


if __name__ == "__main__":
    sys.exit(main())
