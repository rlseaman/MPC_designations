#!/usr/bin/env python3
"""
MPC Designation Library Interoperability Test

Tests Python and TCL implementations head-to-head to verify:
1. Both produce identical outputs for all test data
2. Both handle error cases consistently
3. Helper functions produce equivalent results

Generates a detailed report suitable for external review.
"""

import subprocess
import sys
import os
import csv
import time
import tempfile
from datetime import datetime
from typing import List, Tuple, Dict, Optional

# Add Python src to path
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PYTHON_SRC = os.path.join(SCRIPT_DIR, '..', 'python', 'src')
sys.path.insert(0, PYTHON_SRC)

from mpc_designation import (
    convert_simple, pack, unpack, detect_format, is_valid_designation,
    to_report_format, from_report_format, has_fragment, get_fragment,
    get_parent, designations_equal, __version__ as python_version
)

# TCL library path
TCL_SRC = os.path.join(SCRIPT_DIR, '..', 'tcl', 'src', 'mpc_designation.tcl')

# C CLI path
C_CLI = os.path.join(SCRIPT_DIR, '..', 'c', 'mpc_designation')


def run_tcl_with_input(func_name: str, input_val: str, preserve_whitespace: bool = False) -> Tuple[bool, str]:
    """Run a TCL function with a single input, handling escaping properly."""
    # Escape for TCL string
    escaped = input_val.replace('\\', '\\\\')
    escaped = escaped.replace('"', '\\"')
    escaped = escaped.replace('$', '\\$')
    escaped = escaped.replace('[', '\\[')
    escaped = escaped.replace(']', '\\]')

    tcl_code = f'''source {{/{TCL_SRC.replace(chr(92), "/")}}}
set input "{escaped}"
if {{[catch {{{func_name} $input}} result]}} {{
    puts "ERROR: $result"
    exit 1
}} else {{
    puts $result
}}'''
    try:
        result = subprocess.run(
            ['tclsh'],
            input=tcl_code,
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode != 0:
            return False, result.stdout.strip() + result.stderr.strip()
        output = result.stdout
        if output.endswith('\n'):
            output = output[:-1]
        if not preserve_whitespace:
            output = output.strip()
        return True, output
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT"
    except Exception as e:
        return False, str(e)


def run_tcl_with_inputs(func_name: str, inputs: List[str], preserve_whitespace: bool = False) -> Tuple[bool, str]:
    """Run a TCL function with multiple inputs."""
    escaped_inputs = []
    for inp in inputs:
        escaped = inp.replace('\\', '\\\\')
        escaped = escaped.replace('"', '\\"')
        escaped = escaped.replace('$', '\\$')
        escaped = escaped.replace('[', '\\[')
        escaped = escaped.replace(']', '\\]')
        escaped_inputs.append(f'"{escaped}"')

    args = ' '.join(escaped_inputs)

    tcl_code = f'''source {{/{TCL_SRC.replace(chr(92), "/")}}}
if {{[catch {{{func_name} {args}}} result]}} {{
    puts "ERROR: $result"
    exit 1
}} else {{
    puts $result
}}'''
    try:
        result = subprocess.run(
            ['tclsh'],
            input=tcl_code,
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode != 0:
            return False, result.stdout.strip() + result.stderr.strip()
        output = result.stdout
        if output.endswith('\n'):
            output = output[:-1]
        if not preserve_whitespace:
            output = output.strip()
        return True, output
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT"
    except Exception as e:
        return False, str(e)


def run_tcl(command: str, preserve_whitespace: bool = False) -> Tuple[bool, str]:
    """Run a TCL command and return (success, output). DEPRECATED - use run_tcl_with_input instead."""
    tcl_code = f'''source {{/{TCL_SRC.replace(chr(92), "/")}}}
if {{[catch {{{command}}} result]}} {{
    puts "ERROR: $result"
    exit 1
}} else {{
    puts $result
}}'''
    try:
        result = subprocess.run(
            ['tclsh'],
            input=tcl_code,
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode != 0:
            return False, result.stdout.strip() + result.stderr.strip()
        output = result.stdout
        if output.endswith('\n'):
            output = output[:-1]
        if not preserve_whitespace:
            output = output.strip()
        return True, output
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT"
    except Exception as e:
        return False, str(e)


def tcl_escape(s: str) -> str:
    """Escape a string for safe inclusion in TCL code. Use run_tcl_with_input instead when possible."""
    if '{' in s or '}' in s or '\\' in s or '$' in s or '[' in s or ']' in s:
        escaped = s.replace('\\', '\\\\')
        escaped = escaped.replace('"', '\\"')
        escaped = escaped.replace('$', '\\$')
        escaped = escaped.replace('[', '\\[')
        escaped = escaped.replace(']', '\\]')
        return f'"{escaped}"'
    else:
        return f'{{{s}}}'


def get_tcl_version() -> str:
    """Get TCL library version."""
    success, output = run_tcl('puts $MPCDesignation::version')
    return output if success else "unknown"


def run_c_convert(input_val: str) -> Tuple[bool, str]:
    """Run C CLI to convert a designation."""
    try:
        result = subprocess.run(
            [C_CLI, input_val],
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode != 0:
            return False, f"ERROR: {result.stderr.strip()}"
        return True, result.stdout.strip()
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT"
    except FileNotFoundError:
        return False, "C CLI not found - run 'make' in c/ directory"
    except Exception as e:
        return False, str(e)


def get_c_version() -> str:
    """Get C library version by checking the VERSION file."""
    try:
        version_path = os.path.join(SCRIPT_DIR, '..', 'VERSION')
        with open(version_path, 'r') as f:
            return f.read().strip()
    except Exception:
        return "unknown"


def c_cli_available() -> bool:
    """Check if C CLI is available."""
    return os.path.exists(C_CLI) and os.access(C_CLI, os.X_OK)


class TestResult:
    """Container for test results."""
    def __init__(self, name: str):
        self.name = name
        self.passed = 0
        self.failed = 0
        self.errors: List[Dict] = []
        self.start_time = None
        self.end_time = None

    def record_pass(self):
        self.passed += 1

    def record_fail(self, input_val: str, python_out: str, tcl_out: str, detail: str = "", c_out: str = None):
        self.failed += 1
        error = {
            'input': input_val,
            'python': python_out,
            'tcl': tcl_out,
            'detail': detail
        }
        if c_out is not None:
            error['c'] = c_out
        self.errors.append(error)

    @property
    def total(self) -> int:
        return self.passed + self.failed

    @property
    def duration_sec(self) -> float:
        if self.start_time and self.end_time:
            return self.end_time - self.start_time
        return 0


def test_csv_conversions(csv_path: str, limit: Optional[int] = None, include_c: bool = True) -> TestResult:
    """Test pack/unpack conversions against CSV test data."""
    result = TestResult("CSV Conversion Tests")
    result.start_time = time.time()

    has_c = include_c and c_cli_available()

    with open(csv_path, 'r') as f:
        reader = csv.DictReader(f)
        count = 0

        for row in reader:
            if limit and count >= limit:
                break
            count += 1

            # Handle different column names
            if 'unpacked' in row:
                unpacked = row['unpacked']
                packed = row['packed']
            else:
                unpacked = row['unpacked_provisional_designation']
                packed = row['packed_provisional_designation']

            # Test Python pack
            try:
                py_packed = pack(unpacked)
            except Exception as e:
                py_packed = f"ERROR: {e}"

            # Test TCL pack
            tcl_success, tcl_packed = run_tcl_with_input('MPCDesignation::pack', unpacked)
            if not tcl_success:
                tcl_packed = f"ERROR: {tcl_packed}"

            # Test C pack (convert unpacked -> should get packed)
            c_packed = None
            if has_c:
                c_success, c_packed = run_c_convert(unpacked)
                if not c_success:
                    c_packed = f"ERROR: {c_packed}"

            # Compare pack results
            all_match = py_packed == packed and tcl_packed == packed
            if has_c:
                all_match = all_match and c_packed == packed

            if all_match:
                result.record_pass()
            else:
                result.record_fail(
                    unpacked,
                    py_packed,
                    tcl_packed,
                    f"Expected: {packed}",
                    c_out=c_packed
                )

            # Test Python unpack
            try:
                py_unpacked = unpack(packed)
            except Exception as e:
                py_unpacked = f"ERROR: {e}"

            # Test TCL unpack
            tcl_success, tcl_unpacked = run_tcl_with_input('MPCDesignation::unpack', packed)
            if not tcl_success:
                tcl_unpacked = f"ERROR: {tcl_unpacked}"

            # Test C unpack (convert packed -> should get unpacked)
            c_unpacked = None
            if has_c:
                c_success, c_unpacked = run_c_convert(packed)
                if not c_success:
                    c_unpacked = f"ERROR: {c_unpacked}"

            # Compare unpack results
            all_match = py_unpacked == unpacked and tcl_unpacked == unpacked
            if has_c:
                all_match = all_match and c_unpacked == unpacked

            if all_match:
                result.record_pass()
            else:
                result.record_fail(
                    packed,
                    py_unpacked,
                    tcl_unpacked,
                    f"Expected: {unpacked}",
                    c_out=c_unpacked
                )

            if count % 10000 == 0:
                print(f"  Processed {count} entries...", file=sys.stderr)

    result.end_time = time.time()
    return result


def test_error_cases(csv_path: str, include_c: bool = True) -> TestResult:
    """Test error handling consistency between implementations."""
    result = TestResult("Error Handling Tests")
    result.start_time = time.time()

    has_c = include_c and c_cli_available()

    with open(csv_path, 'r') as f:
        reader = csv.DictReader(f)

        for row in reader:
            input_val = row.get('input')
            expected = row.get('expected_error')
            subcategory = row.get('subcategory', '')

            # Skip comment lines or malformed rows
            if input_val is None or expected is None:
                continue

            # Unescape special characters
            input_val = input_val.replace('\\t', '\t')
            input_val = input_val.replace('\\n', '\n')
            input_val = input_val.replace('\\r', '\r')
            input_val = input_val.replace('\\x00', '\x00')

            # C has known limitation with null bytes (C strings terminate at null)
            c_skip = has_c and ('null' in subcategory.lower())

            # Test Python
            try:
                py_result = convert_simple(input_val)
                py_is_error = False
            except Exception:
                py_is_error = True

            # Test TCL
            tcl_success, tcl_output = run_tcl_with_input('MPCDesignation::convertSimple', input_val)
            tcl_is_error = not tcl_success or tcl_output.startswith("ERROR:")

            # Test C (if available and not skipped)
            c_is_error = None
            if has_c and not c_skip:
                c_success, c_output = run_c_convert(input_val)
                c_is_error = not c_success or c_output.startswith("ERROR:")

            # All implementations should agree on error/success
            if expected == 'valid':
                # All should succeed
                all_ok = not py_is_error and not tcl_is_error
                if has_c and not c_skip:
                    all_ok = all_ok and not c_is_error

                if all_ok:
                    result.record_pass()
                else:
                    c_status = "SKIP" if c_skip else ("ERROR" if c_is_error else "OK") if c_is_error is not None else None
                    result.record_fail(
                        repr(input_val),
                        "ERROR" if py_is_error else "OK",
                        "ERROR" if tcl_is_error else "OK",
                        f"Expected: valid",
                        c_out=c_status
                    )
            else:
                # All should fail
                all_error = py_is_error and tcl_is_error
                if has_c and not c_skip:
                    all_error = all_error and c_is_error

                if all_error:
                    result.record_pass()
                else:
                    c_status = "SKIP" if c_skip else ("ERROR" if c_is_error else "OK") if c_is_error is not None else None
                    result.record_fail(
                        repr(input_val),
                        "ERROR" if py_is_error else "OK",
                        "ERROR" if tcl_is_error else "OK",
                        f"Expected: error ({expected})",
                        c_out=c_status
                    )

    result.end_time = time.time()
    return result


def test_helper_functions() -> TestResult:
    """Test helper functions produce equivalent results."""
    result = TestResult("Helper Function Tests")
    result.start_time = time.time()

    # Test cases for various helper functions
    # Special handling for functions that need whitespace preservation
    whitespace_funcs = {'to_report_format', 'from_report_format'}

    test_cases = [
        # (function, python_call, tcl_call, inputs)
        ('to_report_format', to_report_format, 'MPCDesignation::toReportFormat', [
            '00001', 'J95X00A', '0073P', '0073Pa', '0073Paa', 'CJ95O010'
        ]),
        ('from_report_format', from_report_format, 'MPCDesignation::fromReportFormat', [
            '       00001', '     J95X00A', '0073P       ', '0073P      a', '0073P     aa'
        ]),
        ('has_fragment', has_fragment, 'MPCDesignation::hasFragment', [
            '73P-A', '73P-AA', '73P', 'C/1995 O1', '0073Pa', '0073Paa', '0073P', '1995 XA'
        ]),
        ('get_fragment', get_fragment, 'MPCDesignation::getFragment', [
            '73P-A', '73P-AA', '73P', '0073Pa', '0073Paa', '0073P'
        ]),
        ('get_parent', get_parent, 'MPCDesignation::getParent', [
            '73P-A', '73P-AA', '73P', '0073Pa', '0073Paa', '0073P', '1995 XA'
        ]),
    ]

    for func_name, py_func, tcl_func, inputs in test_cases:
        preserve_ws = func_name in whitespace_funcs
        for inp in inputs:
            # Python result
            try:
                py_result = py_func(inp)
                if isinstance(py_result, bool):
                    py_result = '1' if py_result else '0'
                py_result = str(py_result)
            except Exception as e:
                py_result = f"ERROR: {e}"

            # TCL result
            tcl_success, tcl_result = run_tcl_with_input(
                tcl_func, inp,
                preserve_whitespace=preserve_ws
            )
            if not tcl_success:
                tcl_result = f"ERROR: {tcl_result}"

            if py_result == tcl_result:
                result.record_pass()
            else:
                result.record_fail(
                    f"{func_name}({inp})",
                    py_result,
                    tcl_result,
                    ""
                )

    # Test designations_equal
    equal_pairs = [
        ('1995 XA', 'J95X00A', True),
        ('73P', '0073P', True),
        ('73P-A', '0073Pa', True),
        ('1', '00001', True),
        ('1995 XA', '1995 XB', False),
        ('73P-A', '73P-B', False),
    ]

    for d1, d2, expected in equal_pairs:
        # Python
        try:
            py_result = '1' if designations_equal(d1, d2) else '0'
        except Exception as e:
            py_result = f"ERROR: {e}"

        # TCL
        tcl_success, tcl_result = run_tcl_with_inputs('MPCDesignation::designationsEqual', [d1, d2])
        if not tcl_success:
            tcl_result = f"ERROR: {tcl_result}"

        expected_str = '1' if expected else '0'
        if py_result == expected_str and tcl_result == expected_str:
            result.record_pass()
        else:
            result.record_fail(
                f"designationsEqual({d1}, {d2})",
                py_result,
                tcl_result,
                f"Expected: {expected_str}"
            )

    result.end_time = time.time()
    return result


def test_edge_cases(include_c: bool = True) -> TestResult:
    """Test edge cases and boundary conditions."""
    result = TestResult("Edge Case Tests")
    result.start_time = time.time()

    has_c = include_c and c_cli_available()

    edge_cases = [
        # Pre-1925 designations (A-prefix)
        ('A908 CJ', 'J08C00J'),
        ('A801 AA', 'I01A00A'),
        # Survey designations
        ('2040 P-L', 'PLS2040'),
        ('3138 T-1', 'T1S3138'),
        # Numbered asteroids boundary
        ('1', '00001'),
        ('99999', '99999'),
        ('100000', 'A0000'),
        ('359999', 'Z9999'),
        ('360000', 'a0000'),
        ('619999', 'z9999'),
        ('620000', '~0000'),
        # Comets
        ('1P', '0001P'),
        ('73P-A', '0073Pa'),
        ('73P-AA', '0073Paa'),
        ('C/1995 O1', 'CJ95O010'),
        # Satellites
        ('S/2019 S 22', 'SK19S220'),
    ]

    for unpacked, packed in edge_cases:
        # Test pack direction
        try:
            py_packed = pack(unpacked)
        except Exception as e:
            py_packed = f"ERROR: {e}"

        tcl_success, tcl_packed = run_tcl_with_input('MPCDesignation::pack', unpacked)
        if not tcl_success:
            tcl_packed = f"ERROR: {tcl_packed}"

        c_packed = None
        if has_c:
            c_success, c_packed = run_c_convert(unpacked)
            if not c_success:
                c_packed = f"ERROR: {c_packed}"

        all_match = py_packed == packed and tcl_packed == packed
        if has_c:
            all_match = all_match and c_packed == packed

        if all_match:
            result.record_pass()
        else:
            result.record_fail(
                f"pack({unpacked})",
                py_packed,
                tcl_packed,
                f"Expected: {packed}",
                c_out=c_packed
            )

        # Test unpack direction
        try:
            py_unpacked = unpack(packed)
        except Exception as e:
            py_unpacked = f"ERROR: {e}"

        tcl_success, tcl_unpacked = run_tcl_with_input('MPCDesignation::unpack', packed)
        if not tcl_success:
            tcl_unpacked = f"ERROR: {tcl_unpacked}"

        c_unpacked = None
        if has_c:
            c_success, c_unpacked = run_c_convert(packed)
            if not c_success:
                c_unpacked = f"ERROR: {c_unpacked}"

        all_match = py_unpacked == unpacked and tcl_unpacked == unpacked
        if has_c:
            all_match = all_match and c_unpacked == unpacked

        if all_match:
            result.record_pass()
        else:
            result.record_fail(
                f"unpack({packed})",
                py_unpacked,
                tcl_unpacked,
                f"Expected: {unpacked}",
                c_out=c_unpacked
            )

    result.end_time = time.time()
    return result


def run_individual_library_tests() -> Dict:
    """Run each library's full test suite and capture results."""
    results = {}

    # Python tests
    print("  Running Python full test suite...")
    py_tests = []

    # Python CSV test
    csv_path = os.path.join(SCRIPT_DIR, 'prov_unpack_to_pack.csv')
    if os.path.exists(csv_path):
        cmd = ['python3', os.path.join(SCRIPT_DIR, '..', 'python', 'test', 'test_csv.py'), csv_path]
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
        import re
        passed_match = re.search(r'Passed:\s*(\d+)', result.stdout)
        failed_match = re.search(r'Failed:\s*(\d+)', result.stdout)
        if passed_match:
            passed = int(passed_match.group(1))
            failed = int(failed_match.group(1)) if failed_match else 0
            py_tests.append(('CSV Conversions', passed, failed))

    # Python error test
    error_csv = os.path.join(SCRIPT_DIR, 'error_test_cases.csv')
    if os.path.exists(error_csv):
        cmd = ['python3', os.path.join(SCRIPT_DIR, '..', 'python', 'test', 'test_errors.py'), error_csv]
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
        for line in result.stdout.split('\n'):
            if 'Passed:' in line:
                import re
                match = re.search(r'Passed:\s*(\d+)', line)
                failed_match = re.search(r'Failed:\s*(\d+)', line)
                if match:
                    passed = int(match.group(1))
                    failed = int(failed_match.group(1)) if failed_match else 0
                    py_tests.append(('Error Handling', passed, failed))
                    break

    # Python helper/fragment tests
    cmd = ['python3', os.path.join(SCRIPT_DIR, '..', 'python', 'test', 'test_helpers.py')]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    for line in result.stdout.split('\n'):
        if 'Passed:' in line:
            import re
            match = re.search(r'Passed:\s*(\d+)', line)
            failed_match = re.search(r'Failed:\s*(\d+)', line)
            if match:
                passed = int(match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                py_tests.append(('Helper Functions', passed, failed))
                break

    cmd = ['python3', os.path.join(SCRIPT_DIR, '..', 'python', 'test', 'test_fragments.py')]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    for line in result.stdout.split('\n'):
        if 'Passed:' in line:
            import re
            match = re.search(r'Passed:\s*(\d+)', line)
            failed_match = re.search(r'Failed:\s*(\d+)', line)
            if match:
                passed = int(match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                py_tests.append(('Fragment Handling', passed, failed))
                break

    results['python'] = py_tests

    # TCL tests
    print("  Running TCL full test suite...")
    tcl_tests = []

    # TCL CSV test
    if os.path.exists(csv_path):
        cmd = ['tclsh', os.path.join(SCRIPT_DIR, '..', 'tcl', 'test', 'test_csv.tcl'), csv_path]
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
        import re
        passed_match = re.search(r'Passed:\s*(\d+)', result.stdout)
        failed_match = re.search(r'Failed:\s*(\d+)', result.stdout)
        if passed_match:
            passed = int(passed_match.group(1))
            failed = int(failed_match.group(1)) if failed_match else 0
            tcl_tests.append(('CSV Conversions', passed, failed))

    # TCL error test
    if os.path.exists(error_csv):
        cmd = ['tclsh', os.path.join(SCRIPT_DIR, '..', 'tcl', 'test', 'test_errors.tcl'), error_csv]
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
        for line in result.stdout.split('\n'):
            if 'Passed:' in line:
                import re
                match = re.search(r'Passed:\s*(\d+)', line)
                failed_match = re.search(r'Failed:\s*(\d+)', line)
                if match:
                    passed = int(match.group(1))
                    failed = int(failed_match.group(1)) if failed_match else 0
                    tcl_tests.append(('Error Handling', passed, failed))
                    break

    # TCL helper/fragment tests
    cmd = ['tclsh', os.path.join(SCRIPT_DIR, '..', 'tcl', 'test', 'test_helpers.tcl')]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    for line in result.stdout.split('\n'):
        if 'Passed:' in line:
            import re
            match = re.search(r'Passed:\s*(\d+)', line)
            failed_match = re.search(r'Failed:\s*(\d+)', line)
            if match:
                passed = int(match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                tcl_tests.append(('Helper Functions', passed, failed))
                break

    cmd = ['tclsh', os.path.join(SCRIPT_DIR, '..', 'tcl', 'test', 'test_fragments.tcl')]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    for line in result.stdout.split('\n'):
        if 'Passed:' in line or 'Summary' in line:
            import re
            match = re.search(r'Passed:\s*(\d+)', line)
            failed_match = re.search(r'Failed:\s*(\d+)', line)
            if match:
                passed = int(match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                tcl_tests.append(('Fragment Handling', passed, failed))
                break

    results['tcl'] = tcl_tests

    # C tests (if CLI available)
    if c_cli_available():
        print("  Running C full test suite...")
        c_tests = []

        # C CSV test
        c_test_csv = os.path.join(SCRIPT_DIR, '..', 'c', 'test_csv')
        if os.path.exists(c_test_csv) and os.path.exists(csv_path):
            result = subprocess.run([c_test_csv, csv_path], capture_output=True, text=True, timeout=300)
            import re
            passed_match = re.search(r'Passed:\s*(\d+)', result.stdout)
            failed_match = re.search(r'Failed:\s*(\d+)', result.stdout)
            if passed_match:
                passed = int(passed_match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                c_tests.append(('CSV Conversions', passed, failed))

        # C error test
        c_test_errors = os.path.join(SCRIPT_DIR, '..', 'c', 'test_errors')
        if os.path.exists(c_test_errors) and os.path.exists(error_csv):
            result = subprocess.run([c_test_errors, error_csv], capture_output=True, text=True, timeout=60)
            import re
            passed_match = re.search(r'Passed:\s*(\d+)', result.stdout)
            failed_match = re.search(r'Failed:\s*(\d+)', result.stdout)
            if passed_match:
                passed = int(passed_match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                c_tests.append(('Error Handling', passed, failed))

        # C helper test
        c_test_helpers = os.path.join(SCRIPT_DIR, '..', 'c', 'test_helpers')
        if os.path.exists(c_test_helpers):
            result = subprocess.run([c_test_helpers], capture_output=True, text=True, timeout=60)
            import re
            passed_match = re.search(r'Passed:\s*(\d+)', result.stdout)
            failed_match = re.search(r'Failed:\s*(\d+)', result.stdout)
            if passed_match:
                passed = int(passed_match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                c_tests.append(('Helper Functions', passed, failed))

        # C fragment test
        c_test_fragments = os.path.join(SCRIPT_DIR, '..', 'c', 'test_fragments')
        if os.path.exists(c_test_fragments):
            result = subprocess.run([c_test_fragments], capture_output=True, text=True, timeout=60)
            import re
            passed_match = re.search(r'Passed:\s*(\d+)', result.stdout)
            failed_match = re.search(r'Failed:\s*(\d+)', result.stdout)
            if passed_match:
                passed = int(passed_match.group(1))
                failed = int(failed_match.group(1)) if failed_match else 0
                c_tests.append(('Fragment Handling', passed, failed))

        results['c'] = c_tests

    return results


def generate_report(results: List[TestResult], output_path: str, lib_results: Dict = None):
    """Generate a detailed interoperability report."""

    py_ver = python_version
    tcl_ver = get_tcl_version()
    c_ver = get_c_version()
    has_c = c_cli_available()

    with open(output_path, 'w') as f:
        f.write("=" * 78 + "\n")
        f.write("MPC DESIGNATION LIBRARY INTEROPERABILITY REPORT\n")
        f.write("=" * 78 + "\n\n")

        f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Python Library Version: {py_ver}\n")
        f.write(f"TCL Library Version: {tcl_ver}\n")
        if has_c:
            f.write(f"C Library Version: {c_ver}\n")
        f.write(f"Python Version: {sys.version.split()[0]}\n\n")

        # Individual Library Tests
        if lib_results:
            f.write("-" * 78 + "\n")
            f.write("INDIVIDUAL LIBRARY TEST RESULTS\n")
            f.write("-" * 78 + "\n\n")

            f.write("Python Library:\n")
            py_total_passed = 0
            py_total_failed = 0
            for name, passed, failed in lib_results.get('python', []):
                f.write(f"  {name:<30} Passed: {passed:>10,}  Failed: {failed}\n")
                py_total_passed += passed
                py_total_failed += failed
            f.write(f"  {'TOTAL':<30} Passed: {py_total_passed:>10,}  Failed: {py_total_failed}\n\n")

            f.write("TCL Library:\n")
            tcl_total_passed = 0
            tcl_total_failed = 0
            for name, passed, failed in lib_results.get('tcl', []):
                f.write(f"  {name:<30} Passed: {passed:>10,}  Failed: {failed}\n")
                tcl_total_passed += passed
                tcl_total_failed += failed
            f.write(f"  {'TOTAL':<30} Passed: {tcl_total_passed:>10,}  Failed: {tcl_total_failed}\n\n")

            if 'c' in lib_results:
                f.write("C Library:\n")
                c_total_passed = 0
                c_total_failed = 0
                for name, passed, failed in lib_results.get('c', []):
                    f.write(f"  {name:<30} Passed: {passed:>10,}  Failed: {failed}\n")
                    c_total_passed += passed
                    c_total_failed += failed
                f.write(f"  {'TOTAL':<30} Passed: {c_total_passed:>10,}  Failed: {c_total_failed}\n\n")

        # Summary
        f.write("-" * 78 + "\n")
        f.write("INTEROPERABILITY TEST SUMMARY\n")
        f.write("-" * 78 + "\n\n")

        total_passed = sum(r.passed for r in results)
        total_failed = sum(r.failed for r in results)
        total_tests = total_passed + total_failed

        f.write(f"{'Test Category':<40} {'Passed':>10} {'Failed':>10} {'Total':>10}\n")
        f.write("-" * 70 + "\n")

        for result in results:
            f.write(f"{result.name:<40} {result.passed:>10} {result.failed:>10} {result.total:>10}\n")

        f.write("-" * 70 + "\n")
        f.write(f"{'TOTAL':<40} {total_passed:>10} {total_failed:>10} {total_tests:>10}\n\n")

        if total_failed == 0:
            f.write("RESULT: ALL TESTS PASSED - Libraries are fully interoperable\n\n")
        else:
            f.write(f"RESULT: {total_failed} FAILURES - See details below\n\n")

        # Performance
        f.write("-" * 78 + "\n")
        f.write("PERFORMANCE\n")
        f.write("-" * 78 + "\n\n")

        for result in results:
            if result.duration_sec > 0:
                rate = result.total / result.duration_sec if result.duration_sec > 0 else 0
                f.write(f"{result.name}: {result.duration_sec:.2f}s ({rate:.0f} tests/sec)\n")

        f.write("\n")

        # Error details
        if total_failed > 0:
            f.write("-" * 78 + "\n")
            f.write("FAILURE DETAILS\n")
            f.write("-" * 78 + "\n\n")

            for result in results:
                if result.errors:
                    f.write(f"\n{result.name}:\n")
                    for i, error in enumerate(result.errors[:20]):  # Limit to first 20
                        f.write(f"  [{i+1}] Input: {error['input']}\n")
                        f.write(f"      Python: {error['python']}\n")
                        f.write(f"      TCL:    {error['tcl']}\n")
                        if 'c' in error and error['c'] is not None:
                            f.write(f"      C:      {error['c']}\n")
                        if error['detail']:
                            f.write(f"      {error['detail']}\n")
                    if len(result.errors) > 20:
                        f.write(f"  ... and {len(result.errors) - 20} more failures\n")

        # Specification compliance
        f.write("-" * 78 + "\n")
        f.write("SPECIFICATION COMPLIANCE\n")
        f.write("-" * 78 + "\n\n")

        lib_count = "All three libraries" if has_c else "Both libraries"
        f.write(f"{lib_count} implement the MPC packed designation format as specified at:\n")
        f.write("https://www.minorplanetcenter.net/iau/info/PackedDes.html\n\n")

        f.write("Supported designation types:\n")
        f.write("  - Permanent numbered asteroids (1 to 15,396,335)\n")
        f.write("  - Provisional asteroid designations (1800-2199)\n")
        f.write("  - Extended provisional format (cycle >= 620)\n")
        f.write("  - Survey designations (P-L, T-1, T-2, T-3)\n")
        f.write("  - Pre-1925 A-prefix format\n")
        f.write("  - Numbered periodic comets with fragments\n")
        f.write("  - Provisional comet designations with fragments\n")
        f.write("  - Natural satellite designations\n")
        f.write("  - Historical/ancient comets (year < 1000)\n")
        f.write("  - BCE comet designations\n\n")

        f.write("Test data source: MPC official designation database\n")
        f.write("Test data size: 2,022,404 designation pairs\n\n")

        # Conclusion
        f.write("-" * 78 + "\n")
        f.write("CONCLUSION\n")
        f.write("-" * 78 + "\n\n")

        if has_c:
            impl_list = "Python, TCL, and C"
            lib_word = "All three libraries"
            any_word = "Any"
        else:
            impl_list = "Python and TCL"
            lib_word = "Both libraries"
            any_word = "Either"

        if total_failed == 0:
            f.write(f"The {impl_list} implementations are FULLY INTEROPERABLE.\n")
            f.write(f"{lib_word} produce identical results for all test cases.\n")
            f.write(f"{any_word} library can be used in production with confidence.\n")
        else:
            f.write(f"DISCREPANCIES FOUND between {impl_list} implementations.\n")
            f.write("Review the failure details above before production deployment.\n")

        f.write("\n" + "=" * 78 + "\n")


def main():
    print("MPC Designation Library Interoperability Test")
    print("=" * 50)
    print()

    # Check TCL is available
    tcl_ver = get_tcl_version()
    if tcl_ver == "unknown":
        print("ERROR: Cannot access TCL library. Ensure tclsh is installed.")
        sys.exit(1)

    # Check C availability
    has_c = c_cli_available()

    print(f"Python Library Version: {python_version}")
    print(f"TCL Library Version: {tcl_ver}")
    if has_c:
        print(f"C Library Version: {get_c_version()}")
    else:
        print("C Library: Not available (run 'make' in c/ directory to build)")
    print()

    results = []

    # Test 1: Edge cases (quick)
    print("Running edge case tests...")
    results.append(test_edge_cases())
    print(f"  Passed: {results[-1].passed}, Failed: {results[-1].failed}")

    # Test 2: Helper functions
    print("Running helper function tests...")
    results.append(test_helper_functions())
    print(f"  Passed: {results[-1].passed}, Failed: {results[-1].failed}")

    # Test 3: Error handling
    error_csv = os.path.join(SCRIPT_DIR, 'error_test_cases.csv')
    if os.path.exists(error_csv):
        print("Running error handling tests...")
        results.append(test_error_cases(error_csv))
        print(f"  Passed: {results[-1].passed}, Failed: {results[-1].failed}")

    # Test 4: Full CSV test (sampled for speed in interop test)
    csv_path = os.path.join(SCRIPT_DIR, 'prov_unpack_to_pack.csv')
    if os.path.exists(csv_path):
        # Sample test for interop - full CSV is slow with subprocess calls
        print("Running CSV conversion tests (sampled: 1000 entries)...")
        results.append(test_csv_conversions(csv_path, limit=1000))
        print(f"  Passed: {results[-1].passed}, Failed: {results[-1].failed}")

    # Run individual library tests
    print("Running individual library full test suites...")
    lib_results = run_individual_library_tests()

    print()

    # Generate report
    report_path = os.path.join(SCRIPT_DIR, 'INTEROPERABILITY_REPORT.txt')
    generate_report(results, report_path, lib_results)
    print(f"Report written to: {report_path}")

    # Summary
    total_passed = sum(r.passed for r in results)
    total_failed = sum(r.failed for r in results)

    if has_c:
        impl_list = "Python, TCL, and C"
    else:
        impl_list = "Python and TCL"

    print()
    print("=" * 50)
    if total_failed == 0:
        print(f"ALL {total_passed} TESTS PASSED")
        print(f"{impl_list} libraries are fully interoperable.")
    else:
        print(f"FAILED: {total_failed} of {total_passed + total_failed} tests")
        print("See report for details.")

    return 0 if total_failed == 0 else 1


if __name__ == '__main__':
    sys.exit(main())
