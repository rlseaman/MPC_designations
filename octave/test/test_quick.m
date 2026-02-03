#!/usr/bin/env octave
% test_quick.m - Quick validation tests for MPC designation converter

1;

% Source the library
script_dir = fileparts(mfilename('fullpath'));
if isempty(script_dir)
  script_dir = '.';
end
source(fullfile(script_dir, '..', 'src', 'mpc_designation.m'));

global tests_passed tests_failed;
tests_passed = 0;
tests_failed = 0;

function result = run_test(name, expr)
  global tests_passed tests_failed;
  try
    if expr
      tests_passed = tests_passed + 1;
      fprintf('  PASS: %s\n', name);
      result = true;
    else
      tests_failed = tests_failed + 1;
      fprintf('  FAIL: %s\n', name);
      result = false;
    end
  catch err
    tests_failed = tests_failed + 1;
    fprintf('  FAIL: %s - %s\n', name, err.message);
    result = false;
  end
end

fprintf('=== Quick Validation Tests ===\n\n');

% Permanent asteroid tests
fprintf('Permanent asteroids:\n');
run_test('pack 1 -> 00001', strcmp(mpc_pack_permanent(1), '00001'));
run_test('pack 99999 -> 99999', strcmp(mpc_pack_permanent(99999), '99999'));
run_test('pack 100000 -> A0000', strcmp(mpc_pack_permanent(100000), 'A0000'));
run_test('pack 100001 -> A0001', strcmp(mpc_pack_permanent(100001), 'A0001'));
run_test('pack 360000 -> a0000', strcmp(mpc_pack_permanent(360000), 'a0000'));
run_test('pack 620000 -> ~0000', strcmp(mpc_pack_permanent(620000), '~0000'));
run_test('unpack 00001 -> 1', mpc_unpack_permanent('00001') == 1);
run_test('unpack A0001 -> 100001', mpc_unpack_permanent('A0001') == 100001);
run_test('unpack ~0000 -> 620000', mpc_unpack_permanent('~0000') == 620000);
fprintf('\n');

% Provisional asteroid tests
fprintf('Provisional asteroids:\n');
run_test('pack 1995 XA -> J95X00A', strcmp(mpc_pack_provisional('1995 XA'), 'J95X00A'));
run_test('pack 2024 AB1 -> K24A01B', strcmp(mpc_pack_provisional('2024 AB1'), 'K24A01B'));
run_test('pack 2040 P-L -> PLS2040', strcmp(mpc_pack_provisional('2040 P-L'), 'PLS2040'));
run_test('unpack J95X00A -> 1995 XA', strcmp(mpc_unpack_provisional('J95X00A'), '1995 XA'));
run_test('unpack K24A01B -> 2024 AB1', strcmp(mpc_unpack_provisional('K24A01B'), '2024 AB1'));
run_test('unpack PLS2040 -> 2040 P-L', strcmp(mpc_unpack_provisional('PLS2040'), '2040 P-L'));
fprintf('\n');

% Extended provisional tests
fprintf('Extended provisional:\n');
run_test('pack 2024 AB631 -> _OA004S', strcmp(mpc_pack_provisional('2024 AB631'), '_OA004S'));
run_test('pack 2024 AA631 -> _OA004R', strcmp(mpc_pack_provisional('2024 AA631'), '_OA004R'));
run_test('unpack _OA004S -> 2024 AB631', strcmp(mpc_unpack_extended_provisional('_OA004S'), '2024 AB631'));
run_test('unpack _OA004R -> 2024 AA631', strcmp(mpc_unpack_extended_provisional('_OA004R'), '2024 AA631'));
fprintf('\n');

% Comet tests
fprintf('Comets:\n');
run_test('pack 1P -> 0001P', strcmp(mpc_pack_comet_numbered('1P'), '0001P'));
run_test('pack C/1995 O1 -> CJ95O010', strcmp(mpc_pack_comet_full('C/1995 O1'), 'CJ95O010'));
run_test('pack D/1993 F2-B -> DJ93F02b', strcmp(mpc_pack_comet_full('D/1993 F2-B'), 'DJ93F02b'));
run_test('unpack 0001P -> 1P', strcmp(mpc_unpack_comet_numbered('0001P'), '1P'));
run_test('unpack CJ95O010 -> C/1995 O1', strcmp(mpc_unpack_comet_full('CJ95O010'), 'C/1995 O1'));
run_test('unpack DJ93F02b -> D/1993 F2-B', strcmp(mpc_unpack_comet_full('DJ93F02b'), 'D/1993 F2-B'));
fprintf('\n');

% Satellite tests
fprintf('Satellites:\n');
run_test('pack S/2019 S 22 -> SK19S220', strcmp(mpc_pack_satellite('S/2019 S 22'), 'SK19S220'));
run_test('pack S/2003 J 2 -> SK03J020', strcmp(mpc_pack_satellite('S/2003 J 2'), 'SK03J020'));
run_test('unpack SK19S220 -> S/2019 S 22', strcmp(mpc_unpack_satellite('SK19S220'), 'S/2019 S 22'));
run_test('unpack SK03J020 -> S/2003 J 2', strcmp(mpc_unpack_satellite('SK03J020'), 'S/2003 J 2'));
fprintf('\n');

% High-level API tests
fprintf('High-level API:\n');
run_test('convert_simple 1995 XA -> J95X00A', strcmp(mpc_convert_simple('1995 XA'), 'J95X00A'));
run_test('convert_simple J95X00A -> 1995 XA', strcmp(mpc_convert_simple('J95X00A'), '1995 XA'));
run_test('pack already packed', strcmp(mpc_pack('J95X00A'), 'J95X00A'));
run_test('unpack already unpacked', strcmp(mpc_unpack('1995 XA'), '1995 XA'));
run_test('is_valid valid', mpc_is_valid('1995 XA'));
run_test('is_valid invalid', ~mpc_is_valid('invalid'));
fprintf('\n');

% Format detection tests
fprintf('Format detection:\n');
info = mpc_detect_format('1995 XA');
run_test('detect unpacked provisional', strcmp(info.format, 'unpacked') && strcmp(info.type, 'provisional'));
info = mpc_detect_format('J95X00A');
run_test('detect packed provisional', strcmp(info.format, 'packed') && strcmp(info.type, 'provisional'));
info = mpc_detect_format('1');
run_test('detect unpacked permanent', strcmp(info.format, 'unpacked') && strcmp(info.type, 'permanent'));
info = mpc_detect_format('00001');
run_test('detect packed permanent', strcmp(info.format, 'packed') && strcmp(info.type, 'permanent'));
fprintf('\n');

% Summary
fprintf('=== Results: %d passed, %d failed ===\n', tests_passed, tests_failed);

if tests_failed > 0
  exit(1);
end
