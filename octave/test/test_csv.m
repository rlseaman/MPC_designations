#!/usr/bin/env octave
% test_csv.m - CSV test suite for MPC designation converter

1;

% Source the library
script_dir = fileparts(mfilename('fullpath'));
if isempty(script_dir)
  script_dir = '.';
end
source(fullfile(script_dir, '..', 'src', 'mpc_designation.m'));

% Find test data file
test_data_file = fullfile(script_dir, '..', '..', 'test-data', 'prov_unpack_to_pack.csv.gz');

if ~exist(test_data_file, 'file')
  fprintf(2, 'Error: Test data file not found: %s\n', test_data_file);
  exit(1);
end

fprintf('Loading test data...\n');

% Read gzipped CSV using gunzip
[status, output] = system(sprintf('gunzip -c "%s"', test_data_file));
if status ~= 0
  fprintf(2, 'Error: Failed to decompress test data\n');
  exit(1);
end

lines = strsplit(output, '\n');
% Skip header and remove empty lines
lines = lines(2:end);
lines = lines(~cellfun(@isempty, lines));

total = length(lines);
fprintf('Testing %d conversions...\n', total);

passed = 0;
failed = 0;
errors = {};

tic;

for i = 1:total
  parts = strsplit(lines{i}, ',');
  if length(parts) < 2
    continue;
  end

  unpacked = parts{1};
  expected_packed = parts{2};

  try
    result = mpc_pack(unpacked);
    if strcmp(result, expected_packed)
      passed = passed + 1;
    else
      failed = failed + 1;
      if length(errors) < 10
        errors{end+1} = sprintf('  %s -> %s (expected %s)', unpacked, result, expected_packed);
      end
    end
  catch err
    failed = failed + 1;
    if length(errors) < 10
      errors{end+1} = sprintf('  %s -> ERROR: %s', unpacked, err.message);
    end
  end

  % Progress update every 100k
  if mod(i, 100000) == 0
    fprintf('  Progress: %d / %d (%.1f%%)\n', i, total, 100 * i / total);
  end
end

elapsed = toc;

fprintf('\n');
fprintf('=== Results ===\n');
fprintf('Passed: %d / %d\n', passed, total);
fprintf('Failed: %d\n', failed);
fprintf('Time: %.2f seconds\n', elapsed);
fprintf('Rate: %.0f conversions/second\n', total / elapsed);

if failed > 0
  fprintf('\nFirst failures:\n');
  for i = 1:length(errors)
    fprintf('%s\n', errors{i});
  end
  exit(1);
else
  fprintf('\nAll tests passed!\n');
end
