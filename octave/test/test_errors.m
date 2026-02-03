#!/usr/bin/env octave
% test_errors.m - Error handling tests for MPC designation converter

1;

% Source the library
script_dir = fileparts(mfilename('fullpath'));
if isempty(script_dir)
  script_dir = '.';
end
source(fullfile(script_dir, '..', 'src', 'mpc_designation.m'));

% Find test data file
test_data_file = fullfile(script_dir, '..', '..', 'test-data', 'error_test_cases.csv');

if ~exist(test_data_file, 'file')
  fprintf(2, 'Error: Test data file not found: %s\n', test_data_file);
  exit(1);
end

fprintf('=== Error Handling Tests ===\n\n');

% Read CSV
fid = fopen(test_data_file, 'r');
content = fread(fid, '*char')';
fclose(fid);

lines = strsplit(content, '\n');
% Skip header
lines = lines(2:end);

passed = 0;
failed = 0;
errors = {};

for i = 1:length(lines)
  line = lines{i};
  if isempty(line)
    continue;
  end

  % Parse CSV (handle quoted fields)
  if line(1) == '"'
    % Find closing quote
    close_quote = strfind(line(2:end), '"');
    if ~isempty(close_quote)
      input_str = line(2:close_quote(1));
      % Rest after the comma
    else
      continue;
    end
  else
    parts = strsplit(line, ',');
    if length(parts) < 2
      continue;
    end
    input_str = parts{1};
  end

  % Test that it produces an error
  try
    mpc_convert_simple(input_str);
    % If we get here, it didn't error
    failed = failed + 1;
    if length(errors) < 10
      errors{end+1} = sprintf('  ''%s'' should error but succeeded', input_str);
    end
  catch
    % Expected - it should error
    passed = passed + 1;
  end
end

fprintf('Passed: %d / %d\n', passed, passed + failed);
fprintf('Failed: %d\n', failed);

if failed > 0
  fprintf('\nFailures:\n');
  for i = 1:length(errors)
    fprintf('%s\n', errors{i});
  end
  exit(1);
else
  fprintf('\nAll error tests passed!\n');
end
