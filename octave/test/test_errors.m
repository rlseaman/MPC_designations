#!/usr/bin/env octave
% test_errors.m - Error handling tests for MPC designation converter
%
% Reads test-data/error_test_cases.csv (columns: category, subcategory,
% input, expected_error, description). Rows with expected_error == 'valid'
% must convert successfully; all other rows must raise an error.
% Escape sequences in the input column (\t, \n, \r, \f, \v, \0, \\, \xNN)
% are decoded before testing.

1;

function s = unescape_input(s)
  % Decode \xNN hex escapes first, then the standard escapes
  [tok, starts, ends] = regexp(s, '\\x([0-9a-fA-F]{2})', 'tokens', 'start', 'end');
  for k = numel(starts):-1:1
    s = [s(1:starts(k)-1), char(hex2dec(tok{k}{1})), s(ends(k)+1:end)];
  end
  s = strrep(s, '\n', char(10));
  s = strrep(s, '\r', char(13));
  s = strrep(s, '\t', char(9));
  s = strrep(s, '\f', char(12));
  s = strrep(s, '\v', char(11));
  s = strrep(s, '\0', char(0));
  s = strrep(s, '\\', '\');
end

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

fid = fopen(test_data_file, 'r');
content = fread(fid, '*char')';
fclose(fid);

lines = strsplit(content, char(10));

passed = 0;
failed = 0;

for i = 1:length(lines)
  line = strtrim(lines{i});
  if isempty(line) || line(1) == '#' || strncmp(line, 'category,', 9)
    continue;
  end

  % Split into at most 5 fields (description may contain commas)
  commas = find(line == ',');
  if length(commas) < 4
    continue;
  end
  category = line(1:commas(1)-1);
  subcategory = line(commas(1)+1:commas(2)-1);
  input_str = unescape_input(line(commas(2)+1:commas(3)-1));
  expected = line(commas(3)+1:commas(4)-1);
  description = line(commas(4)+1:end);

  got_error = false;
  output = '';
  err_msg = '';
  try
    output = mpc_convert_simple(input_str);
  catch e
    got_error = true;
    err_msg = e.message;
  end

  if strcmp(expected, 'valid')
    if ~got_error
      passed = passed + 1;
    else
      failed = failed + 1;
      fprintf('FAIL [%s/%s]: ''%s''\n', category, subcategory, description);
      fprintf('      Expected: valid conversion\n');
      fprintf('      Got:      %s\n', err_msg);
    end
  else
    if got_error
      passed = passed + 1;
    else
      failed = failed + 1;
      fprintf('FAIL [%s/%s]: ''%s''\n', category, subcategory, description);
      fprintf('      Expected: error (%s)\n', expected);
      fprintf('      Got:      ''%s'' (success)\n', output);
    end
  end
end

fprintf('\n=== Error Test Results ===\n');
fprintf('Total:  %d\n', passed + failed);
fprintf('Passed: %d\n', passed);
fprintf('Failed: %d\n', failed);

if failed > 0
  exit(1);
else
  fprintf('\nAll error tests passed!\n');
end
