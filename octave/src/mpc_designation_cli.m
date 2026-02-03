#!/usr/bin/env octave
% MPC Designation CLI - Command-line interface
%
% Usage: octave --no-gui mpc_designation_cli.m [options] designation [designation ...]
%
% Options:
%   -v, --verbose   Show detailed information
%   -h, --help      Show help message

% Prevent running as function
1;

function print_usage()
  fprintf('Usage: octave --no-gui mpc_designation_cli.m [-v|--verbose] <designation> [designation ...]\n');
  fprintf('\n');
  fprintf('Convert between packed and unpacked MPC designations.\n');
  fprintf('Auto-detects the input format and converts to the other.\n');
  fprintf('\n');
  fprintf('Options:\n');
  fprintf('  -v, --verbose   Show detailed information about the conversion\n');
  fprintf('  -h, --help      Show this help message\n');
  fprintf('\n');
  fprintf('Examples:\n');
  fprintf('  octave --no-gui mpc_designation_cli.m 00001             -> 1\n');
  fprintf('  octave --no-gui mpc_designation_cli.m 1                 -> 00001\n');
  fprintf('  octave --no-gui mpc_designation_cli.m J95X00A           -> 1995 XA\n');
  fprintf('  octave --no-gui mpc_designation_cli.m ''1995 XA''         -> J95X00A\n');
end

% Source the library
script_dir = fileparts(mfilename('fullpath'));
if isempty(script_dir)
  script_dir = '.';
end
source(fullfile(script_dir, 'mpc_designation.m'));

% Get command line arguments
args = argv();

if length(args) == 0
  print_usage();
  exit(1);
end

verbose = false;
designations = {};

for i = 1:length(args)
  arg = args{i};
  if strcmp(arg, '-v') || strcmp(arg, '--verbose')
    verbose = true;
  elseif strcmp(arg, '-h') || strcmp(arg, '--help')
    print_usage();
    exit(0);
  else
    designations{end+1} = arg;
  end
end

if length(designations) == 0
  print_usage();
  exit(1);
end

multiple = length(designations) > 1;

for i = 1:length(designations)
  des = designations{i};
  try
    result = mpc_convert(des);
    info = result.info;
    output = result.output;

    if verbose
      fprintf('  Input:    %s\n', des);
      fprintf('  Detected: %s format, %s\n', info.format, info.subtype);
      if strcmp(info.format, 'packed')
        action = 'unpacking to human-readable form';
      else
        action = 'packing to MPC compact form';
      end
      fprintf('  Action:   %s\n', action);
      fprintf('  Output:   %s\n', output);
      if multiple
        fprintf('\n');
      end
    elseif multiple
      fprintf('%s -> %s\n', des, output);
    else
      fprintf('%s\n', output);
    end
  catch err
    fprintf(2, 'Error: %s\n', err.message);
    exit(1);
  end
end
