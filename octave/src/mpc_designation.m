% MPC_DESIGNATION - Convert between packed and unpacked MPC designations
%
% Functions:
%   mpc_convert_simple(des)     - Auto-detect and convert, return string
%   mpc_convert(des)            - Auto-detect and convert, return struct
%   mpc_pack(des)               - Ensure packed format
%   mpc_unpack(des)             - Ensure unpacked format
%   mpc_is_valid(des)           - Check if valid (returns true/false)
%   mpc_detect_format(des)      - Return format info without converting
%
% Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
%
% Compatible with GNU Octave and MATLAB.

% Prevent this file from being run as a script
1;

% =============================================================================
% Constants
% =============================================================================

function chars = base62_chars()
  chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
end

function n = max_asteroid_number()
  n = 15396335;  % 620000 + 62^4 - 1
end

% =============================================================================
% Base-62 encoding utilities
% =============================================================================

function n = base62_to_num(c)
  chars = base62_chars();
  idx = find(chars == c, 1);
  if isempty(idx)
    error('MPCDesignationError:InvalidChar', 'Invalid base-62 character: %s', c);
  end
  n = idx - 1;  % 0-indexed
end

function c = num_to_base62(n)
  if n < 0 || n > 61
    error('MPCDesignationError:OutOfRange', 'Number out of base-62 range: %d', n);
  end
  chars = base62_chars();
  c = chars(n + 1);
end

function n = base62_string_to_num(s)
  n = 0;
  for i = 1:length(s)
    n = n * 62 + base62_to_num(s(i));
  end
end

function s = num_to_base62_string(n, width)
  s = blanks(width);
  for i = width:-1:1
    s(i) = num_to_base62(mod(n, 62));
    n = floor(n / 62);
  end
end

% =============================================================================
% Cycle count encoding
% =============================================================================

function n = decode_cycle_count(encoded)
  if length(encoded) < 2
    error('MPCDesignationError:InvalidEncoding', 'Invalid cycle count encoding');
  end

  first = encoded(1);
  second = encoded(2);

  if first >= '0' && first <= '9'
    tens = first - '0';
  elseif first >= 'A' && first <= 'Z'
    tens = first - 'A' + 10;
  elseif first >= 'a' && first <= 'z'
    tens = first - 'a' + 36;
  else
    error('MPCDesignationError:InvalidEncoding', 'Invalid cycle count encoding');
  end

  if second < '0' || second > '9'
    error('MPCDesignationError:InvalidEncoding', 'Invalid cycle count encoding');
  end

  n = tens * 10 + (second - '0');
end

function s = encode_cycle_count(count)
  if count < 0 || count >= 620
    error('MPCDesignationError:OutOfRange', 'Cycle count out of range (0-619): %d', count);
  end

  tens = floor(count / 10);
  ones = mod(count, 10);

  if tens < 10
    first = char('0' + tens);
  elseif tens < 36
    first = char('A' + tens - 10);
  else
    first = char('a' + tens - 36);
  end

  s = [first, char('0' + ones)];
end

% =============================================================================
% Letter/position utilities
% =============================================================================

function pos = letter_to_position(letter)
  if letter < 'A' || letter > 'Z'
    error('MPCDesignationError:InvalidLetter', 'Invalid half-month letter: %s', letter);
  end
  pos = letter - 'A' + 1;
  if letter > 'I'
    pos = pos - 1;  % Skip I
  end
end

function letter = position_to_letter(pos)
  if pos < 1 || pos > 25
    error('MPCDesignationError:InvalidPosition', 'Invalid letter position: %d', pos);
  end
  p = pos;
  if p >= 9
    p = p + 1;  % Skip I
  end
  letter = char('A' + p - 1);
end

function valid = is_valid_half_month(c)
  valid = (c >= 'A' && c <= 'Y' && c ~= 'I');
end

% =============================================================================
% Century codes
% =============================================================================

function code = century_to_code(century)
  codes = 'ABCDEFGHIJKL';
  if century < 10 || century > 21
    error('MPCDesignationError:InvalidCentury', 'Invalid century: %d', century);
  end
  code = codes(century - 9);
end

function century = code_to_century(code)
  codes = 'ABCDEFGHIJKL';
  idx = find(codes == code, 1);
  if isempty(idx)
    error('MPCDesignationError:InvalidCode', 'Invalid century code: %s', code);
  end
  century = idx + 9;
end

% =============================================================================
% Permanent (numbered) asteroid designations
% =============================================================================

function n = mpc_unpack_permanent(packed)
  packed = strtrim(packed);
  if length(packed) ~= 5
    error('MPCDesignationError:InvalidLength', 'Invalid packed permanent designation length');
  end

  first = packed(1);

  % Tilde format (>= 620,000)
  if first == '~'
    n = 620000 + base62_string_to_num(packed(2:5));
    return;
  end

  % Simple numeric format (< 100,000)
  if first >= '0' && first <= '9'
    n = str2double(packed);
    return;
  end

  % Extended format with uppercase letter (100,000 - 359,999)
  if first >= 'A' && first <= 'Z'
    val = double(first) - 55;  % A=10
    rest = str2double(packed(2:5));
    n = val * 10000 + rest;
    return;
  end

  % Extended format with lowercase letter (360,000 - 619,999)
  if first >= 'a' && first <= 'z'
    val = double(first) - 61;  % a=36
    rest = str2double(packed(2:5));
    n = val * 10000 + rest;
    return;
  end

  error('MPCDesignationError:InvalidFormat', 'Invalid packed permanent designation');
end

function s = mpc_pack_permanent(number)
  if number < 1 || number > max_asteroid_number()
    error('MPCDesignationError:InvalidNumber', 'Invalid asteroid number: %d', number);
  end

  if number < 100000
    s = sprintf('%05d', number);
    return;
  end

  if number < 620000
    div_val = floor(number / 10000);
    mod_val = mod(number, 10000);
    if div_val < 36
      letter = char(div_val + 55);  % A-Z
    else
      letter = char(div_val + 61);  % a-z
    end
    s = sprintf('%s%04d', letter, mod_val);
    return;
  end

  % Tilde + base-62 format
  offset = number - 620000;
  s = ['~', num_to_base62_string(offset, 4)];
end

% =============================================================================
% Provisional asteroid designations
% =============================================================================

function s = mpc_unpack_provisional(packed)
  packed = strtrim(packed);

  % Check for survey designations
  if length(packed) == 7
    prefix = packed(1:3);
    if strcmp(prefix, 'PLS')
      num = str2double(packed(4:7));
      s = sprintf('%d P-L', num);
      return;
    elseif strcmp(prefix, 'T1S')
      num = str2double(packed(4:7));
      s = sprintf('%d T-1', num);
      return;
    elseif strcmp(prefix, 'T2S')
      num = str2double(packed(4:7));
      s = sprintf('%d T-2', num);
      return;
    elseif strcmp(prefix, 'T3S')
      num = str2double(packed(4:7));
      s = sprintf('%d T-3', num);
      return;
    end
  end

  if length(packed) ~= 7
    error('MPCDesignationError:InvalidLength', 'Invalid packed provisional designation length');
  end

  century_code = packed(1);
  year = packed(2:3);
  half_month = packed(4);
  order_encoded = packed(5:6);
  second_letter = packed(7);

  century = code_to_century(century_code);
  full_year = sprintf('%d%s', century, year);
  order_num = decode_cycle_count(order_encoded);

  if order_num == 0
    s = sprintf('%s %s%s', full_year, half_month, second_letter);
  else
    s = sprintf('%s %s%s%d', full_year, half_month, second_letter, order_num);
  end
end

function s = mpc_pack_provisional(unpacked)
  unpacked = strtrim(unpacked);

  % Check for survey designations
  [tokens, ~] = regexp(unpacked, '^(\d+) (P-L|T-[123])$', 'tokens', 'match');
  if ~isempty(tokens)
    number = str2double(tokens{1}{1});
    survey = tokens{1}{2};
    if number < 1
      error('MPCDesignationError:InvalidNumber', 'Survey number must be positive');
    end
    if strcmp(survey, 'P-L')
      s = sprintf('PLS%04d', number);
    elseif strcmp(survey, 'T-1')
      s = sprintf('T1S%04d', number);
    elseif strcmp(survey, 'T-2')
      s = sprintf('T2S%04d', number);
    else
      s = sprintf('T3S%04d', number);
    end
    return;
  end

  % Check for old-style designation: "A908 CJ" or "B842 FA"
  [tokens, ~] = regexp(unpacked, '^[AB](\d)(\d{2}) ([A-Z])([A-Z])$', 'tokens', 'match');
  if ~isempty(tokens)
    century_digit = tokens{1}{1};
    year_short = tokens{1}{2};
    half_month = tokens{1}{3};
    second_letter = tokens{1}{4};

    if strcmp(century_digit, '8')
      century_code = 'I';
    elseif strcmp(century_digit, '9')
      century_code = 'J';
    elseif strcmp(century_digit, '0')
      century_code = 'K';
    else
      error('MPCDesignationError:InvalidCentury', 'Invalid century digit in old-style designation');
    end

    s = sprintf('%s%s%s00%s', century_code, year_short, half_month, second_letter);
    return;
  end

  % Match standard provisional: "1995 XA" or "1995 XA12"
  [tokens, ~] = regexp(unpacked, '^(\d{4}) ([A-Z])([A-Z])(\d*)$', 'tokens', 'match');
  if isempty(tokens)
    error('MPCDesignationError:InvalidFormat', 'Invalid unpacked provisional designation: %s', unpacked);
  end

  year = tokens{1}{1};
  half_month = tokens{1}{2};
  second_letter = tokens{1}{3};
  order_str = tokens{1}{4};

  if ~is_valid_half_month(half_month)
    error('MPCDesignationError:InvalidLetter', 'Invalid half-month letter: %s', half_month);
  end

  century = str2double(year(1:2));
  year_short = year(3:4);
  century_code = century_to_code(century);

  if isempty(order_str)
    order_num = 0;
  else
    order_num = str2double(order_str);
  end

  % Check if we need extended format
  if order_num >= 620
    s = pack_extended_provisional(str2double(year), half_month, second_letter, order_num);
    return;
  end

  order_encoded = encode_cycle_count(order_num);
  s = sprintf('%s%s%s%s%s', century_code, year_short, half_month, order_encoded, second_letter);
end

% =============================================================================
% Extended provisional format (cycle >= 620)
% =============================================================================

function s = pack_extended_provisional(year, half_month, second_letter, cycle)
  year_short = mod(year, 100);
  letter_pos = letter_to_position(second_letter);
  base_sequence = (cycle - 620) * 25 + letter_pos - 1;
  seq_encoded = num_to_base62_string(base_sequence, 4);
  year_char = num_to_base62(year_short);
  s = sprintf('_%s%s%s', year_char, half_month, seq_encoded);
end

function s = mpc_unpack_extended_provisional(packed)
  packed = strtrim(packed);
  if length(packed) ~= 7 || packed(1) ~= '_'
    error('MPCDesignationError:InvalidFormat', 'Invalid extended packed provisional');
  end

  year_digit = packed(2);
  half_month = packed(3);
  seq_encoded = packed(4:7);

  base_sequence = base62_string_to_num(seq_encoded);
  cycle = 620 + floor(base_sequence / 25);
  letter_pos = mod(base_sequence, 25) + 1;
  second_letter = position_to_letter(letter_pos);

  year_val = base62_to_num(year_digit);
  year = 2000 + year_val;

  s = sprintf('%d %s%s%d', year, half_month, second_letter, cycle);
end

% =============================================================================
% Comet designations
% =============================================================================

function s = mpc_unpack_comet_provisional(packed)
  packed = strtrim(packed);
  len = length(packed);

  if len ~= 7 && len ~= 8
    error('MPCDesignationError:InvalidLength', 'Invalid packed comet provisional designation length');
  end

  century_code = packed(1);
  year = packed(2:3);
  half_month = packed(4);
  order_encoded = packed(5:6);
  if len == 7
    fragment = packed(7);
  else
    fragment = packed(7:8);
  end

  century = code_to_century(century_code);
  full_year = sprintf('%d%s', century, year);
  order_num = decode_cycle_count(order_encoded);

  s = sprintf('%s %s%d', full_year, half_month, order_num);
  if ~strcmp(fragment, '0')
    s = sprintf('%s-%s', s, upper(fragment));
  end
end

function s = mpc_pack_comet_provisional(unpacked)
  unpacked = strtrim(unpacked);

  [tokens, ~] = regexp(unpacked, '^(\d{4}) ([A-Z])(\d+)(-([A-Z]{1,2}))?$', 'tokens', 'match');
  if isempty(tokens)
    error('MPCDesignationError:InvalidFormat', 'Invalid unpacked comet provisional designation: %s', unpacked);
  end

  year = tokens{1}{1};
  half_month = tokens{1}{2};
  order_str = tokens{1}{3};
  fragment = '';
  if length(tokens{1}) >= 5
    fragment = tokens{1}{5};
  end

  order_num = str2double(order_str);
  if order_num < 1
    error('MPCDesignationError:InvalidNumber', 'Comet order number must be positive');
  end

  century = str2double(year(1:2));
  year_short = year(3:4);
  century_code = century_to_code(century);
  order_encoded = encode_cycle_count(order_num);

  if isempty(fragment)
    fragment_code = '0';
  else
    fragment_code = lower(fragment);
  end

  s = sprintf('%s%s%s%s%s', century_code, year_short, half_month, order_encoded, fragment_code);
end

function s = mpc_unpack_comet_numbered(packed)
  packed = strtrim(packed);

  [tokens, ~] = regexp(packed, '^(\d{4})([PD])$', 'tokens', 'match');
  if isempty(tokens)
    error('MPCDesignationError:InvalidFormat', 'Invalid packed numbered comet designation');
  end

  number = str2double(tokens{1}{1});
  comet_type = tokens{1}{2};
  s = sprintf('%d%s', number, comet_type);
end

function s = mpc_pack_comet_numbered(unpacked)
  unpacked = strtrim(unpacked);

  [tokens, ~] = regexp(unpacked, '^(\d+)([PD])(/[A-Za-z].*)?$', 'tokens', 'match');
  if isempty(tokens)
    error('MPCDesignationError:InvalidFormat', 'Invalid unpacked numbered comet designation');
  end

  number = str2double(tokens{1}{1});
  comet_type = tokens{1}{2};

  if number < 1 || number > 9999
    error('MPCDesignationError:OutOfRange', 'Comet number out of range (1-9999): %d', number);
  end

  s = sprintf('%04d%s', number, comet_type);
end

% =============================================================================
% Satellite designations
% =============================================================================

function s = mpc_unpack_satellite(packed)
  packed = strtrim(packed);

  if length(packed) ~= 8 || packed(1) ~= 'S'
    error('MPCDesignationError:InvalidFormat', 'Invalid packed satellite designation');
  end

  century_code = packed(2);
  year = packed(3:4);
  planet = packed(5);
  number_encoded = packed(6:7);

  if ~any(planet == 'JSUN')
    error('MPCDesignationError:InvalidPlanet', 'Invalid planet code: %s', planet);
  end

  century = code_to_century(century_code);
  full_year = sprintf('%d%s', century, year);
  number = decode_cycle_count(number_encoded);

  s = sprintf('S/%s %s %d', full_year, planet, number);
end

function s = mpc_pack_satellite(unpacked)
  unpacked = strtrim(unpacked);

  [tokens, ~] = regexp(unpacked, '^S/(\d{4}) ([JSUN]) (\d+)$', 'tokens', 'match');
  if isempty(tokens)
    error('MPCDesignationError:InvalidFormat', 'Invalid unpacked satellite designation');
  end

  year = tokens{1}{1};
  planet = tokens{1}{2};
  number = str2double(tokens{1}{3});

  if number < 1
    error('MPCDesignationError:InvalidNumber', 'Satellite number must be positive');
  end

  century = str2double(year(1:2));
  year_short = year(3:4);
  century_code = century_to_code(century);
  number_encoded = encode_cycle_count(number);

  s = sprintf('S%s%s%s%s0', century_code, year_short, planet, number_encoded);
end

% =============================================================================
% Ancient/BCE comet designations
% =============================================================================

function [prefix, code] = encode_bce_year(year)
  if year >= 0
    error('MPCDesignationError:InvalidYear', 'Not a BCE year: %d', year);
  end

  abs_year = abs(year);
  code_num = 99 - mod(abs_year, 100);

  if abs_year < 100
    prefix = '/';
  elseif abs_year < 200
    prefix = '.';
  elseif abs_year < 300
    prefix = '-';
  else
    error('MPCDesignationError:OutOfRange', 'BCE year out of supported range: %d', year);
  end

  code = sprintf('%02d', code_num);
end

function year = decode_bce_year(prefix, code)
  code_num = str2double(code);
  year_part = 99 - code_num;

  if prefix == '/'
    year = -year_part;
  elseif prefix == '.'
    year = -(year_part + 100);
  elseif prefix == '-'
    year = -(year_part + 200);
  else
    error('MPCDesignationError:InvalidPrefix', 'Invalid BCE prefix: %s', prefix);
  end
end

function s = pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment)
  order_encoded = encode_cycle_count(order_num);
  if isempty(fragment)
    fragment_code = '0';
  else
    fragment_code = lower(fragment);
  end

  if year < 0
    [prefix, code] = encode_bce_year(year);
    s = sprintf('%s%s%s%s%s%s', comet_type, prefix, code, half_month, order_encoded, fragment_code);
  else
    s = sprintf('%s%03d%s%s%s', comet_type, year, half_month, order_encoded, fragment_code);
  end
end

function s = mpc_unpack_ancient_comet(packed)
  packed = strtrim(packed);

  if length(packed) ~= 8
    error('MPCDesignationError:InvalidLength', 'Invalid ancient comet designation length');
  end

  comet_type = packed(1);
  if ~any(comet_type == 'PCDXAI')
    error('MPCDesignationError:InvalidType', 'Invalid comet type: %s', comet_type);
  end

  if any(packed(2) == '/.-')
    year = decode_bce_year(packed(2), packed(3:4));
    half_month = packed(5);
    order_encoded = packed(6:7);
    fragment = packed(8);
  else
    year = str2double(packed(2:4));
    half_month = packed(5);
    order_encoded = packed(6:7);
    fragment = packed(8);
  end

  order_num = decode_cycle_count(order_encoded);
  s = sprintf('%s/%d %s%d', comet_type, year, half_month, order_num);

  if fragment ~= '0'
    s = sprintf('%s-%s', s, upper(fragment));
  end
end

% =============================================================================
% Full comet designations
% =============================================================================

function packed_ok = is_asteroid_style_packed(provisional_part)
  packed_ok = false;
  if length(provisional_part) ~= 7
    return;
  end
  last_char = provisional_part(7);
  packed_ok = (last_char >= 'A' && last_char <= 'Z');
end

function unpacked_ok = is_asteroid_style_unpacked(provisional)
  unpacked_ok = false;
  [tokens, ~] = regexp(provisional, '^\d{4} ([A-Z])(.)', 'tokens', 'match');
  if isempty(tokens)
    return;
  end
  second_char = tokens{1}{2};
  unpacked_ok = (second_char >= 'A' && second_char <= 'Z') || (second_char >= 'a' && second_char <= 'z');
end

function s = mpc_unpack_comet_full(packed)
  packed_orig = packed;
  len = length(packed);

  if len == 8
    comet_type = packed(1);
    provisional_part = packed(2:8);

    if ~any(comet_type == 'PCDXAI')
      error('MPCDesignationError:InvalidType', 'Invalid comet type: %s', comet_type);
    end

    if is_asteroid_style_packed(provisional_part)
      provisional = mpc_unpack_provisional(provisional_part);
    else
      provisional = mpc_unpack_comet_provisional(provisional_part);
    end

    s = sprintf('%s/%s', comet_type, provisional);
    return;
  end

  if len == 9
    comet_type = packed(1);
    provisional_part = packed(2:9);

    if ~any(comet_type == 'PCDXAI')
      error('MPCDesignationError:InvalidType', 'Invalid comet type: %s', comet_type);
    end

    provisional = mpc_unpack_comet_provisional(provisional_part);
    s = sprintf('%s/%s', comet_type, provisional);
    return;
  end

  if len == 12 || (len < 12 && packed(1) == ' ')
    while length(packed) < 12
      packed = [' ', packed];
    end

    num_part = strtrim(packed(1:4));
    comet_type = packed(5);
    provisional_part = packed(6:12);

    if ~any(comet_type == 'PCDXAI')
      error('MPCDesignationError:InvalidType', 'Invalid comet type: %s', comet_type);
    end

    if is_asteroid_style_packed(provisional_part)
      provisional = mpc_unpack_provisional(provisional_part);
    else
      provisional = mpc_unpack_comet_provisional(provisional_part);
    end

    if isempty(num_part)
      s = sprintf('%s/%s', comet_type, provisional);
    else
      num = str2double(num_part);
      s = sprintf('%d%s/%s', num, comet_type, provisional);
    end
    return;
  end

  error('MPCDesignationError:InvalidLength', 'Invalid packed full comet designation length');
end

function s = mpc_pack_comet_full(unpacked)
  unpacked = strtrim(unpacked);

  % Try numbered comet first (e.g., "1P/1995 O1")
  [tokens, ~] = regexp(unpacked, '^(\d+)([PCDXAI])/(-?\d+) (.+)$', 'tokens', 'match');
  if ~isempty(tokens)
    tok = tokens{1};
    number_str = tok{1};
    comet_type = tok{2};
    year = str2double(tok{3});
    prov_part = tok{4};
  else
    % Try unnumbered comet (e.g., "C/1995 O1")
    [tokens, ~] = regexp(unpacked, '^([PCDXAI])/(-?\d+) (.+)$', 'tokens', 'match');
    if isempty(tokens)
      error('MPCDesignationError:InvalidFormat', 'Invalid unpacked comet designation: %s', unpacked);
    end
    tok = tokens{1};
    number_str = '';
    comet_type = tok{1};
    year = str2double(tok{2});
    prov_part = tok{3};
  end

  % Check for ancient or BCE year
  if year < 1000
    % Try to match with fragment first
    [tokens2, ~] = regexp(prov_part, '^([A-Z])(\d+)-([A-Z])$', 'tokens', 'match');
    if ~isempty(tokens2)
      tok2 = tokens2{1};
      half_month = tok2{1};
      order_num = str2double(tok2{2});
      fragment = tok2{3};
      s = pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment);
      return;
    end
    % Try without fragment
    [tokens2, ~] = regexp(prov_part, '^([A-Z])(\d+)$', 'tokens', 'match');
    if ~isempty(tokens2)
      tok2 = tokens2{1};
      half_month = tok2{1};
      order_num = str2double(tok2{2});
      s = pack_ancient_comet_provisional(comet_type, year, half_month, order_num, '');
      return;
    end
    error('MPCDesignationError:InvalidFormat', 'Invalid ancient comet provisional: %s', prov_part);
  end

  % Modern comet
  provisional = sprintf('%d %s', year, prov_part);

  if is_asteroid_style_unpacked(provisional)
    provisional_packed = mpc_pack_provisional(provisional);
  else
    provisional_packed = mpc_pack_comet_provisional(provisional);
  end

  if isempty(number_str)
    s = sprintf('%s%s', comet_type, provisional_packed);
  else
    num = str2double(number_str);
    if num < 1 || num > 9999
      error('MPCDesignationError:OutOfRange', 'Comet number out of range (1-9999): %d', num);
    end
    s = sprintf('%04d%s%s', num, comet_type, provisional_packed);
  end
end

% =============================================================================
% Format detection
% =============================================================================

function info = mpc_detect_format(designation)
  % Validate input
  if ~ischar(designation) && ~isstring(designation)
    error('MPCDesignationError:InvalidInput', 'Designation must be a string');
  end
  designation = char(designation);

  % Check for non-printable characters
  codes = double(designation);
  if any(codes < 32 | codes > 126)
    error('MPCDesignationError:InvalidChar', 'Invalid character in designation');
  end

  info = struct('format', '', 'type', '', 'subtype', '');

  % Check for packed full comet (12 chars with spaces)
  if length(designation) == 12
    if ~isempty(regexp(designation, '^[ 0-9]{4}[PCDXAI][IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$', 'once'))
      info.format = 'packed';
      info.type = 'comet_full';
      info.subtype = 'comet with provisional designation (12-char)';
      return;
    end
  end

  % Check for packed comet (8 chars)
  if length(designation) == 8 && any(designation(1) == 'PCDXAI')
    if ~isempty(regexp(designation, '^[PCDXAI][A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z]$', 'once'))
      info.format = 'packed';
      info.type = 'comet_full';
      info.subtype = 'comet with provisional designation (8-char)';
      return;
    end
  end

  % Check for packed comet with 2-letter fragment (9 chars)
  if length(designation) == 9 && any(designation(1) == 'PCDXAI')
    if ~isempty(regexp(designation, '^[PCDXAI][A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2}$', 'once'))
      info.format = 'packed';
      info.type = 'comet_full';
      info.subtype = 'comet with provisional designation (9-char, 2-letter fragment)';
      return;
    end
  end

  % Check for packed ancient comet (8 chars)
  if length(designation) == 8 && any(designation(1) == 'PCDXAI')
    if ~isempty(regexp(designation, '^[PCDXAI][0-9]{3}[A-Z][0-9A-Za-z]{2}[0-9a-z]$', 'once'))
      info.format = 'packed';
      info.type = 'comet_ancient';
      info.subtype = 'comet with ancient provisional (year < 1000)';
      return;
    end
  end

  % Check for packed BCE comet (8 chars)
  if length(designation) == 8 && any(designation(1) == 'PCDXAI')
    if ~isempty(regexp(designation, '^[PCDXAI][/.\-][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$', 'once'))
      info.format = 'packed';
      info.type = 'comet_bce';
      info.subtype = 'comet with BCE provisional';
      return;
    end
  end

  des = strtrim(designation);

  % Check for consecutive spaces
  if ~isempty(strfind(des, '  '))
    error('MPCDesignationError:InvalidFormat', 'Consecutive spaces in designation');
  end

  % Check for packed satellite (8 chars starting with S)
  if length(des) == 8 && des(1) == 'S'
    if ~isempty(regexp(des, '^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$', 'once'))
      info.format = 'packed';
      info.type = 'satellite';
      planet = des(5);
      planets = struct('J', 'Jupiter', 'S', 'Saturn', 'U', 'Uranus', 'N', 'Neptune');
      info.subtype = sprintf('natural satellite (%s)', planets.(planet));
      return;
    end
  end

  % Check for packed permanent (5 chars)
  if length(des) == 5
    if des(1) == '~'
      if ~isempty(regexp(des, '^~[0-9A-Za-z]{4}$', 'once'))
        info.format = 'packed';
        info.type = 'permanent';
        info.subtype = 'permanent numbered (tilde/base-62, >= 620000)';
        return;
      end
    elseif all(des >= '0' & des <= '9')
      info.format = 'packed';
      info.type = 'permanent';
      info.subtype = 'permanent numbered (5-digit, < 100000)';
      return;
    elseif ~isempty(regexp(des, '^[A-Za-z][0-9]{4}$', 'once'))
      info.format = 'packed';
      info.type = 'permanent';
      if des(1) >= 'A' && des(1) <= 'Z'
        info.subtype = 'permanent numbered (letter-prefix, 100000-359999)';
      else
        info.subtype = 'permanent numbered (letter-prefix, 360000-619999)';
      end
      return;
    end

    % Check for packed numbered comet
    if ~isempty(regexp(des, '^[0-9]{4}[PD]$', 'once'))
      info.format = 'packed';
      info.type = 'comet_numbered';
      info.subtype = 'comet numbered';
      return;
    end
  end

  % Check for packed provisional (7 chars)
  if length(des) == 7
    if des(1) == '_'
      if ~isempty(regexp(des, '^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$', 'once'))
        info.format = 'packed';
        info.type = 'provisional_extended';
        info.subtype = 'provisional (extended format, cycle >=620)';
        return;
      end
    end

    if ~isempty(regexp(des, '^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$', 'once'))
      info.format = 'packed';
      info.type = 'provisional';
      info.subtype = 'provisional';
      return;
    end

    if ~isempty(regexp(des, '^PLS[0-9]{4}$', 'once'))
      info.format = 'packed';
      info.type = 'survey';
      info.subtype = 'survey (Palomar-Leiden)';
      return;
    end

    if ~isempty(regexp(des, '^T[123]S[0-9]{4}$', 'once'))
      info.format = 'packed';
      info.type = 'survey';
      info.subtype = sprintf('survey (Trojan T-%s)', des(2));
      return;
    end

    if ~isempty(regexp(des, '^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$', 'once'))
      info.format = 'packed';
      info.type = 'comet_provisional';
      info.subtype = 'comet provisional';
      return;
    end
  end

  % --- UNPACKED FORMATS ---

  % Check for unpacked satellite
  if ~isempty(regexp(des, '^S/\d{4} [JSUN] \d+$', 'once'))
    info.format = 'unpacked';
    info.type = 'satellite';
    [tokens, ~] = regexp(des, '^S/\d{4} ([JSUN]) \d+$', 'tokens', 'match');
    planet = tokens{1}{1};
    planets = struct('J', 'Jupiter', 'S', 'Saturn', 'U', 'Uranus', 'N', 'Neptune');
    info.subtype = sprintf('natural satellite (%s)', planets.(planet));
    return;
  end

  % Check for unpacked permanent (all digits)
  if ~isempty(regexp(des, '^\d+$', 'once'))
    info.format = 'unpacked';
    info.type = 'permanent';
    info.subtype = 'permanent numbered';
    return;
  end

  % Check for unpacked survey
  if ~isempty(regexp(des, '^\d+ (P-L|T-[123])$', 'once'))
    info.format = 'unpacked';
    info.type = 'survey';
    if ~isempty(strfind(des, 'P-L'))
      info.subtype = 'survey (Palomar-Leiden)';
    else
      info.subtype = 'survey (Trojan)';
    end
    return;
  end

  % Check for old-style asteroid
  if ~isempty(regexp(des, '^[AB]\d{3} [A-Z][A-Z]$', 'once'))
    info.format = 'unpacked';
    info.type = 'provisional';
    info.subtype = 'provisional (old-style pre-1925)';
    return;
  end

  % Check for unpacked provisional asteroid
  if ~isempty(regexp(des, '^\d{4} [A-Z][A-Z]\d*$', 'once'))
    info.format = 'unpacked';
    info.type = 'provisional';
    info.subtype = 'provisional';
    return;
  end

  % Check for unpacked comet with type prefix
  if ~isempty(regexp(des, '^\d*[PCDXAI]/-?\d+ [A-Z][A-Z0-9]+(-[A-Z]{1,2})?$', 'once'))
    info.format = 'unpacked';
    info.type = 'comet_full';
    info.subtype = 'comet with provisional';
    return;
  end

  % Check for unpacked numbered periodic comet
  if ~isempty(regexp(des, '^\d+[PD](/[A-Za-z].*)?$', 'once'))
    info.format = 'unpacked';
    info.type = 'comet_numbered';
    info.subtype = 'comet numbered';
    return;
  end

  error('MPCDesignationError:UnknownFormat', 'Unable to detect designation format: %s', designation);
end

% =============================================================================
% Main conversion functions
% =============================================================================

function result = mpc_convert(designation)
  info = mpc_detect_format(designation);

  if strcmp(info.format, 'packed')
    switch info.type
      case 'permanent'
        output = num2str(mpc_unpack_permanent(designation));
      case {'provisional', 'survey'}
        output = mpc_unpack_provisional(designation);
      case 'provisional_extended'
        output = mpc_unpack_extended_provisional(designation);
      case 'comet_numbered'
        output = mpc_unpack_comet_numbered(designation);
      case 'comet_provisional'
        output = mpc_unpack_comet_provisional(designation);
      case 'comet_full'
        output = mpc_unpack_comet_full(designation);
      case {'comet_ancient', 'comet_bce'}
        output = mpc_unpack_ancient_comet(designation);
      case 'satellite'
        output = mpc_unpack_satellite(designation);
      otherwise
        error('MPCDesignationError:UnknownType', 'Unknown type: %s', info.type);
    end
  else  % unpacked
    switch info.type
      case 'permanent'
        num = str2double(strtrim(designation));
        if isnan(num) || num < 1 || num > max_asteroid_number()
          error('MPCDesignationError:InvalidNumber', 'Invalid asteroid number');
        end
        output = mpc_pack_permanent(num);
      case {'provisional', 'survey'}
        output = mpc_pack_provisional(designation);
      case 'comet_numbered'
        output = mpc_pack_comet_numbered(designation);
      case 'comet_full'
        output = mpc_pack_comet_full(designation);
      case 'satellite'
        output = mpc_pack_satellite(designation);
      otherwise
        error('MPCDesignationError:UnknownType', 'Unknown type: %s', info.type);
    end
  end

  result = struct('input', designation, 'output', output, 'info', info);
end

function output = mpc_convert_simple(designation)
  result = mpc_convert(designation);
  output = result.output;
end

function output = mpc_pack(designation)
  info = mpc_detect_format(designation);
  if strcmp(info.format, 'packed')
    output = strtrim(designation);
  else
    result = mpc_convert(designation);
    output = result.output;
  end
end

function output = mpc_unpack(designation)
  info = mpc_detect_format(designation);
  if strcmp(info.format, 'unpacked')
    output = strtrim(designation);
  else
    result = mpc_convert(designation);
    output = result.output;
  end
end

function valid = mpc_is_valid(designation)
  try
    mpc_detect_format(designation);
    valid = true;
  catch
    valid = false;
  end
end
