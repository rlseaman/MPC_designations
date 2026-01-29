#!/usr/bin/env ruby
# frozen_string_literal: true

# mpc_designation.rb - Convert between packed and unpacked MPC designations
#
# Based on Minor Planet Center specifications:
# https://www.minorplanetcenter.net/iau/info/PackedDes.html

require 'set'

module MPCDesignation
  # Base-62 character set: 0-9, A-Z, a-z
  BASE62_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

  # Century codes for provisional designations
  CENTURY_CODES = {
    'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
    'G' => 16, 'H' => 17, 'I' => 18, 'J' => 19, 'K' => 20, 'L' => 21
  }.freeze
  REVERSE_CENTURY_CODES = CENTURY_CODES.invert.freeze

  # Survey codes mapping
  SURVEY_PACKED_TO_UNPACKED = {
    'PLS' => 'P-L',
    'T1S' => 'T-1',
    'T2S' => 'T-2',
    'T3S' => 'T-3'
  }.freeze
  SURVEY_UNPACKED_TO_PACKED = SURVEY_PACKED_TO_UNPACKED.invert.freeze

  # Valid comet type prefixes
  COMET_TYPES = Set.new(%w[P C D X A I]).freeze

  # Human-readable descriptions of comet types
  COMET_TYPE_DESCRIPTIONS = {
    'P' => 'periodic',
    'C' => 'non-periodic',
    'D' => 'defunct',
    'X' => 'uncertain orbit',
    'A' => 'asteroid with comet designation',
    'I' => 'interstellar'
  }.freeze

  # Planet codes for natural satellite designations
  SATELLITE_PLANETS = Set.new(%w[J S U N]).freeze
  SATELLITE_PLANET_NAMES = {
    'J' => 'Jupiter',
    'S' => 'Saturn',
    'U' => 'Uranus',
    'N' => 'Neptune'
  }.freeze

  # Valid characters in MPC designations (after trimming)
  VALID_MPC_CHARS = Set.new('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 /-~_.'.chars).freeze

  # Maximum asteroid number: 620000 + 62^4 - 1 = 15396335
  MAX_ASTEROID_NUMBER = 15_396_335

  # Exception class for invalid MPC designations
  class Error < StandardError; end

  class << self
    # Validate raw input string BEFORE trimming
    def validate_raw_input(s)
      s.each_char do |c|
        code = c.ord
        raise Error, "Invalid character in designation: #{c.inspect}" if code < 32 || code > 126
      end
    end

    # Validate whitespace in a trimmed designation string
    def validate_whitespace(s)
      prev_space = false
      s.each_char do |c|
        code = c.ord
        raise Error, "Invalid character in designation: #{c.inspect}" if code < 32 || code > 126

        if c == ' '
          raise Error, 'Consecutive spaces in designation' if prev_space

          prev_space = true
        else
          prev_space = false
        end
      end
    end

    # Check if a letter is a valid half-month letter (A-Y excluding I)
    def valid_half_month?(letter)
      letter =~ /^[A-HJ-Y]$/
    end

    # Check if a string contains only valid MPC designation characters
    def valid_mpc_chars?(s)
      s.each_char.all? { |c| VALID_MPC_CHARS.include?(c) }
    end

    # Check if a string is a valid MPC designation
    def valid_designation?(designation)
      return false if designation.nil? || !designation.is_a?(String) || designation.empty?

      begin
        detect_format(designation)
        true
      rescue Error
        false
      end
    end

    # Sanitize a designation string for processing
    def sanitize(designation)
      raise Error, "Designation must be a string, got #{designation.class}" unless designation.is_a?(String)

      validate_raw_input(designation)
      result = designation.strip
      raise Error, 'Empty designation' if result.empty?

      result
    end

    # =============================================================================
    # Base-62 encoding utilities
    # =============================================================================

    def base62_to_num(char)
      idx = BASE62_CHARS.index(char)
      raise Error, "Invalid base-62 character: #{char}" if idx.nil?

      idx
    end

    def num_to_base62(num)
      raise Error, "Number out of base-62 range: #{num}" if num < 0 || num > 61

      BASE62_CHARS[num]
    end

    def base62_string_to_num(s)
      result = 0
      s.each_char do |char|
        result = result * 62 + base62_to_num(char)
      end
      result
    end

    def num_to_base62_string(num, width = 4)
      result = []
      width.times do
        result.unshift(num_to_base62(num % 62))
        num /= 62
      end
      result.join
    end

    # =============================================================================
    # Permanent (numbered) asteroid designations
    # =============================================================================

    def unpack_permanent(packed)
      packed = packed.strip
      length = packed.length
      first = packed[0]

      # Check for tilde format first (>= 620,000)
      if first == '~' && length == 5
        base62_part = packed[1, 4]
        return 620_000 + base62_string_to_num(base62_part)
      end

      raise Error, "Invalid packed permanent designation length: #{packed}" if length != 5

      rest = packed[1, 4]

      if first =~ /\d/
        packed.to_i
      elsif first =~ /[A-Z]/
        val = first.ord - 55 # A=10, B=11, etc.
        val * 10_000 + rest.to_i
      elsif first =~ /[a-z]/
        val = first.ord - 61 # a=36, b=37, etc.
        val * 10_000 + rest.to_i
      else
        raise Error, "Invalid packed permanent designation: #{packed}"
      end
    end

    def pack_permanent(number)
      raise Error, "Invalid asteroid number: #{number}" unless number.is_a?(Integer) && number >= 1 && number <= MAX_ASTEROID_NUMBER

      if number < 100_000
        format('%05d', number)
      elsif number < 620_000
        div = number / 10_000
        mod = number % 10_000
        letter = if div < 36
                   (div + 55).chr # A-Z for 10-35
                 else
                   (div + 61).chr # a-z for 36-61
                 end
        "#{letter}#{format('%04d', mod)}"
      else
        offset = number - 620_000
        "~#{num_to_base62_string(offset)}"
      end
    end

    # =============================================================================
    # Cycle count encoding for provisional designations
    # =============================================================================

    def decode_cycle_count(encoded)
      first = encoded[0]
      second = encoded[1]

      tens = if first =~ /\d/
               first.to_i
             elsif first =~ /[A-Z]/
               first.ord - 55
             elsif first =~ /[a-z]/
               first.ord - 61
             else
               raise Error, "Invalid cycle count encoding: #{encoded}"
             end

      raise Error, "Invalid cycle count encoding: #{encoded}" unless second =~ /\d/

      tens * 10 + second.to_i
    end

    def encode_cycle_count(count)
      raise Error, "Cycle count out of range (0-619): #{count}" if count < 0 || count >= 620

      tens = count / 10
      ones = count % 10

      first = if tens < 10
                tens.to_s
              elsif tens < 36
                (tens + 55).chr # A-Z
              else
                (tens + 61).chr # a-z
              end

      "#{first}#{ones}"
    end

    # =============================================================================
    # Letter/position utilities for provisional designations
    # =============================================================================

    def letter_to_position(letter)
      raise Error, "Invalid half-month letter: #{letter}" if letter.nil? || letter.length != 1

      pos = letter.upcase.ord - 'A'.ord + 1
      pos -= 1 if letter.upcase > 'I' # Skip I
      pos
    end

    def position_to_letter(pos)
      raise Error, "Invalid letter position: #{pos}" if pos < 1 || pos > 25

      pos += 1 if pos >= 9 # Skip I
      ('A'.ord + pos - 1).chr
    end

    def cycle_letter_to_sequence(cycle, letter)
      cycle * 25 + letter_to_position(letter)
    end

    def sequence_to_cycle_letter(sequence)
      cycle = (sequence - 1) / 25
      pos = ((sequence - 1) % 25) + 1
      [cycle, position_to_letter(pos)]
    end

    # =============================================================================
    # Extended provisional format (cycle >= 620)
    # =============================================================================

    def needs_extended_format?(cycle)
      cycle >= 620
    end

    def pack_extended_provisional(year, half_month, second_letter, cycle)
      year_short = year % 100
      base_sequence = (cycle - 620) * 25 + letter_to_position(second_letter) - 1
      seq_encoded = num_to_base62_string(base_sequence, 4)
      "_#{num_to_base62(year_short)}#{half_month}#{seq_encoded}"
    end

    def unpack_extended_provisional(packed)
      packed = packed.strip
      raise Error, "Invalid extended packed provisional: #{packed}" if packed.length != 7 || packed[0] != '_'

      year_digit = packed[1]
      half_month = packed[2]
      seq_encoded = packed[3, 4]

      base_sequence = base62_string_to_num(seq_encoded)
      cycle = 620 + base_sequence / 25
      letter_pos = (base_sequence % 25) + 1
      second_letter = position_to_letter(letter_pos)
      year = 2000 + base62_to_num(year_digit)

      "#{year} #{half_month}#{second_letter}#{cycle}"
    end

    # =============================================================================
    # Standard provisional asteroid designations
    # =============================================================================

    def unpack_provisional(packed)
      packed = packed.strip

      # Check for survey designations first
      if packed.length == 7 && SURVEY_PACKED_TO_UNPACKED.key?(packed[0, 3])
        survey = SURVEY_PACKED_TO_UNPACKED[packed[0, 3]]
        number = packed[3, 4]
        return "#{number.to_i} #{survey}"
      end

      raise Error, "Invalid packed provisional designation length: #{packed}" if packed.length != 7

      century = packed[0]
      year = packed[1, 2]
      half_month = packed[3]
      order_encoded = packed[4, 2]
      second_letter = packed[6]

      raise Error, "Invalid century code: #{century}" unless CENTURY_CODES.key?(century)

      full_year = "#{CENTURY_CODES[century]}#{year}"
      order_num = decode_cycle_count(order_encoded)

      if order_num.zero?
        "#{full_year} #{half_month}#{second_letter}"
      else
        "#{full_year} #{half_month}#{second_letter}#{order_num}"
      end
    end

    def pack_provisional(unpacked)
      unpacked = unpacked.strip

      # Check for survey designations
      if (match = unpacked.match(/^(\d+) (P-L|T-[123])$/))
        number = match[1].to_i
        survey = match[2]
        raise Error, "Survey number must be positive: #{number}" if number < 1

        return "#{SURVEY_UNPACKED_TO_PACKED[survey]}#{format('%04d', number)}"
      end

      # Check for old-style designation: "A908 CJ" or "B842 FA"
      if (match = unpacked.match(/^[AB](\d)(\d{2}) ([A-Z])([A-Z])$/))
        century_digit = match[1]
        year_short = match[2]
        half_month = match[3]
        second_letter = match[4]

        century_code = case century_digit
                       when '8' then 'I'
                       when '9' then 'J'
                       when '0' then 'K'
                       else raise Error, "Invalid century digit in old-style designation: #{century_digit}"
                       end

        return "#{century_code}#{year_short}#{half_month}00#{second_letter}"
      end

      # Match standard provisional: "1995 XA" or "1995 XA12"
      match = unpacked.match(/^(\d{4}) ([A-Z])([A-Z])(\d*)$/)
      raise Error, "Invalid unpacked provisional designation: #{unpacked}" unless match

      year = match[1]
      half_month = match[2]
      second_letter = match[3]
      order_str = match[4]

      raise Error, "Invalid half-month letter: #{half_month}" unless valid_half_month?(half_month)

      century = year[0, 2].to_i
      year_short = year[2, 2]

      raise Error, "Invalid century in year: #{year}" unless REVERSE_CENTURY_CODES.key?(century)

      century_code = REVERSE_CENTURY_CODES[century]
      order_num = order_str.empty? ? 0 : order_str.to_i

      if needs_extended_format?(order_num)
        return pack_extended_provisional(year.to_i, half_month, second_letter, order_num)
      end

      order_encoded = encode_cycle_count(order_num)
      "#{century_code}#{year_short}#{half_month}#{order_encoded}#{second_letter}"
    end

    # =============================================================================
    # Comet provisional designations
    # =============================================================================

    def unpack_comet_provisional(packed)
      packed = packed.strip
      length = packed.length

      raise Error, "Invalid packed comet provisional designation length: #{packed}" unless [7, 8].include?(length)

      century = packed[0]
      year = packed[1, 2]
      half_month = packed[3]
      order_encoded = packed[4, 2]
      fragment = length == 7 ? packed[6] : packed[6, 2]

      raise Error, "Invalid century code: #{century}" unless CENTURY_CODES.key?(century)

      full_year = "#{CENTURY_CODES[century]}#{year}"
      order_num = decode_cycle_count(order_encoded)

      result = "#{full_year} #{half_month}#{order_num}"
      result += "-#{fragment.upcase}" if fragment != '0'

      result
    end

    def pack_comet_provisional(unpacked)
      unpacked = unpacked.strip

      match = unpacked.match(/^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$/)
      raise Error, "Invalid unpacked comet provisional designation: #{unpacked}" unless match

      year = match[1]
      half_month = match[2]
      order_num = match[3].to_i
      fragment = match[4]

      raise Error, "Comet order number must be positive: #{order_num}" if order_num < 1

      century = year[0, 2].to_i
      year_short = year[2, 2]

      raise Error, "Invalid century in year: #{year}" unless REVERSE_CENTURY_CODES.key?(century)

      century_code = REVERSE_CENTURY_CODES[century]
      order_encoded = encode_cycle_count(order_num)
      fragment_code = fragment.nil? ? '0' : fragment.downcase

      "#{century_code}#{year_short}#{half_month}#{order_encoded}#{fragment_code}"
    end

    # =============================================================================
    # Numbered comet designations
    # =============================================================================

    def unpack_comet_numbered(packed)
      packed = packed.strip

      match = packed.match(/^(\d{4})([PD])$/)
      raise Error, "Invalid packed numbered comet designation: #{packed}" unless match

      number = match[1].to_i
      comet_type = match[2]
      "#{number}#{comet_type}"
    end

    def pack_comet_numbered(unpacked)
      unpacked = unpacked.strip

      match = unpacked.match(/^(\d+)([PD])(?:\/[A-Za-z].*)?$/)
      raise Error, "Invalid unpacked numbered comet designation: #{unpacked}" unless match

      number = match[1].to_i
      comet_type = match[2]

      raise Error, "Comet number out of range (1-9999): #{number}" if number < 1 || number > 9999

      "#{format('%04d', number)}#{comet_type}"
    end

    # =============================================================================
    # Natural satellite designations
    # =============================================================================

    def unpack_satellite(packed)
      packed = packed.strip

      raise Error, "Invalid packed satellite designation: #{packed}" if packed.length != 8 || packed[0] != 'S'

      century = packed[1]
      year = packed[2, 2]
      planet = packed[4]
      number_encoded = packed[5, 2]

      raise Error, "Invalid century code: #{century}" unless CENTURY_CODES.key?(century)
      raise Error, "Invalid planet code: #{planet}" unless SATELLITE_PLANETS.include?(planet)

      full_year = "#{CENTURY_CODES[century]}#{year}"
      number = decode_cycle_count(number_encoded)

      "S/#{full_year} #{planet} #{number}"
    end

    def pack_satellite(unpacked)
      unpacked = unpacked.strip

      match = unpacked.match(/^S\/(\d{4}) ([JSUN]) (\d+)$/)
      raise Error, "Invalid unpacked satellite designation: #{unpacked}" unless match

      year = match[1]
      planet = match[2]
      number = match[3].to_i

      raise Error, "Satellite number must be positive: #{number}" if number < 1

      century = year[0, 2].to_i
      year_short = year[2, 2]

      raise Error, "Invalid century in year: #{year}" unless REVERSE_CENTURY_CODES.key?(century)

      century_code = REVERSE_CENTURY_CODES[century]
      number_encoded = encode_cycle_count(number)

      "S#{century_code}#{year_short}#{planet}#{number_encoded}0"
    end

    # =============================================================================
    # BCE year encoding for ancient comets
    # =============================================================================

    def encode_bce_year(year)
      raise Error, "Not a BCE year: #{year}" if year >= 0

      abs_year = year.abs
      code = 99 - (abs_year % 100)

      prefix = if abs_year < 100
                 '/'
               elsif abs_year < 200
                 '.'
               elsif abs_year < 300
                 '-'
               else
                 raise Error, "BCE year out of supported range: #{year}"
               end

      [prefix, format('%02d', code)]
    end

    def decode_bce_year(prefix, code)
      code_num = code.to_i
      year_part = 99 - code_num

      case prefix
      when '/' then -year_part
      when '.' then -(year_part + 100)
      when '-' then -(year_part + 200)
      else raise Error, "Invalid BCE prefix: #{prefix}"
      end
    end

    def ancient_year?(year)
      year < 1000
    end

    # =============================================================================
    # Ancient/BCE comet provisional designations
    # =============================================================================

    def pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment = '')
      order_encoded = encode_cycle_count(order_num)
      fragment_code = fragment.empty? ? '0' : fragment.downcase

      if year < 0
        prefix, code = encode_bce_year(year)
        "#{comet_type}#{prefix}#{code}#{half_month}#{order_encoded}#{fragment_code}"
      else
        "#{comet_type}#{format('%03d', year)}#{half_month}#{order_encoded}#{fragment_code}"
      end
    end

    def unpack_ancient_comet_provisional(packed)
      packed = packed.strip

      raise Error, "Invalid ancient comet designation length: #{packed}" if packed.length != 8

      comet_type = packed[0]
      raise Error, "Invalid comet type: #{comet_type}" unless COMET_TYPES.include?(comet_type)

      if %w[/ . -].include?(packed[1])
        prefix = packed[1]
        year_code = packed[2, 2]
        year = decode_bce_year(prefix, year_code)
        half_month = packed[4]
        order_encoded = packed[5, 2]
        fragment = packed[7]
      else
        year = packed[1, 3].to_i
        half_month = packed[4]
        order_encoded = packed[5, 2]
        fragment = packed[7]
      end

      order_num = decode_cycle_count(order_encoded)
      result = "#{comet_type}/#{year} #{half_month}#{order_num}"
      result += "-#{fragment.upcase}" if fragment != '0'

      result
    end

    # =============================================================================
    # Helper functions for comet format detection
    # =============================================================================

    def asteroid_style_packed?(provisional_part)
      return false if provisional_part.length != 7

      provisional_part[6] =~ /[A-Z]/
    end

    def asteroid_style_unpacked?(provisional)
      match = provisional.match(/^\d{4} ([A-Z])(.)/)
      return false unless match

      match[2] =~ /[A-Za-z]/
    end

    # =============================================================================
    # Full comet designations (with type prefix)
    # =============================================================================

    def unpack_comet_full(packed)
      length = packed.length

      if length == 8
        comet_type = packed[0]
        provisional_part = packed[1, 7]
        num_str = ''
      elsif length == 9
        comet_type = packed[0]
        provisional_part = packed[1, 8]
        num_str = ''
      elsif length == 12 || (length < 12 && packed[0] == ' ')
        packed = packed.rjust(12)
        num_part = packed[0, 4]
        comet_type = packed[4]
        provisional_part = packed[5, 7]
        num_str = num_part.strip
      else
        raise Error, "Invalid packed full comet designation length: #{packed}"
      end

      raise Error, "Invalid comet type: #{comet_type}" unless COMET_TYPES.include?(comet_type)

      provisional = if asteroid_style_packed?(provisional_part)
                      unpack_provisional(provisional_part)
                    else
                      unpack_comet_provisional(provisional_part)
                    end

      if num_str.empty?
        "#{comet_type}/#{provisional}"
      else
        number = num_str.to_i
        "#{number}#{comet_type}/#{provisional}"
      end
    end

    def pack_comet_full(unpacked)
      unpacked = unpacked.strip

      match = unpacked.match(/^(\d*)([PCDXAI])\/(-?\d+) (.+)$/)
      raise Error, "Invalid unpacked comet designation: #{unpacked}" unless match

      number = match[1]
      comet_type = match[2]
      year = match[3].to_i
      prov_part = match[4]

      raise Error, "Invalid comet type: #{comet_type}" unless COMET_TYPES.include?(comet_type)

      if ancient_year?(year)
        ancient_match = prov_part.match(/^([A-Z])(\d+)(?:-([A-Z]))?$/)
        if ancient_match
          half_month = ancient_match[1]
          order_num = ancient_match[2].to_i
          fragment = ancient_match[3] || ''
          return pack_ancient_comet_provisional(comet_type, year, half_month, order_num, fragment)
        else
          raise Error, "Invalid ancient comet provisional: #{prov_part}"
        end
      end

      provisional = "#{year} #{prov_part}"

      provisional_packed = if asteroid_style_unpacked?(provisional)
                             pack_provisional(provisional)
                           else
                             pack_comet_provisional(provisional)
                           end

      if number.empty?
        "#{comet_type}#{provisional_packed}"
      else
        num = number.to_i
        raise Error, "Comet number out of range (1-9999): #{num}" if num < 1 || num > 9999

        "#{format('%04d', num)}#{comet_type}#{provisional_packed}"
      end
    end

    # =============================================================================
    # Format detection
    # =============================================================================

    def detect_format(designation)
      result = { 'format' => '', 'type' => '', 'subtype' => '' }

      validate_raw_input(designation)

      # Check for packed full comet designation BEFORE trimming (12 chars with spaces)
      if designation.length == 12
        if designation.match(/^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$/)
          result['format'] = 'packed'
          result['type'] = 'comet_full'
          result['subtype'] = 'comet with provisional designation (12-char)'
          return result
        end
      end

      # Check for packed comet designation (8 chars)
      if designation.length == 8
        if designation.match(/^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$/)
          result['format'] = 'packed'
          result['type'] = 'comet_full'
          result['subtype'] = 'comet with provisional designation (8-char)'
          return result
        end
      end

      # Check for packed comet with 2-letter fragment (9 chars)
      if designation.length == 9
        if designation.match(/^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$/)
          result['format'] = 'packed'
          result['type'] = 'comet_full'
          result['subtype'] = 'comet with provisional designation (9-char, 2-letter fragment)'
          return result
        end
      end

      # Check for packed ancient comet (8 chars)
      if designation.length == 8
        if designation.match(/^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/)
          result['format'] = 'packed'
          result['type'] = 'comet_ancient'
          result['subtype'] = 'comet with ancient provisional (year < 1000)'
          return result
        end
      end

      # Check for packed BCE comet (8 chars)
      if designation.length == 8
        if designation.match(/^([PCDXAI])([\/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/)
          result['format'] = 'packed'
          result['type'] = 'comet_bce'
          result['subtype'] = 'comet with BCE provisional'
          return result
        end
      end

      des = designation.strip
      validate_whitespace(des)

      # Check for packed satellite designation (8 chars starting with S)
      if des.length == 8 && des[0] == 'S'
        if des.match(/^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$/)
          result['format'] = 'packed'
          result['type'] = 'satellite'
          planet = des[4]
          planet_name = SATELLITE_PLANET_NAMES[planet] || planet
          result['subtype'] = "natural satellite (#{planet_name})"
          return result
        end
      end

      # Check for packed permanent (numbered) asteroid
      if des.length == 5
        if des[0] == '~'
          if des.match(/^~[0-9A-Za-z]{4}$/)
            result['format'] = 'packed'
            result['type'] = 'permanent'
            result['subtype'] = 'permanent numbered (tilde/base-62, >= 620000)'
            return result
          end
        elsif des.match(/^\d{5}$/)
          result['format'] = 'packed'
          result['type'] = 'permanent'
          result['subtype'] = 'permanent numbered (5-digit, < 100000)'
          return result
        elsif des.match(/^[A-Za-z][0-9]{4}$/)
          result['format'] = 'packed'
          result['type'] = 'permanent'
          result['subtype'] = if des[0] =~ /[A-Z]/
                                'permanent numbered (letter-prefix, 100000-359999)'
                              else
                                'permanent numbered (letter-prefix, 360000-619999)'
                              end
          return result
        end
      end

      # Check for packed provisional asteroid (7 chars)
      if des.length == 7
        if des[0] == '_'
          if des.match(/^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$/)
            result['format'] = 'packed'
            result['type'] = 'provisional_extended'
            result['subtype'] = 'provisional (extended format, cycle >=620)'
            return result
          end
        end
        if des.match(/^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$/)
          result['format'] = 'packed'
          result['type'] = 'provisional'
          result['subtype'] = 'provisional'
          return result
        end
        if des.start_with?('PLS') && des[3, 4].match(/^\d{4}$/)
          result['format'] = 'packed'
          result['type'] = 'survey'
          result['subtype'] = 'survey (Palomar-Leiden)'
          return result
        end
        if des.match(/^T[123]S\d{4}$/)
          result['format'] = 'packed'
          result['type'] = 'survey'
          result['subtype'] = "survey (Trojan T-#{des[1]})"
          return result
        end
      end

      # Check for packed numbered comet (5 chars ending in P or D)
      if des.length == 5
        if des.match(/^[0-9]{4}[PD]$/)
          result['format'] = 'packed'
          result['type'] = 'comet_numbered'
          comet_type = des[4]
          type_desc = COMET_TYPE_DESCRIPTIONS[comet_type] || comet_type
          result['subtype'] = "comet numbered #{type_desc}"
          return result
        end
      end

      # Check for packed comet provisional (7 chars starting with century code)
      if des.length == 7
        if des.match(/^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$/)
          result['format'] = 'packed'
          result['type'] = 'comet_provisional'
          result['subtype'] = 'comet provisional'
          return result
        end
      end

      # --- UNPACKED FORMATS ---

      # Check for unpacked satellite: "S/2019 S 22"
      if (match = des.match(/^S\/\d{4} ([JSUN]) \d+$/))
        result['format'] = 'unpacked'
        result['type'] = 'satellite'
        planet = match[1]
        planet_name = SATELLITE_PLANET_NAMES[planet] || planet
        result['subtype'] = "natural satellite (#{planet_name})"
        return result
      end

      # Check for unpacked permanent (numbered) asteroid
      if des.match(/^\d+$/)
        result['format'] = 'unpacked'
        result['type'] = 'permanent'
        result['subtype'] = 'permanent numbered'
        return result
      end

      # Check for unpacked survey designation
      if (match = des.match(/^\d+ (P-L|T-[123])$/))
        result['format'] = 'unpacked'
        result['type'] = 'survey'
        survey = match[1]
        result['subtype'] = if survey == 'P-L'
                              'survey (Palomar-Leiden)'
                            else
                              "survey (Trojan #{survey})"
                            end
        return result
      end

      # Check for old-style asteroid designation
      if des.match(/^[AB]\d{3} [A-Z][A-Z]$/)
        result['format'] = 'unpacked'
        result['type'] = 'provisional'
        result['subtype'] = 'provisional (old-style pre-1925)'
        return result
      end

      # Check for unpacked provisional asteroid
      if des.match(/^\d{4} [A-Z][A-Z]\d*$/)
        result['format'] = 'unpacked'
        result['type'] = 'provisional'
        result['subtype'] = 'provisional'
        return result
      end

      # Check for unpacked comet with type prefix
      if (match = des.match(/^(\d*)([PCDXAI])\/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$/))
        num = match[1]
        ctype = match[2]
        year = match[3].to_i

        year_desc = if year < 0
                      'BCE'
                    elsif year < 1000
                      'ancient'
                    else
                      ''
                    end

        result['format'] = 'unpacked'
        result['type'] = 'comet_full'

        type_desc = COMET_TYPE_DESCRIPTIONS[ctype] || ctype
        result['subtype'] = if !num.empty?
                              year_desc.empty? ? "comet numbered with provisional (#{type_desc})" : "comet numbered with #{year_desc} provisional (#{type_desc})"
                            else
                              year_desc.empty? ? "comet provisional (#{type_desc})" : "comet #{year_desc} provisional (#{type_desc})"
                            end
        return result
      end

      # Check for unpacked numbered periodic comet
      if (match = des.match(/^(\d+)([PD])(?:\/[A-Za-z].*)?$/))
        result['format'] = 'unpacked'
        result['type'] = 'comet_numbered'
        comet_type = match[2]
        type_desc = COMET_TYPE_DESCRIPTIONS[comet_type] || comet_type
        result['subtype'] = "comet numbered #{type_desc}"
        return result
      end

      raise Error, "Unable to detect designation format: #{designation}"
    end

    # =============================================================================
    # Main conversion function
    # =============================================================================

    def convert(designation)
      info = detect_format(designation)
      fmt = info['format']
      dtype = info['type']

      result = ''

      if fmt == 'packed'
        case dtype
        when 'permanent'
          result = unpack_permanent(designation).to_s
        when 'provisional', 'survey'
          result = unpack_provisional(designation)
        when 'provisional_extended'
          result = unpack_extended_provisional(designation)
        when 'comet_numbered'
          result = unpack_comet_numbered(designation)
        when 'comet_provisional'
          result = unpack_comet_provisional(designation)
        when 'comet_full'
          result = unpack_comet_full(designation)
        when 'comet_ancient', 'comet_bce'
          result = unpack_ancient_comet_provisional(designation)
        when 'satellite'
          result = unpack_satellite(designation)
        end
      else # unpacked
        case dtype
        when 'permanent'
          result = pack_permanent(designation.to_i)
        when 'provisional', 'survey'
          result = pack_provisional(designation)
        when 'comet_numbered'
          result = pack_comet_numbered(designation)
        when 'comet_full'
          result = pack_comet_full(designation)
        when 'satellite'
          result = pack_satellite(designation)
        end
      end

      {
        'input' => designation,
        'output' => result,
        'info' => info
      }
    end

    def convert_simple(designation)
      convert(designation)['output']
    end

    # =============================================================================
    # High-level pack/unpack functions
    # =============================================================================

    def pack(designation)
      info = detect_format(designation)
      return designation.strip if info['format'] == 'packed'

      convert(designation)['output']
    end

    def unpack(designation)
      info = detect_format(designation)
      return designation.strip if info['format'] == 'unpacked'

      convert(designation)['output']
    end
  end
end
