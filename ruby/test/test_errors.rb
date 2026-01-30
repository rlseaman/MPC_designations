#!/usr/bin/env ruby
# frozen_string_literal: true

# Test MPC Designation error handling.
#
# Tests various classes of invalid input to ensure proper error detection.
# Reads test cases from error_test_cases.csv.
#
# Usage: ruby test_errors.rb <csv_file>

require_relative '../src/mpc_designation'

# Parse escape sequences in a string.
def unescape_string(str)
  # Handle \xNN hex escapes
  str = str.gsub(/\\x([0-9a-fA-F]{2})/) { [$1.to_i(16)].pack('C') }

  # Handle standard escapes
  str = str.gsub('\\n', "\n")
  str = str.gsub('\\r', "\r")
  str = str.gsub('\\t', "\t")
  str = str.gsub('\\f', "\f")
  str = str.gsub('\\v', "\v")
  str = str.gsub('\\0', "\0")
  str = str.gsub('\\\\', '\\')

  str
end

def run_error_tests(csv_file)
  total = 0
  passed = 0
  failed = 0

  puts '=== MPC Designation Error Tests ==='
  puts

  File.foreach(csv_file) do |line|
    line = line.strip

    # Skip empty lines and comments
    next if line.empty? || line.start_with?('#')

    # Skip header
    next if line.start_with?('category,')

    # Parse CSV (simple split)
    parts = line.split(',', 5)
    next if parts.length < 5

    category = parts[0]
    subcategory = parts[1]
    input_str = unescape_string(parts[2])
    expected_error = parts[3]
    description = parts[4]

    total += 1

    # Run the test
    got_error = false
    error_msg = ''
    output = ''

    begin
      output = MPCDesignation.convert_simple(input_str)
    rescue MPCDesignation::Error => e
      got_error = true
      error_msg = e.message
    rescue StandardError => e
      got_error = true
      error_msg = e.message
    end

    test_passed = false

    if expected_error == 'valid'
      # Expect success
      if got_error
        puts "FAIL [#{category}/#{subcategory}]: '#{description}'"
        puts "      Expected: valid conversion"
        puts "      Got:      #{error_msg}"
        failed += 1
      else
        test_passed = true
      end
    else
      # Expect error
      if got_error
        test_passed = true
      else
        puts "FAIL [#{category}/#{subcategory}]: '#{description}'"
        puts "      Expected: error (#{expected_error})"
        puts "      Got:      '#{output}' (success)"
        failed += 1
      end
    end

    passed += 1 if test_passed
  end

  puts
  puts '=== Error Test Results ==='
  puts "Total:  #{total}"
  puts "Passed: #{passed}"
  puts "Failed: #{failed}"

  failed.zero?
end

def main
  if ARGV.empty?
    warn 'Usage: ruby test_errors.rb <csv_file>'
    exit 1
  end

  csv_file = ARGV[0]

  unless File.exist?(csv_file)
    warn "Error: Cannot open file: #{csv_file}"
    exit 1
  end

  success = run_error_tests(csv_file)
  exit(success ? 0 : 1)
end

main if __FILE__ == $PROGRAM_NAME
