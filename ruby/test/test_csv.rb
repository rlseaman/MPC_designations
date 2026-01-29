#!/usr/bin/env ruby
# frozen_string_literal: true

# CSV Test Runner for MPC Designation Converter
# Tests pack direction: unpacked -> packed

require_relative '../src/mpc_designation'

def main
  if ARGV.empty?
    warn 'Usage: test_csv.rb <csv_file>'
    exit 1
  end

  csv_file = ARGV[0]

  unless File.exist?(csv_file)
    warn "File not found: #{csv_file}"
    exit 1
  end

  total = 0
  passed = 0
  failed = 0
  errors = []
  max_errors = 20

  start_time = Time.now

  File.foreach(csv_file).with_index do |line, index|
    # Skip header
    next if index.zero?

    line = line.strip
    next if line.empty?

    parts = line.split(',', 3)
    next if parts.length < 2

    unpacked = parts[0]
    expected = parts[1]
    total += 1

    begin
      result = MPCDesignation.convert_simple(unpacked)
      if result == expected
        passed += 1
      else
        failed += 1
        errors << { input: unpacked, got: result, expected: expected } if errors.length < max_errors
      end
    rescue MPCDesignation::Error => e
      failed += 1
      errors << { input: unpacked, got: "ERROR: #{e.message}", expected: expected } if errors.length < max_errors
    end

    # Progress indicator
    puts "Processed #{total} entries..." if (total % 100_000).zero?
  end

  elapsed_ms = ((Time.now - start_time) * 1000).to_i
  rate = elapsed_ms > 0 ? total.to_f / (elapsed_ms / 1000.0) : 0

  puts
  puts '=== Test Results ==='
  puts "Total:  #{total}"
  puts "Passed: #{passed}"
  puts "Failed: #{failed}"
  puts "Time:   #{elapsed_ms}ms (#{format('%.1f', rate)} entries/sec)"

  unless errors.empty?
    puts
    puts "=== First #{errors.length} errors ==="
    puts format('%-25s %-20s %-20s', 'Input', 'Got', 'Expected')
    puts format('%-25s %-20s %-20s', '-' * 25, '-' * 20, '-' * 20)
    errors.each do |e|
      puts format('%-25s %-20s %-20s', e[:input], e[:got], e[:expected])
    end
  end

  exit(failed > 0 ? 1 : 0)
end

main if __FILE__ == $PROGRAM_NAME
