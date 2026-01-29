#!/usr/bin/env ruby
# frozen_string_literal: true

# Round-trip Test Runner for MPC Designation Converter
# Tests bidirectional conversion and round-trip verification

require_relative '../src/mpc_designation'

MAX_ERRORS = 20

def status_str(failed)
  failed.zero? ? 'PASS' : "FAIL (#{failed})"
end

def main
  if ARGV.empty?
    warn 'Usage: test_roundtrip.rb <csv_file>'
    exit 1
  end

  csv_file = ARGV[0]

  unless File.exist?(csv_file)
    warn "File not found: #{csv_file}"
    exit 1
  end

  # Load test data
  unpacked_list = []
  packed_list = []

  File.foreach(csv_file).with_index do |line, index|
    next if index.zero? # Skip header

    line = line.strip
    next if line.empty?

    parts = line.split(',', 3)
    next if parts.length < 2

    unpacked_list << parts[0]
    packed_list << parts[1]
  end

  total = unpacked_list.length
  puts "Loaded #{total} test cases"
  puts

  errors = []

  # ========== Phase 1: Pack (unpacked -> packed) ==========
  puts '=== Phase 1: Pack (unpacked -> packed) ==='
  pack_passed = 0
  pack_failed = 0
  start_time = Time.now

  total.times do |i|
    unpacked = unpacked_list[i]
    expected = packed_list[i]

    begin
      got = MPCDesignation.convert_simple(unpacked)
      if got == expected
        pack_passed += 1
      else
        pack_failed += 1
        errors << { phase: 'pack', input: unpacked, got: got, expected: expected } if errors.length < MAX_ERRORS
      end
    rescue MPCDesignation::Error => e
      pack_failed += 1
      errors << { phase: 'pack', input: unpacked, got: "ERROR: #{e.message}", expected: expected } if errors.length < MAX_ERRORS
    end
  end

  pack_time = ((Time.now - start_time) * 1000).to_i
  pack_rate = pack_time > 0 ? total.to_f / (pack_time / 1000.0) : 0
  puts "Passed: #{pack_passed}"
  puts "Failed: #{pack_failed}"
  puts "Time:   #{pack_time}ms (#{format('%.1f', pack_rate)} entries/sec)"
  puts

  # ========== Phase 2: Unpack (packed -> unpacked) ==========
  puts '=== Phase 2: Unpack (packed -> unpacked) ==='
  unpack_passed = 0
  unpack_failed = 0
  start_time = Time.now

  total.times do |i|
    packed = packed_list[i]
    expected = unpacked_list[i]

    begin
      got = MPCDesignation.convert_simple(packed)
      if got == expected
        unpack_passed += 1
      else
        unpack_failed += 1
        errors << { phase: 'unpack', input: packed, got: got, expected: expected } if errors.length < MAX_ERRORS
      end
    rescue MPCDesignation::Error => e
      unpack_failed += 1
      errors << { phase: 'unpack', input: packed, got: "ERROR: #{e.message}", expected: expected } if errors.length < MAX_ERRORS
    end
  end

  unpack_time = ((Time.now - start_time) * 1000).to_i
  unpack_rate = unpack_time > 0 ? total.to_f / (unpack_time / 1000.0) : 0
  puts "Passed: #{unpack_passed}"
  puts "Failed: #{unpack_failed}"
  puts "Time:   #{unpack_time}ms (#{format('%.1f', unpack_rate)} entries/sec)"
  puts

  # ========== Phase 3: Unpacked round-trip ==========
  puts '=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==='
  rt_unpacked_passed = 0
  rt_unpacked_failed = 0
  start_time = Time.now

  total.times do |i|
    original = unpacked_list[i]

    begin
      packed = MPCDesignation.convert_simple(original)
      unpacked = MPCDesignation.convert_simple(packed)
      if unpacked == original
        rt_unpacked_passed += 1
      else
        rt_unpacked_failed += 1
      end
    rescue MPCDesignation::Error
      rt_unpacked_failed += 1
    end
  end

  rt_unpacked_time = ((Time.now - start_time) * 1000).to_i
  rt_unpacked_rate = rt_unpacked_time > 0 ? total.to_f / (rt_unpacked_time / 1000.0) : 0
  puts "Passed: #{rt_unpacked_passed}"
  puts "Failed: #{rt_unpacked_failed}"
  puts "Time:   #{rt_unpacked_time}ms (#{format('%.1f', rt_unpacked_rate)} entries/sec)"
  puts

  # ========== Phase 4: Packed round-trip ==========
  puts '=== Phase 4: Packed round-trip: pack(unpack(y)) = y ==='
  errors.clear
  rt_packed_passed = 0
  rt_packed_failed = 0
  start_time = Time.now

  total.times do |i|
    original = packed_list[i]

    begin
      unpacked = MPCDesignation.convert_simple(original)
      repacked = MPCDesignation.convert_simple(unpacked)
      if repacked == original
        rt_packed_passed += 1
      else
        rt_packed_failed += 1
        if errors.length < MAX_ERRORS
          errors << { phase: 'rt-pak', input: original, got: "#{unpacked} -> #{repacked}", expected: original }
        end
      end
    rescue MPCDesignation::Error => e
      rt_packed_failed += 1
      errors << { phase: 'rt-pak', input: original, got: "ERROR: #{e.message}", expected: original } if errors.length < MAX_ERRORS
    end
  end

  rt_packed_time = ((Time.now - start_time) * 1000).to_i
  rt_packed_rate = rt_packed_time > 0 ? total.to_f / (rt_packed_time / 1000.0) : 0
  puts "Passed: #{rt_packed_passed}"
  puts "Failed: #{rt_packed_failed}"
  puts "Time:   #{rt_packed_time}ms (#{format('%.1f', rt_packed_rate)} entries/sec)"
  puts

  # ========== Summary ==========
  puts '=== Summary ==='
  puts format('%-30s %10s %18s %12s', 'Phase', 'Time (ms)', 'Rate (entries/sec)', 'Status')
  puts format('%-30s %10s %18s %12s', '-' * 30, '-' * 10, '-' * 18, '-' * 12)
  puts format('%-30s %10d %18.1f %12s', 'Pack', pack_time, pack_rate, status_str(pack_failed))
  puts format('%-30s %10d %18.1f %12s', 'Unpack', unpack_time, unpack_rate, status_str(unpack_failed))
  puts format('%-30s %10d %18.1f %12s', 'Unpacked RT: unpack(pack(x))=x', rt_unpacked_time, rt_unpacked_rate, status_str(rt_unpacked_failed))
  puts format('%-30s %10d %18.1f %12s', 'Packed RT: pack(unpack(y))=y', rt_packed_time, rt_packed_rate, status_str(rt_packed_failed))
  puts

  # Show errors
  unless errors.empty?
    puts "=== First #{errors.length} errors ==="
    puts format('%-8s %-25s %-20s %-20s', 'Phase', 'Input', 'Got', 'Expected')
    puts format('%-8s %-25s %-20s %-20s', '-' * 8, '-' * 25, '-' * 20, '-' * 20)
    errors.each do |e|
      puts format('%-8s %-25s %-20s %-20s', e[:phase], e[:input], e[:got], e[:expected])
    end
  end

  # Exit with error only if pack or packed RT failed
  total_failed = pack_failed + rt_packed_failed
  exit(total_failed > 0 ? 1 : 0)
end

main if __FILE__ == $PROGRAM_NAME
