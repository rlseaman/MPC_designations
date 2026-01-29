#!/usr/bin/env ruby
# frozen_string_literal: true

# CLI for MPC Designation Converter

require_relative 'mpc_designation'

def print_usage
  warn 'Usage: mpc_designation_cli.rb [-v|--verbose] <designation> [designation ...]'
  warn ''
  warn 'Convert between packed and unpacked MPC designations.'
  warn 'Auto-detects the input format and converts to the other.'
  warn ''
  warn 'Options:'
  warn '  -v, --verbose   Show detailed information about the conversion'
  warn ''
  warn 'Examples:'
  warn '  mpc_designation_cli.rb 00001             -> 1'
  warn '  mpc_designation_cli.rb 1                 -> 00001'
  warn '  mpc_designation_cli.rb J95X00A           -> 1995 XA'
  warn "  mpc_designation_cli.rb '1995 XA'         -> J95X00A"
  warn "  mpc_designation_cli.rb 'C/1995 O1'       -> CJ95O010"
  warn '  mpc_designation_cli.rb 1P                -> 0001P'
end

def main
  args = ARGV.dup

  if args.empty?
    print_usage
    exit 1
  end

  verbose = false
  designations = []

  args.each do |arg|
    case arg
    when '-v', '--verbose'
      verbose = true
    when '-h', '--help'
      print_usage
      exit 0
    else
      designations << arg
    end
  end

  if designations.empty?
    print_usage
    exit 1
  end

  multiple = designations.length > 1

  designations.each do |des|
    begin
      result = MPCDesignation.convert(des)
      info = result['info']
      output = result['output']

      if verbose
        puts "  Input:    #{des}"
        puts "  Detected: #{info['format']} format, #{info['subtype']}"
        action = info['format'] == 'packed' ? 'unpacking to human-readable form' : 'packing to MPC compact form'
        puts "  Action:   #{action}"
        puts "  Output:   #{output}"
        puts if multiple
      elsif multiple
        puts "#{des} -> #{output}"
      else
        puts output
      end
    rescue MPCDesignation::Error => e
      warn "Error: #{e.message}"
      exit 1
    end
  end
end

main if __FILE__ == $PROGRAM_NAME
