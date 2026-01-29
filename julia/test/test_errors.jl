#!/usr/bin/env julia

# Test MPC designation error handling

push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using MPCDesignation

function unescape_input(input::AbstractString)
    result = input
    result = replace(result, "\\t" => "\t")
    result = replace(result, "\\n" => "\n")
    result = replace(result, "\\r" => "\r")
    result = replace(result, "\\f" => "\f")
    result = replace(result, "\\v" => "\v")

    # Handle \x00 style escapes
    while true
        m = match(r"\\x([0-9a-fA-F]{2})", result)
        m === nothing && break
        hex_str = m.captures[1]
        char_val = parse(Int, hex_str, base=16)
        result = replace(result, m.match => string(Char(char_val)), count=1)
    end

    return result
end

function main()
    if length(ARGS) < 1
        println(stderr, "Usage: julia test_errors.jl <error_test_cases.csv>")
        exit(1)
    end

    csv_file = ARGS[1]
    total_tests = 0
    passed_tests = 0
    failed_tests = 0

    println("=== MPC Designation Error Tests (Julia) ===")
    println()

    open(csv_file) do f
        for line in eachline(f)
            # Skip empty lines, comments, and header
            isempty(line) && continue
            startswith(line, "#") && continue
            startswith(line, "category,") && continue

            # Parse CSV line
            parts = split(line, ',', limit=5)
            length(parts) < 5 && continue

            category = parts[1]
            subcategory = parts[2]
            raw_input = parts[3]
            expected_error = parts[4]
            description = parts[5]

            input = unescape_input(raw_input)
            total_tests += 1

            got_error = false
            error_type = ""
            result_output = ""

            try
                result = convert_designation(input)
                result_output = result.output
            catch e
                got_error = true
                if e isa MPCDesignationError
                    err_str = e.msg
                    if occursin("out of range", err_str) || occursin("Invalid asteroid number", err_str)
                        error_type = "range"
                    else
                        error_type = "format"
                    end
                else
                    error_type = "format"
                end
            end

            test_id = "$category/$subcategory"

            if expected_error == "valid"
                # Should succeed
                if got_error
                    failed_tests += 1
                    println("FAIL [$test_id]: '$description'")
                    println("      Expected: valid conversion")
                    println("      Got:      error ($error_type)")
                else
                    passed_tests += 1
                end
            else
                # Should fail
                if !got_error
                    failed_tests += 1
                    println("FAIL [$test_id]: '$description'")
                    println("      Expected: error ($expected_error)")
                    println("      Got:      '$result_output' (success)")
                else
                    passed_tests += 1
                end
            end
        end
    end

    println()
    println("=== Error Test Results ===")
    println("Total:  $total_tests")
    println("Passed: $passed_tests")
    println("Failed: $failed_tests")

    if failed_tests > 0
        exit(1)
    end
end

main()
