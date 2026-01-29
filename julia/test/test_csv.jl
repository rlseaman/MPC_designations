#!/usr/bin/env julia

# Test MPC designation conversions against CSV test data

push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using MPCDesignation

function main()
    if length(ARGS) < 1
        println(stderr, "Usage: julia test_csv.jl <prov_unpack_to_pack.csv>")
        exit(1)
    end

    csv_file = ARGS[1]
    total_tests = 0
    passed_tests = 0
    failed_tests = 0

    println("=== MPC Designation Conversion Tests (Julia) ===")
    println()

    open(csv_file) do f
        is_first_line = true
        for line in eachline(f)
            # Skip empty lines
            isempty(line) && continue

            # Skip header row
            if is_first_line
                is_first_line = false
                if startswith(line, "unpacked") || occursin("designation", line)
                    continue
                end
            end

            # Parse CSV line
            parts = split(line, ',', limit=2)
            length(parts) != 2 && continue

            unpacked = parts[1]
            expected_packed = parts[2]

            total_tests += 1

            try
                actual_packed = convert_simple(unpacked)
                if actual_packed == expected_packed
                    passed_tests += 1
                else
                    failed_tests += 1
                    if failed_tests <= 10
                        println("FAIL: pack('$unpacked')")
                        println("      Expected: '$expected_packed'")
                        println("      Got:      '$actual_packed'")
                    end
                end
            catch e
                failed_tests += 1
                if failed_tests <= 10
                    msg = e isa MPCDesignationError ? e.msg : string(e)
                    println("FAIL: pack('$unpacked')")
                    println("      Expected: '$expected_packed'")
                    println("      Got error: $msg")
                end
            end
        end
    end

    println()
    println("=== Conversion Test Results ===")
    println("Total:  $total_tests")
    println("Passed: $passed_tests")
    println("Failed: $failed_tests")

    if failed_tests > 10
        println("(Showing first 10 failures only)")
    end

    if failed_tests > 0
        exit(1)
    end
end

main()
