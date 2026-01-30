#!/usr/bin/env julia

# Test MPC designation with bidirectional timing and round-trip verification

push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using MPCDesignation

struct TestCase
    unpacked::String
    packed::String
end

function main()
    if length(ARGS) < 1
        println(stderr, "Usage: julia test_roundtrip.jl <prov_unpack_to_pack.csv>")
        exit(1)
    end

    csv_file = ARGS[1]
    test_cases = TestCase[]

    # Load test cases
    open(csv_file) do f
        is_first_line = true
        for line in eachline(f)
            isempty(line) && continue

            if is_first_line
                is_first_line = false
                if startswith(line, "unpacked") || occursin("designation", line)
                    continue
                end
            end

            parts = split(line, ',', limit=2)
            length(parts) != 2 && continue

            push!(test_cases, TestCase(parts[1], parts[2]))
        end
    end

    println("Loaded $(length(test_cases)) test cases")
    println()

    pack_passed = 0
    pack_failed = 0
    unpack_passed = 0
    unpack_failed = 0
    rt_unpacked_passed = 0
    rt_unpacked_failed = 0
    rt_packed_passed = 0
    rt_packed_failed = 0

    # Phase 1: Pack (unpacked -> packed)
    println("=== Phase 1: Pack (unpacked -> packed) ===")
    start_time = time()
    for tc in test_cases
        try
            result = pack(tc.unpacked)
            if result == tc.packed
                pack_passed += 1
            else
                pack_failed += 1
            end
        catch
            pack_failed += 1
        end
    end
    elapsed = (time() - start_time) * 1000
    rate = length(test_cases) / (elapsed / 1000)
    println("Passed: $pack_passed")
    println("Failed: $pack_failed")
    println("Time:   $(round(Int, elapsed))ms ($(round(rate, digits=1)) entries/sec)")
    println()

    # Phase 2: Unpack (packed -> unpacked)
    println("=== Phase 2: Unpack (packed -> unpacked) ===")
    start_time = time()
    for tc in test_cases
        try
            result = unpack(tc.packed)
            if result == tc.unpacked
                unpack_passed += 1
            else
                unpack_failed += 1
            end
        catch
            unpack_failed += 1
        end
    end
    elapsed = (time() - start_time) * 1000
    rate = length(test_cases) / (elapsed / 1000)
    println("Passed: $unpack_passed")
    println("Failed: $unpack_failed")
    println("Time:   $(round(Int, elapsed))ms ($(round(rate, digits=1)) entries/sec)")
    println()

    # Phase 3: Unpacked round-trip: unpack(pack(x)) = x
    println("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===")
    start_time = time()
    for tc in test_cases
        try
            packed = pack(tc.unpacked)
            back = unpack(packed)
            if back == tc.unpacked
                rt_unpacked_passed += 1
            else
                rt_unpacked_failed += 1
            end
        catch
            rt_unpacked_failed += 1
        end
    end
    elapsed = (time() - start_time) * 1000
    rate = length(test_cases) / (elapsed / 1000)
    println("Passed: $rt_unpacked_passed")
    println("Failed: $rt_unpacked_failed")
    println("Time:   $(round(Int, elapsed))ms ($(round(rate, digits=1)) entries/sec)")
    println()

    # Phase 4: Packed round-trip: pack(unpack(y)) = y
    println("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===")
    start_time = time()
    for tc in test_cases
        try
            unpacked_result = unpack(tc.packed)
            back = pack(unpacked_result)
            if back == tc.packed
                rt_packed_passed += 1
            else
                rt_packed_failed += 1
            end
        catch
            rt_packed_failed += 1
        end
    end
    elapsed = (time() - start_time) * 1000
    rate = length(test_cases) / (elapsed / 1000)
    println("Passed: $rt_packed_passed")
    println("Failed: $rt_packed_failed")
    println("Time:   $(round(Int, elapsed))ms ($(round(rate, digits=1)) entries/sec)")
    println()

    # Summary
    println("=== Summary ===")
    println("Pack:       $(pack_failed == 0 ? "PASS" : "FAIL ($pack_failed)")")
    println("Unpack:     $(unpack_failed == 0 ? "PASS" : "FAIL ($unpack_failed)")")
    println("Unpacked RT: $(rt_unpacked_failed == 0 ? "PASS" : "FAIL ($rt_unpacked_failed)")")
    println("Packed RT:   $(rt_packed_failed == 0 ? "PASS" : "FAIL ($rt_packed_failed)")")

    if pack_failed > 0 || rt_packed_failed > 0
        exit(1)
    end
end

main()
