#!/usr/bin/env julia

# CLI for MPC designation converter

# Add the src directory to the load path
push!(LOAD_PATH, @__DIR__)

using MPCDesignation

function print_usage()
    println(stderr, """
Usage: julia mpc_designation_cli.jl [-v|--verbose] <designation> [designation ...]

Convert between packed and unpacked MPC designations.
Auto-detects the input format and converts to the other.

Options:
  -v, --verbose   Show detailed information about the conversion
  --version       Show version information

Examples:
  julia mpc_designation_cli.jl 00001             -> 1
  julia mpc_designation_cli.jl 1                 -> 00001
  julia mpc_designation_cli.jl J95X00A           -> 1995 XA
  julia mpc_designation_cli.jl '1995 XA'         -> J95X00A
  julia mpc_designation_cli.jl 'C/1995 O1'       -> CJ95O010
  julia mpc_designation_cli.jl 1P                -> 0001P""")
end

function main()
    args = ARGS

    if isempty(args)
        print_usage()
        exit(1)
    end

    verbose = false
    designations = String[]

    for arg in args
        if arg in ["-v", "--verbose"]
            verbose = true
        elseif arg in ["-h", "--help"]
            print_usage()
            exit(0)
        elseif arg == "--version"
            println("mpc_designation $(MPCDesignation.VERSION)")
            exit(0)
        else
            push!(designations, arg)
        end
    end

    if isempty(designations)
        print_usage()
        exit(1)
    end

    multiple = length(designations) > 1

    for des in designations
        try
            result = convert_designation(des)
            info = result.info

            if verbose
                println("  Input:    $des")
                println("  Detected: $(info.format) format, $(info.subtype)")
                action = info.format == :packed ? "unpacking to human-readable form" : "packing to MPC compact form"
                println("  Action:   $action")
                println("  Output:   $(result.output)")
                if multiple
                    println()
                end
            elseif multiple
                println("$des -> $(result.output)")
            else
                println(result.output)
            end
        catch e
            if e isa MPCDesignationError
                println(stderr, "Error: $(e.msg)")
            else
                println(stderr, "Error: $e")
            end
            exit(1)
        end
    end
end

main()
