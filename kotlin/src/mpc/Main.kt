package mpc

/**
 * Command-line interface for MPC designation conversion.
 *
 * Usage: mpc_designation [options] <designation>
 *   -v, --verbose   Show detailed format information
 *   -h, --help      Show help message
 *   --version       Show version
 */
fun main(args: Array<String>) {
    if (args.isEmpty()) {
        printUsage()
        System.exit(1)
    }

    var verbose = false
    var designation: String? = null

    var i = 0
    while (i < args.size) {
        when (args[i]) {
            "-h", "--help" -> {
                printUsage()
                return
            }
            "--version" -> {
                println("mpc_designation ${MPCDesignation.VERSION}")
                return
            }
            "-v", "--verbose" -> {
                verbose = true
            }
            else -> {
                // Collect remaining args as the designation
                designation = args.slice(i until args.size).joinToString(" ")
                break
            }
        }
        i++
    }

    val des = designation
    if (des == null) {
        System.err.println("Error: No designation provided")
        printUsage()
        System.exit(1)
        return
    }

    try {
        val result = MPCDesignation.convert(des)

        if (verbose) {
            println("Input:   ${result.input}")
            println("Output:  ${result.output}")
            println("Format:  ${result.info.format?.displayName ?: "unknown"}")
            println("Type:    ${result.info.type}")
            println("Subtype: ${result.info.subtype}")
        } else {
            println(result.output)
        }
    } catch (e: MPCDesignation.MPCDesignationException) {
        System.err.println("Error: ${e.message}")
        System.exit(1)
    }
}

private fun printUsage() {
    println("Usage: mpc_designation [options] <designation>")
    println()
    println("Convert between packed and unpacked MPC designations.")
    println()
    println("Options:")
    println("  -v, --verbose   Show detailed format information")
    println("  -h, --help      Show this help message")
    println("  --version       Show version")
    println()
    println("Examples:")
    println("  mpc_designation '1995 XA'     # Convert to packed: J95X00A")
    println("  mpc_designation J95X00A       # Convert to unpacked: 1995 XA")
    println("  mpc_designation -v '1P'       # Show verbose output")
}
