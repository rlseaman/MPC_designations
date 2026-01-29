package mpc;

import java.util.ArrayList;
import java.util.List;

/**
 * Command-line interface for MPC designation converter.
 */
public class MPCDesignationCLI {

    private static void printUsage() {
        System.err.println("Usage: mpc_designation [-v|--verbose] <designation> [designation ...]");
        System.err.println();
        System.err.println("Convert between packed and unpacked MPC designations.");
        System.err.println("Auto-detects the input format and converts to the other.");
        System.err.println();
        System.err.println("Options:");
        System.err.println("  -v, --verbose   Show detailed information about the conversion");
        System.err.println("  --version       Show version information");
        System.err.println();
        System.err.println("Examples:");
        System.err.println("  mpc_designation 00001             -> 1");
        System.err.println("  mpc_designation 1                 -> 00001");
        System.err.println("  mpc_designation J95X00A           -> 1995 XA");
        System.err.println("  mpc_designation '1995 XA'         -> J95X00A");
        System.err.println("  mpc_designation 'C/1995 O1'       -> CJ95O010");
        System.err.println("  mpc_designation 1P                -> 0001P");
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            printUsage();
            System.exit(1);
        }

        boolean verbose = false;
        List<String> designations = new ArrayList<>();

        for (String arg : args) {
            switch (arg) {
                case "-v":
                case "--verbose":
                    verbose = true;
                    break;
                case "-h":
                case "--help":
                    printUsage();
                    System.exit(0);
                    break;
                case "--version":
                    System.out.println("mpc_designation " + MPCDesignation.VERSION);
                    System.exit(0);
                    break;
                default:
                    designations.add(arg);
                    break;
            }
        }

        if (designations.isEmpty()) {
            printUsage();
            System.exit(1);
        }

        boolean multiple = designations.size() > 1;

        for (String des : designations) {
            try {
                MPCDesignation.Result result = MPCDesignation.convert(des);
                MPCDesignation.Info info = result.info;

                if (verbose) {
                    System.out.println("  Input:    " + des);
                    System.out.println("  Detected: " + info.format + " format, " + info.subtype);
                    String action = info.format == MPCDesignation.FormatType.PACKED
                        ? "unpacking to human-readable form"
                        : "packing to MPC compact form";
                    System.out.println("  Action:   " + action);
                    System.out.println("  Output:   " + result.output);
                    if (multiple) {
                        System.out.println();
                    }
                } else if (multiple) {
                    System.out.println(des + " -> " + result.output);
                } else {
                    System.out.println(result.output);
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                System.err.println("Error: " + e.getMessage());
                System.exit(1);
            }
        }
    }
}
