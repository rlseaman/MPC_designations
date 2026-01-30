/*
 * Program.cs - CLI for MPC designation converter
 */

using System;
using System.Collections.Generic;
using MPC;

class Program
{
    static void PrintUsage()
    {
        Console.Error.WriteLine(@"Usage: mpc_designation [-v|--verbose] <designation> [designation ...]

Convert between packed and unpacked MPC designations.
Auto-detects the input format and converts to the other.

Options:
  -v, --verbose   Show detailed information about the conversion
  --version       Show version information

Examples:
  mpc_designation 00001             -> 1
  mpc_designation 1                 -> 00001
  mpc_designation J95X00A           -> 1995 XA
  mpc_designation '1995 XA'         -> J95X00A
  mpc_designation 'C/1995 O1'       -> CJ95O010
  mpc_designation 1P                -> 0001P");
    }

    static int Main(string[] args)
    {
        if (args.Length == 0)
        {
            PrintUsage();
            return 1;
        }

        bool verbose = false;
        var designations = new List<string>();

        foreach (string arg in args)
        {
            switch (arg)
            {
                case "-v":
                case "--verbose":
                    verbose = true;
                    break;
                case "-h":
                case "--help":
                    PrintUsage();
                    return 0;
                case "--version":
                    Console.WriteLine($"mpc_designation {MPCDesignation.VERSION}");
                    return 0;
                default:
                    designations.Add(arg);
                    break;
            }
        }

        if (designations.Count == 0)
        {
            PrintUsage();
            return 1;
        }

        bool multiple = designations.Count > 1;

        foreach (string des in designations)
        {
            try
            {
                var result = MPCDesignation.Convert(des);

                if (verbose)
                {
                    Console.WriteLine($"  Input:    {des}");
                    string format = result.Info.Format == Format.Packed ? "packed" : "unpacked";
                    Console.WriteLine($"  Detected: {format} format, {result.Info.Subtype}");
                    string action = result.Info.Format == Format.Packed
                        ? "unpacking to human-readable form"
                        : "packing to MPC compact form";
                    Console.WriteLine($"  Action:   {action}");
                    Console.WriteLine($"  Output:   {result.Output}");
                    if (multiple)
                    {
                        Console.WriteLine();
                    }
                }
                else if (multiple)
                {
                    Console.WriteLine($"{des} -> {result.Output}");
                }
                else
                {
                    Console.WriteLine(result.Output);
                }
            }
            catch (MPCDesignationException e)
            {
                Console.Error.WriteLine($"Error: {e.Message}");
                return 1;
            }
        }

        return 0;
    }
}
