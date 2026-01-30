/*
 * TestCsv.cs - Test MPC designation against CSV file of known conversions
 *
 * Usage: dotnet run --project test/TestCsv.csproj <csv_file> [max_errors]
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using MPC;

class TestCsv
{
    struct TestError
    {
        public string Input;
        public string Got;
        public string Expected;
    }

    static bool RunTests(string csvFile, int maxErrors)
    {
        if (!File.Exists(csvFile))
        {
            Console.Error.WriteLine($"Error: Cannot open file: {csvFile}");
            return false;
        }

        long total = 0;
        long passed = 0;
        long failed = 0;
        var errors = new List<TestError>();

        var sw = Stopwatch.StartNew();

        using (var reader = new StreamReader(csvFile))
        {
            // Skip header
            reader.ReadLine();

            string? line;
            while ((line = reader.ReadLine()) != null)
            {
                line = line.Trim();
                if (string.IsNullOrEmpty(line)) continue;

                // Skip header if encountered again
                if (line.StartsWith("unpacked")) continue;

                total++;

                int comma = line.IndexOf(',');
                if (comma < 0) continue;

                string unpacked = line.Substring(0, comma);
                string expectedPacked = line.Substring(comma + 1);

                try
                {
                    string gotPacked = MPCDesignation.ConvertSimple(unpacked);

                    if (gotPacked != expectedPacked)
                    {
                        failed++;
                        if (errors.Count < maxErrors)
                        {
                            errors.Add(new TestError { Input = unpacked, Got = gotPacked, Expected = expectedPacked });
                        }
                    }
                    else
                    {
                        passed++;
                    }
                }
                catch (MPCDesignationException e)
                {
                    failed++;
                    if (errors.Count < maxErrors)
                    {
                        errors.Add(new TestError { Input = unpacked, Got = $"ERROR: {e.Message}", Expected = expectedPacked });
                    }
                }

                if (total % 100000 == 0)
                {
                    Console.WriteLine($"Processed {total} entries...");
                }
            }
        }

        sw.Stop();
        long elapsed = sw.ElapsedMilliseconds;

        Console.WriteLine();
        Console.WriteLine("=== Test Results ===");
        Console.WriteLine($"Total:  {total}");
        Console.WriteLine($"Passed: {passed}");
        Console.WriteLine($"Failed: {failed}");
        Console.WriteLine($"Time:   {elapsed}ms ({total * 1000.0 / elapsed:F1} entries/sec)");
        Console.WriteLine();

        if (failed > 0)
        {
            Console.WriteLine($"=== First {errors.Count} failures ===");
            Console.WriteLine($"{"Input",-25} {"Got",-15} {"Expected",-15}");
            Console.WriteLine(new string('-', 60));
            foreach (var err in errors)
            {
                Console.WriteLine($"{err.Input,-25} {err.Got,-15} {err.Expected,-15}");
            }
        }

        return failed == 0;
    }

    static int Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.Error.WriteLine("Usage: TestCsv <csv_file> [max_errors]");
            return 1;
        }

        string csvFile = args[0];
        int maxErrors = args.Length > 1 ? int.Parse(args[1]) : 100;

        bool success = RunTests(csvFile, maxErrors);
        return success ? 0 : 1;
    }
}
