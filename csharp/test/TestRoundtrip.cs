/*
 * TestRoundtrip.cs - Test MPC designation roundtrip conversion
 *
 * Usage: dotnet run --project test/TestRoundtrip.csproj <csv_file> [max_errors]
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;
using MPC;

class TestRoundtrip
{
    struct TestError
    {
        public string Test;
        public string Input;
        public string Step1;
        public string Step2;
        public string Expected;
    }

    static bool RunRoundtripTests(string csvFile, int maxErrors)
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
        var oldStyleRe = new Regex(@"^[AB]\d{3} [A-Z][A-Z]$");

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
                string packed = line.Substring(comma + 1);

                // Test packed -> unpacked -> packed roundtrip
                try
                {
                    string toUnpacked = MPCDesignation.Unpack(packed);
                    string backToPacked = MPCDesignation.Pack(toUnpacked);

                    if (backToPacked != packed)
                    {
                        failed++;
                        if (errors.Count < maxErrors)
                        {
                            errors.Add(new TestError
                            {
                                Test = "packed roundtrip",
                                Input = packed,
                                Step1 = toUnpacked,
                                Step2 = backToPacked,
                                Expected = packed
                            });
                        }
                        continue;
                    }
                }
                catch (MPCDesignationException e)
                {
                    failed++;
                    if (errors.Count < maxErrors)
                    {
                        errors.Add(new TestError
                        {
                            Test = "packed roundtrip",
                            Input = packed,
                            Step1 = "ERROR",
                            Step2 = e.Message,
                            Expected = packed
                        });
                    }
                    continue;
                }

                // Test unpacked -> packed -> unpacked roundtrip
                try
                {
                    string toPacked = MPCDesignation.Pack(unpacked);
                    string backToUnpacked = MPCDesignation.Unpack(toPacked);

                    bool isOldStyle = oldStyleRe.IsMatch(unpacked);

                    if (!isOldStyle && backToUnpacked != unpacked)
                    {
                        failed++;
                        if (errors.Count < maxErrors)
                        {
                            errors.Add(new TestError
                            {
                                Test = "unpacked roundtrip",
                                Input = unpacked,
                                Step1 = toPacked,
                                Step2 = backToUnpacked,
                                Expected = unpacked
                            });
                        }
                        continue;
                    }

                    // For old-style, verify the modern form roundtrips
                    if (isOldStyle)
                    {
                        string repackedModern = MPCDesignation.Pack(backToUnpacked);
                        if (repackedModern != toPacked)
                        {
                            failed++;
                            if (errors.Count < maxErrors)
                            {
                                errors.Add(new TestError
                                {
                                    Test = "old-style modern roundtrip",
                                    Input = unpacked,
                                    Step1 = backToUnpacked,
                                    Step2 = repackedModern,
                                    Expected = toPacked
                                });
                            }
                            continue;
                        }
                    }
                }
                catch (MPCDesignationException e)
                {
                    failed++;
                    if (errors.Count < maxErrors)
                    {
                        errors.Add(new TestError
                        {
                            Test = "unpacked roundtrip",
                            Input = unpacked,
                            Step1 = "ERROR",
                            Step2 = e.Message,
                            Expected = unpacked
                        });
                    }
                    continue;
                }

                passed++;

                if (total % 100000 == 0)
                {
                    Console.WriteLine($"Processed {total} entries...");
                }
            }
        }

        sw.Stop();
        long elapsed = sw.ElapsedMilliseconds;

        Console.WriteLine();
        Console.WriteLine("=== Roundtrip Test Results ===");
        Console.WriteLine($"Total:  {total}");
        Console.WriteLine($"Passed: {passed}");
        Console.WriteLine($"Failed: {failed}");
        Console.WriteLine($"Time:   {elapsed}ms ({total * 1000.0 / elapsed:F1} entries/sec)");
        Console.WriteLine();

        if (failed > 0)
        {
            Console.WriteLine($"=== First {errors.Count} failures ===");
            Console.WriteLine($"{"Test",-20} {"Input",-20} {"Step1",-15} {"Step2",-20} {"Expected",-15}");
            Console.WriteLine(new string('-', 90));
            foreach (var err in errors)
            {
                Console.WriteLine($"{err.Test,-20} {err.Input,-20} {err.Step1,-15} {err.Step2,-20} {err.Expected,-15}");
            }
        }

        return failed == 0;
    }

    static int Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.Error.WriteLine("Usage: TestRoundtrip <csv_file> [max_errors]");
            return 1;
        }

        string csvFile = args[0];
        int maxErrors = args.Length > 1 ? int.Parse(args[1]) : 100;

        bool success = RunRoundtripTests(csvFile, maxErrors);
        return success ? 0 : 1;
    }
}
