/*
 * TestRoundtrip.cs - Test MPC designation with bidirectional timing and round-trip verification
 *
 * Usage: dotnet run --project test/TestRoundtrip.csproj <csv_file>
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using MPC;

class TestRoundtrip
{
    static int Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.Error.WriteLine("Usage: TestRoundtrip <csv_file>");
            return 1;
        }

        string csvFile = args[0];
        if (!File.Exists(csvFile))
        {
            Console.Error.WriteLine($"Error: Cannot open file: {csvFile}");
            return 1;
        }

        // Load test cases
        var testCases = new List<(string unpacked, string packed)>();
        using (var reader = new StreamReader(csvFile))
        {
            reader.ReadLine(); // Skip header
            string? line;
            while ((line = reader.ReadLine()) != null)
            {
                line = line.Trim();
                if (string.IsNullOrEmpty(line)) continue;
                if (line.StartsWith("unpacked")) continue;

                int comma = line.IndexOf(',');
                if (comma < 0) continue;

                testCases.Add((line.Substring(0, comma), line.Substring(comma + 1)));
            }
        }

        Console.WriteLine($"Loaded {testCases.Count} test cases");
        Console.WriteLine();

        // Phase 1: Pack (unpacked -> packed)
        Console.WriteLine("=== Phase 1: Pack (unpacked -> packed) ===");
        int packPassed = 0;
        int packFailed = 0;
        var sw = Stopwatch.StartNew();

        foreach (var (unpacked, packed) in testCases)
        {
            try
            {
                string result = MPCDesignation.Pack(unpacked);
                if (result == packed)
                    packPassed++;
                else
                    packFailed++;
            }
            catch (MPCDesignationException)
            {
                packFailed++;
            }
        }

        sw.Stop();
        long packElapsed = sw.ElapsedMilliseconds;
        double packRate = testCases.Count * 1000.0 / packElapsed;
        Console.WriteLine($"Passed: {packPassed}");
        Console.WriteLine($"Failed: {packFailed}");
        Console.WriteLine($"Time:   {packElapsed}ms ({packRate:F1} entries/sec)");
        Console.WriteLine();

        // Phase 2: Unpack (packed -> unpacked)
        Console.WriteLine("=== Phase 2: Unpack (packed -> unpacked) ===");
        int unpackPassed = 0;
        int unpackFailed = 0;
        sw.Restart();

        foreach (var (unpacked, packed) in testCases)
        {
            try
            {
                string result = MPCDesignation.Unpack(packed);
                if (result == unpacked)
                    unpackPassed++;
                else
                    unpackFailed++;
            }
            catch (MPCDesignationException)
            {
                unpackFailed++;
            }
        }

        sw.Stop();
        long unpackElapsed = sw.ElapsedMilliseconds;
        double unpackRate = testCases.Count * 1000.0 / unpackElapsed;
        Console.WriteLine($"Passed: {unpackPassed}");
        Console.WriteLine($"Failed: {unpackFailed}");
        Console.WriteLine($"Time:   {unpackElapsed}ms ({unpackRate:F1} entries/sec)");
        Console.WriteLine();

        // Phase 3: Unpacked round-trip: unpack(pack(x)) = x
        Console.WriteLine("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===");
        int rtUnpackedPassed = 0;
        int rtUnpackedFailed = 0;
        sw.Restart();

        foreach (var (unpacked, packed) in testCases)
        {
            try
            {
                string packedResult = MPCDesignation.Pack(unpacked);
                string back = MPCDesignation.Unpack(packedResult);
                if (back == unpacked)
                    rtUnpackedPassed++;
                else
                    rtUnpackedFailed++;
            }
            catch (MPCDesignationException)
            {
                rtUnpackedFailed++;
            }
        }

        sw.Stop();
        long rtUnpackedElapsed = sw.ElapsedMilliseconds;
        double rtUnpackedRate = testCases.Count * 1000.0 / rtUnpackedElapsed;
        Console.WriteLine($"Passed: {rtUnpackedPassed}");
        Console.WriteLine($"Failed: {rtUnpackedFailed}");
        Console.WriteLine($"Time:   {rtUnpackedElapsed}ms ({rtUnpackedRate:F1} entries/sec)");
        Console.WriteLine();

        // Phase 4: Packed round-trip: pack(unpack(y)) = y
        Console.WriteLine("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===");
        int rtPackedPassed = 0;
        int rtPackedFailed = 0;
        sw.Restart();

        foreach (var (unpacked, packed) in testCases)
        {
            try
            {
                string unpackedResult = MPCDesignation.Unpack(packed);
                string back = MPCDesignation.Pack(unpackedResult);
                if (back == packed)
                    rtPackedPassed++;
                else
                    rtPackedFailed++;
            }
            catch (MPCDesignationException)
            {
                rtPackedFailed++;
            }
        }

        sw.Stop();
        long rtPackedElapsed = sw.ElapsedMilliseconds;
        double rtPackedRate = testCases.Count * 1000.0 / rtPackedElapsed;
        Console.WriteLine($"Passed: {rtPackedPassed}");
        Console.WriteLine($"Failed: {rtPackedFailed}");
        Console.WriteLine($"Time:   {rtPackedElapsed}ms ({rtPackedRate:F1} entries/sec)");
        Console.WriteLine();

        // Summary
        Console.WriteLine("=== Summary ===");
        Console.WriteLine($"Pack:       {(packFailed == 0 ? "PASS" : $"FAIL ({packFailed})")}");
        Console.WriteLine($"Unpack:     {(unpackFailed == 0 ? "PASS" : $"FAIL ({unpackFailed})")}");
        Console.WriteLine($"Unpacked RT: {(rtUnpackedFailed == 0 ? "PASS" : $"FAIL ({rtUnpackedFailed})")}");
        Console.WriteLine($"Packed RT:   {(rtPackedFailed == 0 ? "PASS" : $"FAIL ({rtPackedFailed})")}");

        return (packFailed > 0 || rtPackedFailed > 0) ? 1 : 0;
    }
}
