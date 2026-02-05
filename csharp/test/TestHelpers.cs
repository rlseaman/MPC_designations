/*
 * TestHelpers.cs - Test MPC designation helper functions
 *
 * Tests format conversion (minimal <-> 12-char report format),
 * fragment extraction, and designation comparison functions.
 *
 * Usage: dotnet run --project test/TestHelpers.csproj
 */

using System;
using MPC;

class TestHelpers
{
    static int passed = 0;
    static int failed = 0;

    static void TestToReport(string input, string expected, string desc)
    {
        try
        {
            string output = MPCDesignation.ToReportFormat(input);
            if (output == expected)
            {
                Console.WriteLine($"  PASS: ToReport(\"{input}\") -> \"{output}\"");
                passed++;
            }
            else
            {
                Console.WriteLine($"  FAIL: ToReport(\"{input}\"): expected \"{expected}\", got \"{output}\"");
                failed++;
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"  FAIL: ToReport(\"{input}\"): error {e.Message}");
            failed++;
        }
    }

    static void TestFromReport(string input, string expected, string desc)
    {
        try
        {
            string output = MPCDesignation.FromReportFormat(input);
            if (output == expected)
            {
                Console.WriteLine($"  PASS: FromReport(\"{input}\") -> \"{output}\"");
                passed++;
            }
            else
            {
                Console.WriteLine($"  FAIL: FromReport(\"{input}\"): expected \"{expected}\", got \"{output}\"");
                failed++;
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"  FAIL: FromReport(\"{input}\"): error {e.Message}");
            failed++;
        }
    }

    static void TestHasFragment(string input, bool expected, string desc)
    {
        bool result = MPCDesignation.HasFragment(input);
        if (result == expected)
        {
            Console.WriteLine($"  PASS: HasFragment(\"{input}\") -> {result}");
            passed++;
        }
        else
        {
            Console.WriteLine($"  FAIL: HasFragment(\"{input}\"): expected {expected}, got {result}");
            failed++;
        }
    }

    static void TestGetFragment(string input, string expected, string desc)
    {
        string output = MPCDesignation.GetFragment(input);
        if (output == expected)
        {
            Console.WriteLine($"  PASS: GetFragment(\"{input}\") -> \"{output}\"");
            passed++;
        }
        else
        {
            Console.WriteLine($"  FAIL: GetFragment(\"{input}\"): expected \"{expected}\", got \"{output}\"");
            failed++;
        }
    }

    static void TestGetParent(string input, string expected, string desc)
    {
        string output = MPCDesignation.GetParent(input);
        if (output == expected)
        {
            Console.WriteLine($"  PASS: GetParent(\"{input}\") -> \"{output}\"");
            passed++;
        }
        else
        {
            Console.WriteLine($"  FAIL: GetParent(\"{input}\"): expected \"{expected}\", got \"{output}\"");
            failed++;
        }
    }

    static void TestEqual(string d1, string d2, bool expected, string desc)
    {
        bool result = MPCDesignation.DesignationsEqual(d1, d2);
        if (result == expected)
        {
            Console.WriteLine($"  PASS: Equal(\"{d1}\", \"{d2}\") -> {result}");
            passed++;
        }
        else
        {
            Console.WriteLine($"  FAIL: Equal(\"{d1}\", \"{d2}\"): expected {expected}, got {result}");
            failed++;
        }
    }

    static int Main(string[] args)
    {
        Console.WriteLine("=== MPC Designation Helper Function Tests ===\n");

        // Test ToReportFormat
        Console.WriteLine("--- ToReportFormat ---");

        // Numbered asteroids
        TestToReport("00001", "       00001", "Numbered asteroid 1");
        TestToReport("00433", "       00433", "Numbered asteroid 433");
        TestToReport("99999", "       99999", "Numbered asteroid 99999");
        TestToReport("A0000", "       A0000", "Numbered asteroid 100000");
        TestToReport("~0000", "       ~0000", "Numbered asteroid 620000");

        // Provisional asteroids
        TestToReport("J95X00A", "     J95X00A", "Provisional 1995 XA");
        TestToReport("K24A12B", "     K24A12B", "Provisional 2024 AB12");

        // Survey designations
        TestToReport("PLS2040", "     PLS2040", "Survey P-L");
        TestToReport("T3S3141", "     T3S3141", "Survey T-3");

        // Numbered comets
        TestToReport("0001P", "0001P       ", "Comet 1P");
        TestToReport("0073P", "0073P       ", "Comet 73P");

        // Numbered comets with fragments
        TestToReport("0073Pa", "0073P      a", "Comet 73P-A");
        TestToReport("0073Pb", "0073P      b", "Comet 73P-B");
        TestToReport("0073Paa", "0073P     aa", "Comet 73P-AA");
        TestToReport("0073Paz", "0073P     az", "Comet 73P-AZ");
        TestToReport("0073Pzz", "0073P     zz", "Comet 73P-ZZ");

        // Provisional comets
        TestToReport("CJ95O010", "    CJ95O010", "Comet C/1995 O1");
        TestToReport("DJ93F020", "    DJ93F020", "Comet D/1993 F2");
        TestToReport("DJ93F02a", "    DJ93F02a", "Comet D/1993 F2-A");

        // Test FromReportFormat
        Console.WriteLine("\n--- FromReportFormat ---");

        // Numbered asteroids
        TestFromReport("       00001", "00001", "Numbered asteroid 1");
        TestFromReport("       00433", "00433", "Numbered asteroid 433");
        TestFromReport("       A0000", "A0000", "Numbered asteroid 100000");

        // Provisional asteroids
        TestFromReport("     J95X00A", "J95X00A", "Provisional 1995 XA");

        // Numbered comets
        TestFromReport("0073P       ", "0073P", "Comet 73P");

        // Numbered comets with fragments
        TestFromReport("0073P      a", "0073Pa", "Comet 73P-A");
        TestFromReport("0073P     aa", "0073Paa", "Comet 73P-AA");
        TestFromReport("0073P     az", "0073Paz", "Comet 73P-AZ");

        // Provisional comets
        TestFromReport("    CJ95O010", "CJ95O010", "Comet C/1995 O1");

        // Test HasFragment
        Console.WriteLine("\n--- HasFragment ---");

        // Unpacked with fragments
        TestHasFragment("73P-A", true, "Unpacked numbered comet with fragment");
        TestHasFragment("73P-AA", true, "Unpacked numbered comet with 2-letter fragment");
        TestHasFragment("D/1993 F2-A", true, "Unpacked provisional comet with fragment");
        TestHasFragment("P/1930 J1-AA", true, "Unpacked provisional comet with 2-letter fragment");

        // Unpacked without fragments
        TestHasFragment("73P", false, "Unpacked numbered comet no fragment");
        TestHasFragment("C/1995 O1", false, "Unpacked provisional comet no fragment");

        // Packed with fragments
        TestHasFragment("0073Pa", true, "Packed numbered comet with fragment");
        TestHasFragment("0073Paa", true, "Packed numbered comet with 2-letter fragment");
        TestHasFragment("DJ93F02a", true, "Packed provisional comet with fragment");

        // Packed without fragments
        TestHasFragment("0073P", false, "Packed numbered comet no fragment");
        TestHasFragment("CJ95O010", false, "Packed provisional comet no fragment");

        // Non-comets
        TestHasFragment("1995 XA", false, "Asteroid no fragment");
        TestHasFragment("00001", false, "Numbered asteroid");

        // Test GetFragment
        Console.WriteLine("\n--- GetFragment ---");

        // Unpacked with fragments
        TestGetFragment("73P-A", "A", "Unpacked single fragment");
        TestGetFragment("73P-AA", "AA", "Unpacked 2-letter fragment");
        TestGetFragment("73P-I", "I", "Unpacked fragment I");
        TestGetFragment("D/1993 F2-B", "B", "Unpacked provisional fragment");
        TestGetFragment("P/1930 J1-AZ", "AZ", "Unpacked provisional 2-letter");

        // Unpacked without fragments
        TestGetFragment("73P", "", "Unpacked no fragment");
        TestGetFragment("C/1995 O1", "", "Unpacked provisional no fragment");

        // Packed with fragments
        TestGetFragment("0073Pa", "A", "Packed single fragment");
        TestGetFragment("0073Paa", "AA", "Packed 2-letter fragment");
        TestGetFragment("0073Pi", "I", "Packed fragment I");
        TestGetFragment("DJ93F02b", "B", "Packed provisional fragment");

        // Packed without fragments
        TestGetFragment("0073P", "", "Packed no fragment");
        TestGetFragment("CJ95O010", "", "Packed provisional no fragment");

        // Test GetParent
        Console.WriteLine("\n--- GetParent ---");

        // Unpacked with fragments
        TestGetParent("73P-A", "73P", "Unpacked single fragment");
        TestGetParent("73P-AA", "73P", "Unpacked 2-letter fragment");
        TestGetParent("D/1993 F2-B", "D/1993 F2", "Unpacked provisional fragment");
        TestGetParent("P/1930 J1-AA", "P/1930 J1", "Unpacked provisional 2-letter");

        // Unpacked without fragments
        TestGetParent("73P", "73P", "Unpacked no fragment");
        TestGetParent("C/1995 O1", "C/1995 O1", "Unpacked provisional no fragment");

        // Packed with fragments
        TestGetParent("0073Pa", "0073P", "Packed single fragment");
        TestGetParent("0073Paa", "0073P", "Packed 2-letter fragment");

        // Packed without fragments
        TestGetParent("0073P", "0073P", "Packed no fragment");

        // Non-comets (should return as-is)
        TestGetParent("1995 XA", "1995 XA", "Asteroid");
        TestGetParent("00001", "00001", "Numbered asteroid");

        // Test DesignationsEqual
        Console.WriteLine("\n--- DesignationsEqual ---");

        // Same designation, different formats
        TestEqual("1995 XA", "J95X00A", true, "Provisional packed/unpacked");
        TestEqual("73P", "0073P", true, "Numbered comet packed/unpacked");
        TestEqual("73P-A", "0073Pa", true, "Comet with fragment packed/unpacked");
        TestEqual("73P-AA", "0073Paa", true, "Comet with 2-letter fragment");
        TestEqual("1", "00001", true, "Numbered asteroid");
        TestEqual("C/1995 O1", "CJ95O010", true, "Provisional comet");

        // Different designations
        TestEqual("1995 XA", "1995 XB", false, "Different provisional");
        TestEqual("73P-A", "73P-B", false, "Different fragments");
        TestEqual("73P", "74P", false, "Different comet numbers");
        TestEqual("1", "2", false, "Different asteroid numbers");

        // Same designation (both packed or both unpacked)
        TestEqual("1995 XA", "1995 XA", true, "Same unpacked");
        TestEqual("J95X00A", "J95X00A", true, "Same packed");

        // Summary
        Console.WriteLine("\n==================================================");
        Console.WriteLine($"Total: {passed + failed}, Passed: {passed}, Failed: {failed}");

        return failed == 0 ? 0 : 1;
    }
}
