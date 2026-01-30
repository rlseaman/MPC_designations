/*
 * TestErrors.cs - Test MPC designation error handling
 *
 * Usage: dotnet run --project test/TestErrors.csproj [error_test_cases.csv]
 */

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using MPC;

class TestErrors
{
    static string UnescapeString(string s)
    {
        var result = new StringBuilder();
        for (int i = 0; i < s.Length; i++)
        {
            if (s[i] == '\\' && i + 1 < s.Length)
            {
                char next = s[i + 1];
                if (next == 'x' && i + 3 < s.Length)
                {
                    string hex = s.Substring(i + 2, 2);
                    if (int.TryParse(hex, System.Globalization.NumberStyles.HexNumber, null, out int val))
                    {
                        result.Append((char)val);
                        i += 3;
                        continue;
                    }
                }
                switch (next)
                {
                    case 'n': result.Append('\n'); i++; break;
                    case 'r': result.Append('\r'); i++; break;
                    case 't': result.Append('\t'); i++; break;
                    case 'f': result.Append('\f'); i++; break;
                    case 'v': result.Append('\v'); i++; break;
                    case '0': result.Append('\0'); i++; break;
                    case '\\': result.Append('\\'); i++; break;
                    default: result.Append(s[i]); break;
                }
            }
            else
            {
                result.Append(s[i]);
            }
        }
        return result.ToString();
    }

    static bool RunErrorTests(string csvFile)
    {
        if (!File.Exists(csvFile))
        {
            Console.Error.WriteLine($"Error: Cannot open file: {csvFile}");
            return false;
        }

        int total = 0;
        int passed = 0;
        int failed = 0;

        Console.WriteLine("=== MPC Designation Error Tests ===\n");

        using (var reader = new StreamReader(csvFile))
        {
            string? line;
            while ((line = reader.ReadLine()) != null)
            {
                line = line.Trim();

                // Skip empty lines and comments
                if (string.IsNullOrEmpty(line) || line.StartsWith("#")) continue;

                // Skip header
                if (line.StartsWith("category,")) continue;

                // Parse CSV (simple split by comma, limit to 5 parts)
                string[] parts = line.Split(new[] { ',' }, 5);
                if (parts.Length < 5) continue;

                string category = parts[0];
                string subcategory = parts[1];
                string inputStr = UnescapeString(parts[2]);
                string expectedError = parts[3];
                string description = parts[4];

                total++;

                bool gotError = false;
                string errorMsg = "";
                string output = "";

                try
                {
                    output = MPCDesignation.ConvertSimple(inputStr);
                }
                catch (MPCDesignationException e)
                {
                    gotError = true;
                    errorMsg = e.Message;
                }
                catch (Exception e)
                {
                    gotError = true;
                    errorMsg = e.Message;
                }

                bool testPassed = false;

                if (expectedError == "valid")
                {
                    if (!gotError)
                    {
                        testPassed = true;
                    }
                    else
                    {
                        Console.WriteLine($"FAIL [{category}/{subcategory}]: '{description}'");
                        Console.WriteLine($"      Expected: valid conversion");
                        Console.WriteLine($"      Got:      {errorMsg}");
                        failed++;
                    }
                }
                else
                {
                    if (gotError)
                    {
                        testPassed = true;
                    }
                    else
                    {
                        Console.WriteLine($"FAIL [{category}/{subcategory}]: '{description}'");
                        Console.WriteLine($"      Expected: error ({expectedError})");
                        Console.WriteLine($"      Got:      '{output}' (success)");
                        failed++;
                    }
                }

                if (testPassed)
                {
                    passed++;
                }
            }
        }

        Console.WriteLine("\n=== Error Test Results ===");
        Console.WriteLine($"Total:  {total}");
        Console.WriteLine($"Passed: {passed}");
        Console.WriteLine($"Failed: {failed}");

        return failed == 0;
    }

    static int Main(string[] args)
    {
        string csvFile = args.Length > 0 ? args[0] : "error_test_cases.csv";

        bool success = RunErrorTests(csvFile);
        return success ? 0 : 1;
    }
}
