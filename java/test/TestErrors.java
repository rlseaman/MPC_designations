import mpc.MPCDesignation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Test MPC designation error handling.
 */
public class TestErrors {

    /**
     * Convert escape sequences in test input.
     */
    private static String unescapeInput(String input) {
        String result = input;
        result = result.replace("\\t", "\t");
        result = result.replace("\\n", "\n");
        result = result.replace("\\r", "\r");
        result = result.replace("\\f", "\f");
        result = result.replace("\\v", "\u000B");

        // Handle \x00 style escapes
        StringBuilder sb = new StringBuilder();
        int i = 0;
        while (i < result.length()) {
            if (i + 3 < result.length() && result.charAt(i) == '\\' && result.charAt(i + 1) == 'x') {
                try {
                    String hexStr = result.substring(i + 2, i + 4);
                    int val = Integer.parseInt(hexStr, 16);
                    sb.append((char) val);
                    i += 4;
                } catch (NumberFormatException e) {
                    sb.append(result.charAt(i));
                    i++;
                }
            } else {
                sb.append(result.charAt(i));
                i++;
            }
        }

        return sb.toString();
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: TestErrors <error_test_cases.csv>");
            System.exit(1);
        }

        String csvFile = args[0];
        int totalTests = 0;
        int passedTests = 0;
        int failedTests = 0;

        System.out.println("=== MPC Designation Error Tests (Java) ===");
        System.out.println();

        try (BufferedReader reader = new BufferedReader(new FileReader(csvFile))) {
            String line;
            while ((line = reader.readLine()) != null) {
                // Skip empty lines, comments, and header
                if (line.isEmpty() || line.startsWith("#") || line.startsWith("category,")) {
                    continue;
                }

                // Parse CSV line
                String[] parts = line.split(",", 5);
                if (parts.length < 5) {
                    continue;
                }

                String category = parts[0];
                String subcategory = parts[1];
                String rawInput = parts[2];
                String expectedError = parts[3];
                String description = parts[4];

                String input = unescapeInput(rawInput);
                totalTests++;

                boolean gotError = false;
                String errorType = "";
                String resultOutput = "";

                try {
                    MPCDesignation.Result result = MPCDesignation.convert(input);
                    resultOutput = result.output;
                } catch (MPCDesignation.MPCDesignationException e) {
                    gotError = true;
                    String errStr = e.getMessage();
                    if (errStr.contains("out of range") || errStr.contains("Invalid asteroid number")) {
                        errorType = "range";
                    } else {
                        errorType = "format";
                    }
                }

                String testId = category + "/" + subcategory;

                if (expectedError.equals("valid")) {
                    // Should succeed
                    if (gotError) {
                        failedTests++;
                        System.out.println("FAIL [" + testId + "]: '" + description + "'");
                        System.out.println("      Expected: valid conversion");
                        System.out.println("      Got:      error (" + errorType + ")");
                    } else {
                        passedTests++;
                    }
                } else {
                    // Should fail
                    if (!gotError) {
                        failedTests++;
                        System.out.println("FAIL [" + testId + "]: '" + description + "'");
                        System.out.println("      Expected: error (" + expectedError + ")");
                        System.out.println("      Got:      '" + resultOutput + "' (success)");
                    } else {
                        passedTests++;
                    }
                }
            }
        } catch (IOException e) {
            System.err.println("Cannot open file: " + csvFile);
            System.exit(1);
        }

        System.out.println();
        System.out.println("=== Error Test Results ===");
        System.out.println("Total:  " + totalTests);
        System.out.println("Passed: " + passedTests);
        System.out.println("Failed: " + failedTests);

        if (failedTests > 0) {
            System.exit(1);
        }
    }
}
