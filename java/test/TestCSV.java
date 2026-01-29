import mpc.MPCDesignation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Test MPC designation conversions against CSV test data.
 */
public class TestCSV {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: TestCSV <prov_unpack_to_pack.csv>");
            System.exit(1);
        }

        String csvFile = args[0];
        int totalTests = 0;
        int passedTests = 0;
        int failedTests = 0;

        System.out.println("=== MPC Designation Conversion Tests (Java) ===");
        System.out.println();

        try (BufferedReader reader = new BufferedReader(new FileReader(csvFile))) {
            String line;
            boolean isFirstLine = true;
            while ((line = reader.readLine()) != null) {
                // Skip empty lines
                if (line.isEmpty()) {
                    continue;
                }

                // Skip header row
                if (isFirstLine) {
                    isFirstLine = false;
                    if (line.startsWith("unpacked") || line.contains("designation")) {
                        continue;
                    }
                }

                // Parse CSV line
                String[] parts = line.split(",", 2);
                if (parts.length != 2) {
                    continue;
                }

                String unpacked = parts[0];
                String expectedPacked = parts[1];

                totalTests++;

                try {
                    // Test packing
                    String actualPacked = MPCDesignation.convertSimple(unpacked);
                    if (!actualPacked.equals(expectedPacked)) {
                        failedTests++;
                        if (failedTests <= 10) {
                            System.out.println("FAIL: pack('" + unpacked + "')");
                            System.out.println("      Expected: '" + expectedPacked + "'");
                            System.out.println("      Got:      '" + actualPacked + "'");
                        }
                    } else {
                        passedTests++;
                    }
                } catch (MPCDesignation.MPCDesignationException e) {
                    failedTests++;
                    if (failedTests <= 10) {
                        System.out.println("FAIL: pack('" + unpacked + "')");
                        System.out.println("      Expected: '" + expectedPacked + "'");
                        System.out.println("      Got error: " + e.getMessage());
                    }
                }
            }
        } catch (IOException e) {
            System.err.println("Cannot open file: " + csvFile);
            System.exit(1);
        }

        System.out.println();
        System.out.println("=== Conversion Test Results ===");
        System.out.println("Total:  " + totalTests);
        System.out.println("Passed: " + passedTests);
        System.out.println("Failed: " + failedTests);

        if (failedTests > 10) {
            System.out.println("(Showing first 10 failures only)");
        }

        if (failedTests > 0) {
            System.exit(1);
        }
    }
}
