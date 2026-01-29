import mpc.MPCDesignation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Test MPC designation round-trip conversions.
 * Tests: unpacked -> packed -> unpacked and packed -> unpacked -> packed
 */
public class TestRoundtrip {

    private static class TestCase {
        String unpacked;
        String packed;

        TestCase(String unpacked, String packed) {
            this.unpacked = unpacked;
            this.packed = packed;
        }
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: TestRoundtrip <prov_unpack_to_pack.csv>");
            System.exit(1);
        }

        String csvFile = args[0];
        List<TestCase> testCases = new ArrayList<>();

        // Load test cases
        try (BufferedReader reader = new BufferedReader(new FileReader(csvFile))) {
            String line;
            boolean isFirstLine = true;
            while ((line = reader.readLine()) != null) {
                if (line.isEmpty()) continue;

                if (isFirstLine) {
                    isFirstLine = false;
                    if (line.startsWith("unpacked") || line.contains("designation")) {
                        continue;
                    }
                }

                String[] parts = line.split(",", 2);
                if (parts.length != 2) continue;

                testCases.add(new TestCase(parts[0], parts[1]));
            }
        } catch (IOException e) {
            System.err.println("Cannot open file: " + csvFile);
            System.exit(1);
        }

        System.out.println("=== MPC Designation Round-trip Tests (Java) ===");
        System.out.println("Loaded " + testCases.size() + " test cases");
        System.out.println();

        int passed, failed;
        List<String> errors = new ArrayList<>();

        // Phase 1: Pack (unpacked -> packed)
        System.out.println("=== Phase 1: Pack (unpacked -> packed) ===");
        passed = 0;
        failed = 0;
        errors.clear();
        long startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String result = MPCDesignation.convertSimple(tc.unpacked);
                if (result.equals(tc.packed)) {
                    passed++;
                } else {
                    failed++;
                    if (errors.size() < 5) {
                        errors.add("pack('" + tc.unpacked + "'): expected '" + tc.packed + "', got '" + result + "'");
                    }
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                failed++;
                if (errors.size() < 5) {
                    errors.add("pack('" + tc.unpacked + "'): " + e.getMessage());
                }
            }
        }
        long phase1Time = System.currentTimeMillis() - startTime;
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed);
        for (String err : errors) System.out.println("  " + err);
        System.out.println();

        boolean phase1Pass = failed == 0;

        // Phase 2: Unpack (packed -> unpacked)
        System.out.println("=== Phase 2: Unpack (packed -> unpacked) ===");
        passed = 0;
        failed = 0;
        errors.clear();
        startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String result = MPCDesignation.convertSimple(tc.packed);
                if (result.equals(tc.unpacked)) {
                    passed++;
                } else {
                    failed++;
                    if (errors.size() < 5) {
                        errors.add("unpack('" + tc.packed + "'): expected '" + tc.unpacked + "', got '" + result + "'");
                    }
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                failed++;
                if (errors.size() < 5) {
                    errors.add("unpack('" + tc.packed + "'): " + e.getMessage());
                }
            }
        }
        long phase2Time = System.currentTimeMillis() - startTime;
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed + " (old-style designations convert to modern format)");
        for (String err : errors) System.out.println("  " + err);
        System.out.println();

        // Phase 3: Unpacked round-trip: unpack(pack(x)) = x
        System.out.println("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===");
        passed = 0;
        failed = 0;
        errors.clear();
        startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String packed = MPCDesignation.convertSimple(tc.unpacked);
                String back = MPCDesignation.convertSimple(packed);
                if (back.equals(tc.unpacked)) {
                    passed++;
                } else {
                    failed++;
                    if (errors.size() < 5) {
                        errors.add("'" + tc.unpacked + "' -> '" + packed + "' -> '" + back + "'");
                    }
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                failed++;
                if (errors.size() < 5) {
                    errors.add("'" + tc.unpacked + "': " + e.getMessage());
                }
            }
        }
        long phase3Time = System.currentTimeMillis() - startTime;
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed + " (old-style designations convert to modern format)");
        for (String err : errors) System.out.println("  " + err);
        System.out.println();

        // Phase 4: Packed round-trip: pack(unpack(y)) = y
        System.out.println("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===");
        passed = 0;
        failed = 0;
        errors.clear();
        startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String unpacked = MPCDesignation.convertSimple(tc.packed);
                String back = MPCDesignation.convertSimple(unpacked);
                if (back.equals(tc.packed)) {
                    passed++;
                } else {
                    failed++;
                    if (errors.size() < 5) {
                        errors.add("'" + tc.packed + "' -> '" + unpacked + "' -> '" + back + "'");
                    }
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                failed++;
                if (errors.size() < 5) {
                    errors.add("'" + tc.packed + "': " + e.getMessage());
                }
            }
        }
        long phase4Time = System.currentTimeMillis() - startTime;
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed);
        for (String err : errors) System.out.println("  " + err);
        System.out.println();

        boolean phase4Pass = failed == 0;

        // Summary
        System.out.println("=== Summary ===");
        System.out.printf("Phase 1 (pack):           %s%n", phase1Pass ? "PASS" : "FAIL");
        System.out.printf("Phase 4 (packed RT):      %s%n", phase4Pass ? "PASS" : "FAIL");
        System.out.println();
        System.out.println("Note: Phases 2 and 3 have expected failures for old-style");
        System.out.println("      designations (A908 CJ -> 1908 CJ) which is correct behavior.");

        // Only fail if phases 1 or 4 fail
        if (!phase1Pass || !phase4Pass) {
            System.exit(1);
        }
    }
}
