package mpc;

import mpc.MPCDesignation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Test MPC designation with bidirectional timing and round-trip verification.
 *
 * Tests:
 * 1. Pack direction (unpacked -> packed) with timing
 * 2. Unpack direction (packed -> unpacked) with timing
 * 3. Unpacked round-trip: unpack(pack(x)) = x
 * 4. Packed round-trip: pack(unpack(y)) = y
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

        System.out.println("Loaded " + testCases.size() + " test cases");
        System.out.println();

        long packPassed = 0, packFailed = 0;
        long unpackPassed = 0, unpackFailed = 0;
        long rtUnpackedPassed = 0, rtUnpackedFailed = 0;
        long rtPackedPassed = 0, rtPackedFailed = 0;

        // Phase 1: Pack (unpacked -> packed)
        System.out.println("=== Phase 1: Pack (unpacked -> packed) ===");
        long startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String result = MPCDesignation.pack(tc.unpacked);
                if (result.equals(tc.packed)) {
                    packPassed++;
                } else {
                    packFailed++;
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                packFailed++;
            }
        }
        long elapsed = System.currentTimeMillis() - startTime;
        double rate = testCases.size() * 1000.0 / elapsed;
        System.out.println("Passed: " + packPassed);
        System.out.println("Failed: " + packFailed);
        System.out.printf("Time:   %dms (%.1f entries/sec)%n", elapsed, rate);
        System.out.println();

        // Phase 2: Unpack (packed -> unpacked)
        System.out.println("=== Phase 2: Unpack (packed -> unpacked) ===");
        startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String result = MPCDesignation.unpack(tc.packed);
                if (result.equals(tc.unpacked)) {
                    unpackPassed++;
                } else {
                    unpackFailed++;
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                unpackFailed++;
            }
        }
        elapsed = System.currentTimeMillis() - startTime;
        rate = testCases.size() * 1000.0 / elapsed;
        System.out.println("Passed: " + unpackPassed);
        System.out.println("Failed: " + unpackFailed);
        System.out.printf("Time:   %dms (%.1f entries/sec)%n", elapsed, rate);
        System.out.println();

        // Phase 3: Unpacked round-trip: unpack(pack(x)) = x
        System.out.println("=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===");
        startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String packed = MPCDesignation.pack(tc.unpacked);
                String back = MPCDesignation.unpack(packed);
                if (back.equals(tc.unpacked)) {
                    rtUnpackedPassed++;
                } else {
                    rtUnpackedFailed++;
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                rtUnpackedFailed++;
            }
        }
        elapsed = System.currentTimeMillis() - startTime;
        rate = testCases.size() * 1000.0 / elapsed;
        System.out.println("Passed: " + rtUnpackedPassed);
        System.out.println("Failed: " + rtUnpackedFailed);
        System.out.printf("Time:   %dms (%.1f entries/sec)%n", elapsed, rate);
        System.out.println();

        // Phase 4: Packed round-trip: pack(unpack(y)) = y
        System.out.println("=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===");
        startTime = System.currentTimeMillis();
        for (TestCase tc : testCases) {
            try {
                String unpacked = MPCDesignation.unpack(tc.packed);
                String back = MPCDesignation.pack(unpacked);
                if (back.equals(tc.packed)) {
                    rtPackedPassed++;
                } else {
                    rtPackedFailed++;
                }
            } catch (MPCDesignation.MPCDesignationException e) {
                rtPackedFailed++;
            }
        }
        elapsed = System.currentTimeMillis() - startTime;
        rate = testCases.size() * 1000.0 / elapsed;
        System.out.println("Passed: " + rtPackedPassed);
        System.out.println("Failed: " + rtPackedFailed);
        System.out.printf("Time:   %dms (%.1f entries/sec)%n", elapsed, rate);
        System.out.println();

        // Summary
        System.out.println("=== Summary ===");
        System.out.println("Pack:       " + (packFailed == 0 ? "PASS" : "FAIL (" + packFailed + ")"));
        System.out.println("Unpack:     " + (unpackFailed == 0 ? "PASS" : "FAIL (" + unpackFailed + ")"));
        System.out.println("Unpacked RT: " + (rtUnpackedFailed == 0 ? "PASS" : "FAIL (" + rtUnpackedFailed + ")"));
        System.out.println("Packed RT:   " + (rtPackedFailed == 0 ? "PASS" : "FAIL (" + rtPackedFailed + ")"));

        if (packFailed > 0 || rtPackedFailed > 0) {
            System.exit(1);
        }
    }
}
