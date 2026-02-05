{-|
Module      : TestHelpers
Description : Test MPC designation helper functions

Tests format conversion (minimal <-> 12-char report format),
fragment extraction, and designation comparison functions.
-}

module Main where

import System.Exit (exitFailure, exitSuccess)
import Data.IORef
import MPCDesignation

main :: IO ()
main = do
    passedRef <- newIORef (0 :: Int)
    failedRef <- newIORef (0 :: Int)

    putStrLn "=== MPC Designation Helper Function Tests ===\n"

    -- Test toReportFormat
    putStrLn "--- toReportFormat ---"

    -- Numbered asteroids
    testToReport passedRef failedRef "00001" "       00001" "Numbered asteroid 1"
    testToReport passedRef failedRef "00433" "       00433" "Numbered asteroid 433"
    testToReport passedRef failedRef "99999" "       99999" "Numbered asteroid 99999"
    testToReport passedRef failedRef "A0000" "       A0000" "Numbered asteroid 100000"
    testToReport passedRef failedRef "~0000" "       ~0000" "Numbered asteroid 620000"

    -- Provisional asteroids
    testToReport passedRef failedRef "J95X00A" "     J95X00A" "Provisional 1995 XA"
    testToReport passedRef failedRef "K24A12B" "     K24A12B" "Provisional 2024 AB12"

    -- Survey designations
    testToReport passedRef failedRef "PLS2040" "     PLS2040" "Survey P-L"
    testToReport passedRef failedRef "T3S3141" "     T3S3141" "Survey T-3"

    -- Numbered comets
    testToReport passedRef failedRef "0001P" "0001P       " "Comet 1P"
    testToReport passedRef failedRef "0073P" "0073P       " "Comet 73P"

    -- Numbered comets with fragments
    testToReport passedRef failedRef "0073Pa" "0073P      a" "Comet 73P-A"
    testToReport passedRef failedRef "0073Pb" "0073P      b" "Comet 73P-B"
    testToReport passedRef failedRef "0073Paa" "0073P     aa" "Comet 73P-AA"
    testToReport passedRef failedRef "0073Paz" "0073P     az" "Comet 73P-AZ"
    testToReport passedRef failedRef "0073Pzz" "0073P     zz" "Comet 73P-ZZ"

    -- Provisional comets
    testToReport passedRef failedRef "CJ95O010" "    CJ95O010" "Comet C/1995 O1"
    testToReport passedRef failedRef "DJ93F020" "    DJ93F020" "Comet D/1993 F2"
    testToReport passedRef failedRef "DJ93F02a" "    DJ93F02a" "Comet D/1993 F2-A"

    -- Test fromReportFormat
    putStrLn "\n--- fromReportFormat ---"

    -- Numbered asteroids
    testFromReport passedRef failedRef "       00001" "00001" "Numbered asteroid 1"
    testFromReport passedRef failedRef "       00433" "00433" "Numbered asteroid 433"
    testFromReport passedRef failedRef "       A0000" "A0000" "Numbered asteroid 100000"

    -- Provisional asteroids
    testFromReport passedRef failedRef "     J95X00A" "J95X00A" "Provisional 1995 XA"

    -- Numbered comets
    testFromReport passedRef failedRef "0073P       " "0073P" "Comet 73P"

    -- Numbered comets with fragments
    testFromReport passedRef failedRef "0073P      a" "0073Pa" "Comet 73P-A"
    testFromReport passedRef failedRef "0073P     aa" "0073Paa" "Comet 73P-AA"
    testFromReport passedRef failedRef "0073P     az" "0073Paz" "Comet 73P-AZ"

    -- Provisional comets
    testFromReport passedRef failedRef "    CJ95O010" "CJ95O010" "Comet C/1995 O1"

    -- Test hasFragment
    putStrLn "\n--- hasFragment ---"

    -- Unpacked with fragments
    testHasFragment passedRef failedRef "73P-A" True "Unpacked numbered comet with fragment"
    testHasFragment passedRef failedRef "73P-AA" True "Unpacked numbered comet with 2-letter fragment"
    testHasFragment passedRef failedRef "D/1993 F2-A" True "Unpacked provisional comet with fragment"
    testHasFragment passedRef failedRef "P/1930 J1-AA" True "Unpacked provisional comet with 2-letter fragment"

    -- Unpacked without fragments
    testHasFragment passedRef failedRef "73P" False "Unpacked numbered comet no fragment"
    testHasFragment passedRef failedRef "C/1995 O1" False "Unpacked provisional comet no fragment"

    -- Packed with fragments
    testHasFragment passedRef failedRef "0073Pa" True "Packed numbered comet with fragment"
    testHasFragment passedRef failedRef "0073Paa" True "Packed numbered comet with 2-letter fragment"
    testHasFragment passedRef failedRef "DJ93F02a" True "Packed provisional comet with fragment"

    -- Packed without fragments
    testHasFragment passedRef failedRef "0073P" False "Packed numbered comet no fragment"
    testHasFragment passedRef failedRef "CJ95O010" False "Packed provisional comet no fragment"

    -- Non-comets
    testHasFragment passedRef failedRef "1995 XA" False "Asteroid no fragment"
    testHasFragment passedRef failedRef "00001" False "Numbered asteroid"

    -- Test getFragment
    putStrLn "\n--- getFragment ---"

    -- Unpacked with fragments
    testGetFragment passedRef failedRef "73P-A" "A" "Unpacked single fragment"
    testGetFragment passedRef failedRef "73P-AA" "AA" "Unpacked 2-letter fragment"
    testGetFragment passedRef failedRef "73P-I" "I" "Unpacked fragment I"
    testGetFragment passedRef failedRef "D/1993 F2-B" "B" "Unpacked provisional fragment"
    testGetFragment passedRef failedRef "P/1930 J1-AZ" "AZ" "Unpacked provisional 2-letter"

    -- Unpacked without fragments
    testGetFragment passedRef failedRef "73P" "" "Unpacked no fragment"
    testGetFragment passedRef failedRef "C/1995 O1" "" "Unpacked provisional no fragment"

    -- Packed with fragments
    testGetFragment passedRef failedRef "0073Pa" "A" "Packed single fragment"
    testGetFragment passedRef failedRef "0073Paa" "AA" "Packed 2-letter fragment"
    testGetFragment passedRef failedRef "0073Pi" "I" "Packed fragment I"
    testGetFragment passedRef failedRef "DJ93F02b" "B" "Packed provisional fragment"

    -- Packed without fragments
    testGetFragment passedRef failedRef "0073P" "" "Packed no fragment"
    testGetFragment passedRef failedRef "CJ95O010" "" "Packed provisional no fragment"

    -- Test getParent
    putStrLn "\n--- getParent ---"

    -- Unpacked with fragments
    testGetParent passedRef failedRef "73P-A" "73P" "Unpacked single fragment"
    testGetParent passedRef failedRef "73P-AA" "73P" "Unpacked 2-letter fragment"
    testGetParent passedRef failedRef "D/1993 F2-B" "D/1993 F2" "Unpacked provisional fragment"
    testGetParent passedRef failedRef "P/1930 J1-AA" "P/1930 J1" "Unpacked provisional 2-letter"

    -- Unpacked without fragments
    testGetParent passedRef failedRef "73P" "73P" "Unpacked no fragment"
    testGetParent passedRef failedRef "C/1995 O1" "C/1995 O1" "Unpacked provisional no fragment"

    -- Packed with fragments
    testGetParent passedRef failedRef "0073Pa" "0073P" "Packed single fragment"
    testGetParent passedRef failedRef "0073Paa" "0073P" "Packed 2-letter fragment"

    -- Packed without fragments
    testGetParent passedRef failedRef "0073P" "0073P" "Packed no fragment"

    -- Non-comets (should return as-is)
    testGetParent passedRef failedRef "1995 XA" "1995 XA" "Asteroid"
    testGetParent passedRef failedRef "00001" "00001" "Numbered asteroid"

    -- Test designationsEqual
    putStrLn "\n--- designationsEqual ---"

    -- Same designation, different formats
    testEqual passedRef failedRef "1995 XA" "J95X00A" True "Provisional packed/unpacked"
    testEqual passedRef failedRef "73P" "0073P" True "Numbered comet packed/unpacked"
    testEqual passedRef failedRef "73P-A" "0073Pa" True "Comet with fragment packed/unpacked"
    testEqual passedRef failedRef "73P-AA" "0073Paa" True "Comet with 2-letter fragment"
    testEqual passedRef failedRef "1" "00001" True "Numbered asteroid"
    testEqual passedRef failedRef "C/1995 O1" "CJ95O010" True "Provisional comet"

    -- Different designations
    testEqual passedRef failedRef "1995 XA" "1995 XB" False "Different provisional"
    testEqual passedRef failedRef "73P-A" "73P-B" False "Different fragments"
    testEqual passedRef failedRef "73P" "74P" False "Different comet numbers"
    testEqual passedRef failedRef "1" "2" False "Different asteroid numbers"

    -- Same designation (both packed or both unpacked)
    testEqual passedRef failedRef "1995 XA" "1995 XA" True "Same unpacked"
    testEqual passedRef failedRef "J95X00A" "J95X00A" True "Same packed"

    -- Summary
    passed <- readIORef passedRef
    failed <- readIORef failedRef
    let total = passed + failed

    putStrLn "\n=================================================="
    putStrLn $ "Total: " ++ show total ++ ", Passed: " ++ show passed ++ ", Failed: " ++ show failed

    if failed > 0
        then exitFailure
        else exitSuccess

testToReport :: IORef Int -> IORef Int -> String -> String -> String -> IO ()
testToReport passedRef failedRef input expected desc = do
    let output = toReportFormat input
    if output == expected
        then do
            putStrLn $ "  PASS: toReport(\"" ++ input ++ "\") -> \"" ++ output ++ "\""
            modifyIORef passedRef (+1)
        else do
            putStrLn $ "  FAIL: toReport(\"" ++ input ++ "\"): expected \"" ++ expected ++ "\", got \"" ++ output ++ "\""
            modifyIORef failedRef (+1)

testFromReport :: IORef Int -> IORef Int -> String -> String -> String -> IO ()
testFromReport passedRef failedRef input expected desc = do
    case fromReportFormat input of
        Right output ->
            if output == expected
                then do
                    putStrLn $ "  PASS: fromReport(\"" ++ input ++ "\") -> \"" ++ output ++ "\""
                    modifyIORef passedRef (+1)
                else do
                    putStrLn $ "  FAIL: fromReport(\"" ++ input ++ "\"): expected \"" ++ expected ++ "\", got \"" ++ output ++ "\""
                    modifyIORef failedRef (+1)
        Left err -> do
            putStrLn $ "  FAIL: fromReport(\"" ++ input ++ "\"): error " ++ show err
            modifyIORef failedRef (+1)

testHasFragment :: IORef Int -> IORef Int -> String -> Bool -> String -> IO ()
testHasFragment passedRef failedRef input expected desc = do
    let result = hasFragment input
    if result == expected
        then do
            putStrLn $ "  PASS: hasFragment(\"" ++ input ++ "\") -> " ++ show result
            modifyIORef passedRef (+1)
        else do
            putStrLn $ "  FAIL: hasFragment(\"" ++ input ++ "\"): expected " ++ show expected ++ ", got " ++ show result
            modifyIORef failedRef (+1)

testGetFragment :: IORef Int -> IORef Int -> String -> String -> String -> IO ()
testGetFragment passedRef failedRef input expected desc = do
    let output = getFragment input
    if output == expected
        then do
            putStrLn $ "  PASS: getFragment(\"" ++ input ++ "\") -> \"" ++ output ++ "\""
            modifyIORef passedRef (+1)
        else do
            putStrLn $ "  FAIL: getFragment(\"" ++ input ++ "\"): expected \"" ++ expected ++ "\", got \"" ++ output ++ "\""
            modifyIORef failedRef (+1)

testGetParent :: IORef Int -> IORef Int -> String -> String -> String -> IO ()
testGetParent passedRef failedRef input expected desc = do
    let output = getParent input
    if output == expected
        then do
            putStrLn $ "  PASS: getParent(\"" ++ input ++ "\") -> \"" ++ output ++ "\""
            modifyIORef passedRef (+1)
        else do
            putStrLn $ "  FAIL: getParent(\"" ++ input ++ "\"): expected \"" ++ expected ++ "\", got \"" ++ output ++ "\""
            modifyIORef failedRef (+1)

testEqual :: IORef Int -> IORef Int -> String -> String -> Bool -> String -> IO ()
testEqual passedRef failedRef d1 d2 expected desc = do
    let result = designationsEqual d1 d2
    if result == expected
        then do
            putStrLn $ "  PASS: equal(\"" ++ d1 ++ "\", \"" ++ d2 ++ "\") -> " ++ show result
            modifyIORef passedRef (+1)
        else do
            putStrLn $ "  FAIL: equal(\"" ++ d1 ++ "\", \"" ++ d2 ++ "\"): expected " ++ show expected ++ ", got " ++ show result
            modifyIORef failedRef (+1)
