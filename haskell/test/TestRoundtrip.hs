{-|
Module      : TestRoundtrip
Description : Test MPC designation with bidirectional timing and round-trip verification
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (deepseq)
import MPCDesignation

main :: IO ()
main = do
    args <- getArgs
    case args of
        [csvFile] -> runTests csvFile
        _ -> do
            hPutStrLn stderr "Usage: test_roundtrip <csv_file>"
            exitFailure

data TestCase = TestCase
    { tcUnpacked :: String
    , tcPacked :: String
    } deriving (Show)

runTests :: FilePath -> IO ()
runTests csvFile = do
    contents <- readFile csvFile
    let allLines = lines contents
        dataLines = drop 1 allLines  -- Skip header
        testCases = map parseLine dataLines

    let total = length testCases
    putStrLn $ "Loaded " ++ show total ++ " test cases"
    putStrLn ""

    -- Phase 1: Pack (unpacked -> packed)
    putStrLn "=== Phase 1: Pack (unpacked -> packed) ==="
    startTime <- getCurrentTime

    let packResults = map testPack testCases
        (packPassed, packFailed) = countResults packResults

    packPassed `deepseq` packFailed `deepseq` return ()

    endTime <- getCurrentTime
    let elapsed1 = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
        rate1 = fromIntegral total / (elapsed1 / 1000)

    putStrLn $ "Passed: " ++ show packPassed
    putStrLn $ "Failed: " ++ show packFailed
    putStrLn $ "Time:   " ++ show (round elapsed1 :: Int) ++ "ms (" ++ formatRate rate1 ++ " entries/sec)"
    putStrLn ""

    -- Phase 2: Unpack (packed -> unpacked)
    putStrLn "=== Phase 2: Unpack (packed -> unpacked) ==="
    startTime2 <- getCurrentTime

    let unpackResults = map testUnpack testCases
        (unpackPassed, unpackFailed) = countResults unpackResults

    unpackPassed `deepseq` unpackFailed `deepseq` return ()

    endTime2 <- getCurrentTime
    let elapsed2 = realToFrac (diffUTCTime endTime2 startTime2) * 1000 :: Double
        rate2 = fromIntegral total / (elapsed2 / 1000)

    putStrLn $ "Passed: " ++ show unpackPassed
    putStrLn $ "Failed: " ++ show unpackFailed
    putStrLn $ "Time:   " ++ show (round elapsed2 :: Int) ++ "ms (" ++ formatRate rate2 ++ " entries/sec)"
    putStrLn ""

    -- Phase 3: Unpacked round-trip: unpack(pack(x)) = x
    putStrLn "=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==="
    startTime3 <- getCurrentTime

    let rtUnpackedResults = map testUnpackedRoundtrip testCases
        (rtUnpackedPassed, rtUnpackedFailed) = countResults rtUnpackedResults

    rtUnpackedPassed `deepseq` rtUnpackedFailed `deepseq` return ()

    endTime3 <- getCurrentTime
    let elapsed3 = realToFrac (diffUTCTime endTime3 startTime3) * 1000 :: Double
        rate3 = fromIntegral total / (elapsed3 / 1000)

    putStrLn $ "Passed: " ++ show rtUnpackedPassed
    putStrLn $ "Failed: " ++ show rtUnpackedFailed
    putStrLn $ "Time:   " ++ show (round elapsed3 :: Int) ++ "ms (" ++ formatRate rate3 ++ " entries/sec)"
    putStrLn ""

    -- Phase 4: Packed round-trip: pack(unpack(y)) = y
    putStrLn "=== Phase 4: Packed round-trip: pack(unpack(y)) = y ==="
    startTime4 <- getCurrentTime

    let rtPackedResults = map testPackedRoundtrip testCases
        (rtPackedPassed, rtPackedFailed) = countResults rtPackedResults

    rtPackedPassed `deepseq` rtPackedFailed `deepseq` return ()

    endTime4 <- getCurrentTime
    let elapsed4 = realToFrac (diffUTCTime endTime4 startTime4) * 1000 :: Double
        rate4 = fromIntegral total / (elapsed4 / 1000)

    putStrLn $ "Passed: " ++ show rtPackedPassed
    putStrLn $ "Failed: " ++ show rtPackedFailed
    putStrLn $ "Time:   " ++ show (round elapsed4 :: Int) ++ "ms (" ++ formatRate rate4 ++ " entries/sec)"
    putStrLn ""

    -- Summary
    putStrLn "=== Summary ==="
    putStrLn $ "Pack:       " ++ (if packFailed == 0 then "PASS" else "FAIL (" ++ show packFailed ++ ")")
    putStrLn $ "Unpack:     " ++ (if unpackFailed == 0 then "PASS" else "FAIL (" ++ show unpackFailed ++ ")")
    putStrLn $ "Unpacked RT: " ++ (if rtUnpackedFailed == 0 then "PASS" else "FAIL (" ++ show rtUnpackedFailed ++ ")")
    putStrLn $ "Packed RT:   " ++ (if rtPackedFailed == 0 then "PASS" else "FAIL (" ++ show rtPackedFailed ++ ")")

    if packFailed > 0 || rtPackedFailed > 0
        then exitFailure
        else exitSuccess

parseLine :: String -> TestCase
parseLine line =
    let (unpacked, rest) = break (== ',') line
        packed = drop 1 rest
    in TestCase unpacked packed

testPack :: TestCase -> Bool
testPack tc =
    case pack (tcUnpacked tc) of
        Right result -> result == tcPacked tc
        Left _ -> False

testUnpack :: TestCase -> Bool
testUnpack tc =
    case unpack (tcPacked tc) of
        Right result -> result == tcUnpacked tc
        Left _ -> False

testUnpackedRoundtrip :: TestCase -> Bool
testUnpackedRoundtrip tc =
    case pack (tcUnpacked tc) of
        Right packed ->
            case unpack packed of
                Right back -> back == tcUnpacked tc
                Left _ -> False
        Left _ -> False

testPackedRoundtrip :: TestCase -> Bool
testPackedRoundtrip tc =
    case unpack (tcPacked tc) of
        Right unpacked ->
            case pack unpacked of
                Right back -> back == tcPacked tc
                Left _ -> False
        Left _ -> False

countResults :: [Bool] -> (Int, Int)
countResults = foldr count (0, 0)
  where
    count True (p, f) = (p + 1, f)
    count False (p, f) = (p, f + 1)

formatRate :: Double -> String
formatRate r = show (round r :: Int) ++ ".0"
