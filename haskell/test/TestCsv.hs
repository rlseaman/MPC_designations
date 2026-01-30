{-|
Module      : TestCsv
Description : Test MPC designation against CSV test data
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
            hPutStrLn stderr "Usage: test_csv <csv_file>"
            exitFailure

runTests :: FilePath -> IO ()
runTests csvFile = do
    contents <- readFile csvFile
    let allLines = lines contents
        dataLines = drop 1 allLines  -- Skip header
        testCases = map parseLine dataLines

    putStrLn $ "Loaded " ++ show (length testCases) ++ " test cases"
    putStrLn ""

    -- Phase 1: Pack (unpacked -> packed)
    putStrLn "=== Phase 1: Pack (unpacked -> packed) ==="
    startTime <- getCurrentTime

    let packResults = map testPack testCases
        (packPassed, packFailed) = countResults packResults

    -- Force evaluation
    packPassed `deepseq` packFailed `deepseq` return ()

    endTime <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
        rate = fromIntegral (length testCases) / (elapsed / 1000)

    putStrLn $ "Passed: " ++ show packPassed
    putStrLn $ "Failed: " ++ show packFailed
    putStrLn $ "Time:   " ++ show (round elapsed :: Int) ++ "ms (" ++ show (round rate :: Int) ++ ".0 entries/sec)"
    putStrLn ""

    if packFailed > 0
        then exitFailure
        else exitSuccess

parseLine :: String -> (String, String)
parseLine line =
    let (unpacked, rest) = break (== ',') line
        packed = drop 1 rest
    in (unpacked, packed)

testPack :: (String, String) -> Bool
testPack (unpacked, expected) =
    case pack unpacked of
        Right result -> result == expected
        Left _ -> False

countResults :: [Bool] -> (Int, Int)
countResults = foldr count (0, 0)
  where
    count True (p, f) = (p + 1, f)
    count False (p, f) = (p, f + 1)
