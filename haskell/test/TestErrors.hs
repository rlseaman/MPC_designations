{-|
Module      : TestErrors
Description : Test MPC designation error handling
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import MPCDesignation

main :: IO ()
main = do
    args <- getArgs
    case args of
        [csvFile] -> runTests csvFile
        _ -> do
            hPutStrLn stderr "Usage: test_errors <csv_file>"
            exitFailure

runTests :: FilePath -> IO ()
runTests csvFile = do
    contents <- readFile csvFile
    let allLines = lines contents
        dataLines = drop 1 allLines  -- Skip header
        testCases = map parseLine dataLines

    putStrLn $ "Testing " ++ show (length testCases) ++ " error cases"
    putStrLn ""

    let results = map testError testCases
        passed = length $ filter id results
        failed = length $ filter not results

    putStrLn $ "Passed: " ++ show passed
    putStrLn $ "Failed: " ++ show failed

    if failed > 0
        then exitFailure
        else exitSuccess

parseLine :: String -> (String, String)
parseLine line =
    let (input, rest) = break (== ',') line
        errorType = drop 1 rest
    in (input, errorType)

testError :: (String, String) -> Bool
testError (input, _expectedError) =
    -- The test passes if convertSimple returns an error (Left)
    case convertSimple input of
        Left _ -> True
        Right _ -> False
