{-|
Module      : Main
Description : CLI for MPC designation converter
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
        [] -> do
            hPutStrLn stderr "Usage: mpc_designation [-v|--verbose] <designation> [designation ...]"
            hPutStrLn stderr ""
            hPutStrLn stderr "Convert between packed and unpacked MPC designations."
            hPutStrLn stderr ""
            hPutStrLn stderr "Examples:"
            hPutStrLn stderr "  mpc_designation 00001             -> 1"
            hPutStrLn stderr "  mpc_designation 1                 -> 00001"
            hPutStrLn stderr "  mpc_designation J95X00A           -> 1995 XA"
            hPutStrLn stderr "  mpc_designation '1995 XA'         -> J95X00A"
            exitFailure
        ("-h":_) -> do
            putStrLn "MPC Designation Converter (Haskell)"
            exitSuccess
        ("--help":_) -> do
            putStrLn "MPC Designation Converter (Haskell)"
            exitSuccess
        ("-v":rest) -> processVerbose rest
        ("--verbose":rest) -> processVerbose rest
        designations -> processSimple designations

processSimple :: [String] -> IO ()
processSimple designations = do
    let multiple = length designations > 1
    mapM_ (processOne multiple) designations

processOne :: Bool -> String -> IO ()
processOne multiple des = case convertSimple des of
    Left (MPCError msg) -> do
        hPutStrLn stderr $ "Error: " ++ msg
        exitFailure
    Right result ->
        if multiple
            then putStrLn $ des ++ " -> " ++ result
            else putStrLn result

processVerbose :: [String] -> IO ()
processVerbose [] = do
    hPutStrLn stderr "No designations provided"
    exitFailure
processVerbose designations = mapM_ processOneVerbose designations

processOneVerbose :: String -> IO ()
processOneVerbose des = case convert des of
    Left (MPCError msg) -> do
        hPutStrLn stderr $ "Error: " ++ msg
        exitFailure
    Right (result, info) -> do
        putStrLn $ "  Input:    " ++ des
        putStrLn $ "  Detected: " ++ show (formatType info) ++ " format, " ++ subtype info
        let action = if formatType info == Packed
                     then "unpacking to human-readable form"
                     else "packing to MPC compact form"
        putStrLn $ "  Action:   " ++ action
        putStrLn $ "  Output:   " ++ result
        putStrLn ""
