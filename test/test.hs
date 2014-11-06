#! /usr/bin/env runhaskell
module Main where

import Control.Monad (when)
import Data.String.Utils (replace, split, strip)

import System (getArgs)

import System.Directory (setCurrentDirectory, getDirectoryContents)
import System.IO (hGetContents)
import System.Process (readProcess)
--import System.Process (runInteractiveCommand, waitForProcess)
import System.Environment.FindBin (getProgPath)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, joinPath)

main = do
  args <- getArgs
  when (length args > 1) $ error $ "This script currently only supports running one test at a " ++
    "time or running the entire test suite (by passing no command-line arguments)."
  testDir <- getProgPath
  let rootDir = takeDirectory testDir
  let srcDir = joinPath [rootDir, "src"]
  -- Setting "current working directory" is necessary so modules will be found as expected...
  setCurrentDirectory srcDir
  case length args of
    1 -> do out <- runTest rootDir testDir $ joinPath [rootDir, head args]
            putStrLn out
    0 -> do fs <- getTestFiles testDir
            runManyTests rootDir testDir $ map (\f -> joinPath [testDir, f]) fs
            putStrLn "Looks like we've had success."
  where
    runTest rootDir testDir testFile = readProcess "runhaskell"
      ["-i" ++ joinPath [rootDir, "tests"], testFile, testDir] ""
    runManyTests rootDir testDir fs = foreach fs (\f -> do
      putStrLn $ "Running test " ++ f ++ " ..."
      out <- runTest rootDir testDir f
      let outputSplit = filter (/="") $ map strip $ split " " $ replace "\r" " " out
      if looksLikeSuccess outputSplit
        then putStrLn $ "Test " ++ f ++ " seemed to pass."
        else error $ "FAILURE!\n\nHere's the output...\n\n" ++ out)
    looksLikeSuccess out = (takeLast 4 out == ["Errors:", "0", "Failures:", "0"]) ||
      (takeLast 6 out == ["errors", "=", "0,", "failures", "=", "0}"])
    takeLast n l = reverse $ take n $ reverse l
    getTestFiles dir = do
      fs <- getDirectoryContents dir
      return $ filter (`notElem` ["test.hs", "TestHelp.hs"]) $
        filter (\f -> takeLast 3 f == ".hs") fs

foreach items action = mapM_ action items

{-
getProcessOutput :: String -> IO String
getProcessOutput command = do
    -- Create the process
    (_pIn, pOut, pErr, handle) <- runInteractiveCommand command
    -- Wait for the process to finish and store its exit code
    exitCode <- waitForProcess handle
    -- Get the standard output.
    output   <- hGetContents pOut
    -- return both the output and the exit code.
    return output
-}
