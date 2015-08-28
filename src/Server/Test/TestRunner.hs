{-# LANGUAGE PatternGuards #-}

module Server.Test.TestRunner (
  runTest,
  TestResults,
  RunnerResult(..)) where

import qualified Protocol.Test.Test as P
import           Text.Read (readMaybe)
import           Data.Maybe (isJust, fromJust)
import           System.Exit
import           Server.CodeRunner

type TestResults   = [P.TestResult]

runTest :: String -> IO (RunnerResult TestResults)
runTest = runCode readResults

readResults :: CommandExit -> RunnerResult TestResults
readResults (exit, out, err)
    | Just testResults <- readMaybe out = Ok (toTestResults testResults)
    | otherwise = Error (exitCode exit , out ++ err)

toTestResults :: [Maybe (String, String, String)] -> TestResults
toTestResults = map (toTestResult.fromJust) . filter isJust

toTestResult (title, status, result) = P.TestResult title status result

exitCode :: ExitCode -> String
exitCode (ExitFailure _) = "errored"
exitCode _               = "passed"