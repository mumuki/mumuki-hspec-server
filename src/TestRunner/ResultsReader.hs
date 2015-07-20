{-# LANGUAGE PatternGuards #-}

module TestRunner.ResultsReader (
  readResults,
  TestResults,
  TestError) where

import qualified Protocol.Test as P
import           Text.Read (readMaybe)
import           Data.Maybe (isJust, fromJust)
import           System.Exit

type TestResults   = [P.TestResult]
type TestError = (String, String)

readResults :: (ExitCode, String, String) -> Either TestError TestResults
readResults (exit, out, err)
    | Just testResults <- readMaybe out = Right (toTestResults testResults)
    | otherwise = Left (exitCode exit , out ++ err)

toTestResults :: [Maybe (String, String, String)] -> TestResults
toTestResults = map (toTestResult.fromJust) . filter isJust

toTestResult (title, status, result) = P.TestResult title status result

exitCode :: ExitCode -> String
exitCode (ExitFailure _) = "errored"
exitCode _               = "passed"
