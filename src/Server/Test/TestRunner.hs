{-# LANGUAGE PatternGuards #-}

module Server.Test.TestRunner (
  runTest,
  TestResults,
  RunnerResult(..)) where

import qualified Protocol.Test.Test as P
import           Text.Read (readMaybe)
import           Data.Maybe (isJust, fromJust)
import           Server.CodeRunner
import           Interpreter.Exit

type TestResults   = [P.TestResult]

runTest :: String -> IO (RunnerResult TestResults)
runTest = runCode readResults

readResults :: CommandExit -> RunnerResult TestResults
readResults (exit, out, err)
    | Just testResults <- readMaybe out = Ok (toTestResults testResults)
    | otherwise = Error (toStatus exit , out ++ err)

toTestResults :: [Maybe (String, String, String)] -> TestResults
toTestResults = map (toTestResult.fromJust) . filter isJust

toTestResult (title, status, result) = P.TestResult title status result

