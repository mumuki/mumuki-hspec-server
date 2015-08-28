{-# LANGUAGE PatternGuards #-}

module Server.Test.TestRunner (
  runTest,
  TestResults,
  Interpretation(..)) where

import qualified Protocol.Test.Test as P
import           Text.Read (readMaybe)
import           Data.Maybe (isJust, fromJust)
import           Interpreter
import           Interpreter.Exit

type TestResults   = [P.TestResult]

runTest :: String -> IO (Interpretation TestResults)
runTest = interpret readResults

readResults :: CommandExit -> Interpretation TestResults
readResults (exit, out, err)
    | Just testResults <- readMaybe out = Ok (toTestResults testResults)
    | otherwise = Error (toStatus exit, out ++ err)

toTestResults :: [Maybe (String, String, String)] -> TestResults
toTestResults = map (toTestResult.fromJust) . filter isJust

toTestResult (title, status, result) = P.TestResult title status result

