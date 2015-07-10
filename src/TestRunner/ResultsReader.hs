module TestRunner.ResultsReader (
  readResults,
  TestResults,
  TestError) where

import qualified Protocol.Test as P
import           Text.Read (readMaybe)
import           Data.Maybe (isJust)
import           System.Exit

type TestResults   = [P.TestResult]
type TestError = (String, String)

readResults :: (ExitCode, String, String) -> Either TestError TestResults
readResults (exit, out, err)
    | Just testResults <- readMaybe out = (Right) . map (\(Just (title,status,result)) -> (P.TestResult title status result)) . filter isJust $ testResults
    | otherwise = Left (exitCode exit , out ++ err)


exitCode :: ExitCode -> String
exitCode (ExitFailure _) = "failed"
exitCode _               = "passed"
