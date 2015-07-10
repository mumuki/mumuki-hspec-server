module TestRunner (runTest) where

import qualified Config
import qualified Protocol.Test as P
import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose, hPutStr)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           Control.Concurrent
import           Control.Concurrent.Async (race)
import           Text.Read (readMaybe)
import           Data.Maybe (isJust)

type TestResults   = [P.TestResult]
type TestError = (String, String)

runTest :: String -> IO (Either TestError TestResults)
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  hPutStr fileHandle content
  hClose fileHandle
  resultMaybe <- runCommand path
  removeFile path
  case resultMaybe of
    Just result -> return $ readResult result
    Nothing -> return $ Left ("failed", "Test took more than 3 seconds. Test was aborted")

readResult :: (ExitCode, String, String) -> Either TestError TestResults
readResult (exit, out, err)
    | Just testResults <- readMaybe out = (Right) . map (\(Just (title,status,result)) -> (P.TestResult title status result)) . filter isJust $ testResults
    | otherwise = Left (exitCode exit , out ++ err)

runCommand :: String -> IO (Maybe (ExitCode, String, String))
runCommand path = limited 4500000 command
    where command = readProcessWithExitCode "./limit" ([ "1024", "4", "runhaskell" ] ++ Config.runhaskellArgs ++ [ path ]) "";

exitCode :: ExitCode -> String
exitCode (ExitFailure _) = "failed"
exitCode _               = "passed"

limited :: Int -> IO a -> IO (Maybe a)
limited n f = fmap get $ race f (threadDelay n)
  where get (Left a) = Just a
        get _        = Nothing
