module TestRunner (runTest) where

import qualified Config
import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose, hPutStr)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           Control.Concurrent
import           Control.Concurrent.Async (race)
import           TestRunner.ResultsReader

type CommandResults = Maybe (ExitCode, String, String)

runTest :: String -> IO (Either TestError TestResults)
runTest content = do
  path <- writeTempFile content
  commandResults <- runCommand path
  removeFile path
  return.readCommandResults $ commandResults

writeTempFile :: String -> IO FilePath
writeTempFile content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  hPutStr fileHandle content
  hClose fileHandle
  return path

readCommandResults :: CommandResults -> Either TestError TestResults
readCommandResults (Just result) = readResults result
readCommandResults Nothing       = Left ("failed", message)
    where message = "Test took more than 3 seconds. Test was aborted"

runCommand :: String -> IO CommandResults
runCommand path = limited 4500000 command
    where command = readProcessWithExitCode "./limit" ([ "1024", "4", "runhaskell" ] ++ Config.runhaskellArgs ++ [ path ]) "";

limited :: Int -> IO a -> IO (Maybe a)
limited n f = fmap get $ race f (threadDelay n)
  where get (Left a) = Just a
        get _        = Nothing
