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

runTest :: String -> IO (String, String)
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  hPutStr fileHandle content
  hClose fileHandle
  result <- runCommand path
  removeFile path
  case result of
    Just (exit, out, err) -> return (exitCode exit , out ++ err)
    Nothing -> return ("failed", "Test took more than 3 seconds. Test was aborted")


runCommand :: String -> IO (Maybe (ExitCode, String, String))
runCommand path = limited 3000000 command
    where command = readProcessWithExitCode "./limit" ([ "1024", "4", "runhaskell" ] ++ Config.runhaskellArgs ++ [ path ]) "";

exitCode :: ExitCode -> String
exitCode (ExitFailure _) = "failed"
exitCode _               = "passed"

limited :: Int -> IO a -> IO (Maybe a)
limited n f = fmap get $ race f (threadDelay n)
  where get (Left a) = Just a
        get _        = Nothing
