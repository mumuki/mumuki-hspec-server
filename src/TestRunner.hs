{-# LANGUAGE OverloadedStrings #-}

module TestRunner (runTest) where

import qualified Config
import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose, hPutStr)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)

runTest :: String -> IO (String, String)
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  hPutStr fileHandle content
  hClose fileHandle
  (exit, out, err) <- runCommand path
  removeFile path
  return (exitCode exit , out ++ err)

runCommand :: String -> IO (ExitCode, String, String)
runCommand path =
  readProcessWithExitCode "runhaskell"  (Config.runhaskellArgs ++ [ path ]) ""

exitCode (ExitFailure _) = "failed"
exitCode _               = "passed"