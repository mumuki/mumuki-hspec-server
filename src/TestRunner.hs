{-# LANGUAGE OverloadedStrings #-}

module TestRunner (runTest) where

import qualified Config
import qualified Data.ByteString.Lazy as L
import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import qualified Protocol

runTest :: L.ByteString -> IO Protocol.Response
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  L.hPutStr fileHandle content
  hClose fileHandle
  (exit, out, err) <- runCommand path
  removeFile path
  return $ Protocol.Response (exitCode exit) (out ++ err)

runCommand :: String -> IO (ExitCode, String, String)
runCommand path =
  readProcessWithExitCode "runhaskell"  (Config.runhaskellArgs ++ [ path ]) ""

exitCode (ExitFailure _) = "failed"
exitCode _               = "passed"