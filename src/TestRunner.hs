{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TestRunner (TestResult, runTest) where

import qualified Config
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           GHC.Generics

data TestResult =  TestResult { exit  :: String, out :: String }
                    deriving (Show, Generic)
instance ToJSON TestResult


runTest :: L.ByteString -> IO TestResult
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  L.hPutStr fileHandle content
  hClose fileHandle
  (exit, out, err) <- runCommand path
  removeFile path
  return $ TestResult (exitCode exit) (out ++ err)

runCommand path =
  readProcessWithExitCode "runhaskell"  (Config.runhaskellArgs ++ [ path ]) ""

exitCode (ExitFailure n) = "failed"
exitCode _               = "passed"