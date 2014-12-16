{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Site (site) where

import           Data.Aeson
import           Data.Text
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Char8 (pack)
import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           GHC.Generics


data TestResult =  TestResult { exit  :: Int, out :: String }
                    deriving (Show, Generic)
instance ToJSON TestResult

site :: Snap ()
site = method POST (
          route [ ("test", testHandler) ]) <|>
       method GET  (redirect "http://mumuki.herokuapp.com")

testHandler :: Snap ()
testHandler = do
    content <- readRequestBody 102400
    result <- liftIO . runTest  $ content
    writeLBS . encode $ result

runTest :: L.ByteString -> IO TestResult
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  L.hPutStr fileHandle content
  hClose fileHandle
  (exit, out, err) <- readProcessWithExitCode "runhaskell" [
    "-package-conf=/app/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d",
    path ] ""
  removeFile path
  return $ TestResult (exitCode exit) (out ++ err)


exitCode (ExitFailure n) = n
exitCode _               = 0