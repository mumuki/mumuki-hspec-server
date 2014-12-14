{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Char8 (pack)
import           System.Process (readProcessWithExitCode)
import           System.IO (hClose)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("test", testHandler) ]

testHandler :: Snap ()
testHandler = do
    content <- readRequestBody 102400
    result <- liftIO . runTest  $ content
    writeBS . pack $ result

runTest :: L.ByteString -> IO String
runTest content = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  L.hPutStr fileHandle content
  hClose fileHandle
  (_, out, err) <- readProcessWithExitCode "runhaskell" [path] ""
  removeFile path
  return (out ++ err)

