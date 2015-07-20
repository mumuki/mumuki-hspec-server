{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString

mumukiUrl :: ByteString
mumukiUrl = "http://mumuki.io"

runhaskellArgs :: [String]
--Local:
--runhaskellArgs = ["-package-db=./.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"]

--Heroku:
runhaskellArgs = ["-package-db=/app/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d"]
