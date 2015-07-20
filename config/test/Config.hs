{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString

mumukiUrl :: ByteString
mumukiUrl = "http://mumuki.herokuapp.com"

runhaskellArgs :: [String]
--Local:
--runhaskellArgs = ["-package-db=./.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"]

--Travis:
runhaskellArgs = []