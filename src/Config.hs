{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString

mumukiUrl :: ByteString
mumukiUrl = "http://mumuki.herokuapp.com"

runhaskellArgs :: [String]
--Local:
--runhaskellArgs = []
--Heroku:
runhaskellArgs = ["-package-conf=/app/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d"]