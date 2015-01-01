{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString

mumukiUrl :: ByteString
mumukiUrl = "http://mumuki.herokuapp.com"

runhaskellArgs :: [String]
runhaskellArgs = []