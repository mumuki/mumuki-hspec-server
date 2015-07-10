{-# LANGUAGE DeriveGeneric #-}

module Protocol.Test (
  TestResult(..)) where

import           GHC.Generics
import           Data.Aeson

data TestResult = TestResult {
  title  :: String,
  status :: String,
  result :: String
} deriving (Show, Eq, Generic)

instance ToJSON TestResult