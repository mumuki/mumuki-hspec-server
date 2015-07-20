{-# LANGUAGE DeriveGeneric #-}

module Protocol.Expectation (
  Expectation(..),
  ExpectationResult(..)) where

import           GHC.Generics
import           Data.Aeson

data Expectation = Expectation {
  binding :: String,
  inspection :: String
} deriving (Show, Eq, Generic)

data ExpectationResult = ExpectationResult {
  expectation :: Expectation,
  result :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Expectation

instance ToJSON Expectation
instance ToJSON ExpectationResult
