{-# LANGUAGE DeriveGeneric #-}

module Protocol (
  Response(..),
  Request(..),
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

data Request = Request {
  content  :: String,
  test     :: String,
  extra    :: String,
  expectations ::  [Expectation]
} deriving (Show, Generic)

data Response =  Response {
  exit  :: String,
  out   :: String,
  expectationResults :: [ExpectationResult]
} deriving (Show, Generic)

instance FromJSON Request
instance FromJSON Expectation

instance ToJSON Response
instance ToJSON Expectation
instance ToJSON ExpectationResult