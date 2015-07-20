{-# LANGUAGE DeriveGeneric #-}

module Protocol (
  Response(..),
  Request(..),
  emptyResponse) where

import           Protocol.Expectation
import           Protocol.Test
import           GHC.Generics
import           Data.Aeson

data Request = Request {
  content  :: String,
  test     :: String,
  extra    :: String,
  expectations ::  [Expectation]
} deriving (Show, Generic)

data Response =  Response {
  exit  :: String,
  out   :: String,
  testResults :: [TestResult],
  expectationResults :: [ExpectationResult]
} deriving (Show, Generic)

instance FromJSON Request
instance ToJSON Response

emptyResponse :: Response
emptyResponse = Response "" "" [] []