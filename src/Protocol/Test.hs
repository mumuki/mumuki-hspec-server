{-# LANGUAGE DeriveGeneric #-}

module Protocol.Test (
  Response(..),
  Request(..),
  emptyRequest,
  emptyResponse) where

import           Protocol.Test.Expectation
import           Protocol.Test.Test
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
} deriving (Eq, Show, Generic)

instance FromJSON Request
instance ToJSON Response

emptyRequest :: Request
emptyRequest = Request "" "" "" []

emptyResponse :: Response
emptyResponse = Response "" "" [] []