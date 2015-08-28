{-# LANGUAGE DeriveGeneric #-}

module Protocol.Query (
  emptyRequest,
  emptyResponse,
  Response(..),
  Request(..)) where

import           GHC.Generics
import           Data.Aeson

data Request = Request {
  query  :: String,
  content :: String,
  extra    :: String
} deriving (Show, Generic)

data Response =  Response {
  exit  :: String,
  out   :: String
} deriving (Eq, Show, Generic)

instance FromJSON Request
instance ToJSON Response

emptyRequest :: Request
emptyRequest = Request "" "" ""

emptyResponse :: Response
emptyResponse = Response "" ""

