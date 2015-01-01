{-# LANGUAGE DeriveGeneric #-}

module Protocol (Response(..), Request(..)) where

import           GHC.Generics
import           Data.Aeson

data Request = Request {
    content  :: String,
    test     :: String
  } deriving (Show, Generic)

data Response =  Response {
    exit  :: String,
    out   :: String
  } deriving (Show, Generic)

instance FromJSON Request
instance ToJSON Response