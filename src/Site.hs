{-# LANGUAGE OverloadedStrings #-}

module Site (site) where

import           Data.Aeson
import           Control.Applicative
import           Snap.Core
import           Control.Monad.Trans (liftIO)
import           TestRunner

site :: Snap ()
site = method POST (
          route [ ("test", testHandler) ]) <|>
       method GET  (redirect "http://mumuki.herokuapp.com")

testHandler :: Snap ()
testHandler = do
    content <- readRequestBody 102400
    result <- liftIO . runTest  $ content
    writeLBS . encode $ result

