{-# LANGUAGE OverloadedStrings #-}

module Site (site) where

import qualified Config
import           Data.Aeson
import           Control.Applicative
import           Snap.Core
import           Control.Monad.Trans (liftIO)
import           TestServer

site :: Snap ()
site = method POST (
          route [ ("test", testHandler) ]) <|>
       method GET  (redirect Config.mumukiUrl)

testHandler :: Snap ()
testHandler = do
    Just request <-  decode <$> readRequestBody 102400
    result  <- liftIO . TestServer.process $ request
    writeLBS . encode $ result

