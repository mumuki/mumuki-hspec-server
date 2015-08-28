{-# LANGUAGE OverloadedStrings #-}

module Site (site) where

import qualified Config
import           Data.Aeson
import           Control.Applicative
import           Snap.Core
import           Control.Monad.Trans (liftIO)
import           Server.Test as TestServer
import           Server.Query as QueryServer

site :: Snap ()
site = method POST (
          route [ ("test", testHandler),
                  ("query", queryHandler) ]) <|>
       method GET  (redirect Config.mumukiUrl)


testHandler :: Snap ()
testHandler = handler TestServer.process

queryHandler :: Snap ()
queryHandler = handler QueryServer.process


handler f = do
    setTimeout 8
    Just request <-  decode <$> readRequestBody 102400
    result  <- liftIO . f $ request
    writeLBS . encode $ result
