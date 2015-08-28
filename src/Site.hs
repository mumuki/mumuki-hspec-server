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
          route [ ("test", handler TestServer.process),
                  ("query", handler QueryServer.process) ]) <|>
       method GET  (redirect Config.mumukiUrl)

handler f = do
    setTimeout 8
    Just request <-  decode <$> readRequestBody 102400
    result  <- liftIO . f $ request
    writeLBS . encode $ result
