{-# LANGUAGE OverloadedStrings #-}

module Site (site) where

import qualified Config
import           Data.Aeson
import           Control.Applicative
import           Snap.Core
import           Control.Monad.Trans (liftIO)
import           TestRunner
import           TestCompiler

site :: Snap ()
site = method POST (
          route [ ("test", testHandler) ]) <|>
       method GET  (redirect Config.mumukiUrl)

testHandler :: Snap ()
testHandler = do
    content <- readRequestBody 102400
    result <- liftIO . runTest  $ content
    writeLBS . encode $ result

