module Main where

import           Site
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site