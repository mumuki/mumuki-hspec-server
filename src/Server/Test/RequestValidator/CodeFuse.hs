module Server.Test.RequestValidator.CodeFuse (isMalicious) where

import Data.List (isInfixOf)

isMalicious code = "System.IO.Unsafe" `isInfixOf` code