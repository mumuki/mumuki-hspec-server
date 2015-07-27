module RequestValidator.Fuse (isMalicious) where

import Data.List (isInfixOf)

isMalicious code = "System.IO.Unsafe" `isInfixOf` code