module RequestValidator (validateRequest) where

import RequestValidator.CodeFuse
import Protocol (Request(..))
import Control.Monad (msum)

validateRequest ::  Request -> Maybe String
validateRequest request = validateFirst request [
    (content, "submission"),
    (test,    "test"),
    (extra,   "extra")]

validateFirst request = msum.map (validate request)

validate request (property, propertyName)
  | isMalicious (property request) = Just ("Malicious code in " ++ propertyName ++ " detected")
  | otherwise = Nothing