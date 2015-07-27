module RequestValidator (validateRequest) where

import RequestValidator.CodeFuse
import Protocol (Request(..))

validateRequest ::  Request -> Maybe String
validateRequest request 
  | isMalicious (content request) = Just "Malicious code in submission detected"
  | isMalicious (test request)    = Just "Malicious code in test detected"
  | isMalicious (extra request)   = Just "Malicious code in extra code detected"
  | otherwise = Nothing