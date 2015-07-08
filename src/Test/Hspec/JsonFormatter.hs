module Test.Hspec.JsonFormatter where

import Test.Hspec.Formatters
import Data.List


jsonFormatter :: Formatter
jsonFormatter = silent {
  exampleSucceeded = \p ->
    write $ "{\"title\":"++ formatPath p ++",\"status\":\"passed\",\"result\":\"\"},"
, exampleFailed    = \p (Right e) ->
    write $ "{\"title\":"++ formatPath p ++",\"status\":\"failed\",\"result\":\""++ e ++"\"},"
, headerFormatter  = write $ "["
, footerFormatter  = write $ "{}]"
}

formatPath (ps, p) = intercalate " " (ps ++ [p])