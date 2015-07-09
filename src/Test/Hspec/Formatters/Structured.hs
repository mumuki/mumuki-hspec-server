module Test.Hspec.Formatters.Structured where

import Test.Hspec.Formatters
import Data.List


structured :: Formatter
structured = silent {
  exampleSucceeded = \p ->
    write $ "Just (\"" ++ formatPath p ++"\",\"passed\",\"\"),"
, exampleFailed    = \p (Right e) ->
    write $ "Just (\"" ++ formatPath p ++"\",\"failed\",\""++ e ++"\"),"
, headerFormatter  = write $ "["
, footerFormatter  = write $ "Nothing]"
}

formatPath (ps, p) = intercalate " " (ps ++ [p])