module QueryServer (process) where

import Protocol.Query

process :: Request -> IO Response
process = fmap toResponse.runQuery.compileRequest


compileRequest :: Request -> String
compileRequest (Request query content extra) = content ++ extra ++ "main = putStr.show $ " ++ query

runQuery :: String -> IO (String, String)
runQuery compilation = return ("errored", "unimplemented")

toResponse (exit, out) = Response exit out