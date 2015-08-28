module Server.Query (process) where

import Protocol.Query
import Interpreter
import Interpreter.Exit

process :: Request -> IO Response
process = fmap toResponse.runQuery.compileRequest


compileRequest :: Request -> String
compileRequest (Request query content extra) = unlines [
                        content,
                        extra,
                        "main :: IO ()",
                        "main = putStr.show $ " ++ query ]


runQuery :: String -> IO (String, String)
runQuery =  fmap extract.runCode (\(exit, out, err) -> Ok (toStatus exit, out ++ err))
  where extract (Error x) = x
        extract (Ok    x) = x

toResponse (exit, out) = Response exit out