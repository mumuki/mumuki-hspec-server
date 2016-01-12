module Server.Query (process) where

import           Common
import           Protocol.Query
import           Interpreter
import           Interpreter.Exit

process :: Request -> IO Response
process = fmap toResponse.runQuery.compileRequest


compileRequest :: Request -> String
compileRequest (Request query content extra) = unlines [
                        "import Text.Show.Functions",
                        content,
                        extra,
                        "main :: IO ()",
                        "main = putStr.show $ " ++ query ]


runQuery :: String -> IO Raw
runQuery =  fmap extract.interpret (Ok . toRaw)
  where extract (Error x) = x
        extract (Ok    x) = x

toResponse (exit, out) = Response (show exit) out
