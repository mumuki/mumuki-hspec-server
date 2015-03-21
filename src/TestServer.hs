module TestServer where

import qualified Protocol as P
import qualified TestRunner
import qualified TestCompiler
import           Control.Applicative

process :: P.Request -> IO P.Response
process = liftA toResponse . TestRunner.runTest . compile
  where toResponse = uncurry (P.Response)

compile :: P.Request -> String
compile request = TestCompiler.compile (P.test request) (P.extra request) (P.content request)