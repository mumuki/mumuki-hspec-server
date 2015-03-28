module TestServer where

import qualified Protocol as P
import qualified TestRunner
import qualified TestCompiler
import qualified ExpectationsRunner

process :: P.Request -> IO P.Response
process r = do
            (exit, out) <- TestRunner.runTest . compile $ r
            let expectationResults = ExpectationsRunner.runExpectations (P.expectations r)  (P.content r)
            return $ P.Response exit out expectationResults

compile :: P.Request -> String
compile request = TestCompiler.compile (P.test request) (P.extra request) (P.content request)


