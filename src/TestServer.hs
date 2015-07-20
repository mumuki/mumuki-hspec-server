module TestServer where

import           Protocol
import           TestRunner
import           TestCompiler
import           ExpectationsRunner
import           SmellsDetectorRunner


process :: Request -> IO Response
process r = do
  baseResponse <- (fmap toResponse) . runTest . compileRequest $ r
  return $ baseResponse { expectationResults = totalExpectationResults }

  where expectationResults = runExpectations (expectations r) (content r)
        smellsResuls = runSmellsDetection (content r)
        totalExpectationResults = expectationResults ++ smellsResuls

toResponse :: Either TestError TestResults -> Response
toResponse (Left (e, o)) = emptyResponse { exit = e, out = o }
toResponse (Right trs)   = emptyResponse { testResults = trs }

compileRequest :: Request -> String
compileRequest request = compile (test request) (extra request) (content request)


