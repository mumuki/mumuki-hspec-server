{-# LANGUAGE PatternGuards #-}

module Server.Test (process) where

import           Common
import           Protocol.Test
import           Server.Test.TestRunner
import           Server.Test.TestCompiler
import           Server.Test.ExpectationsRunner
import           Server.Test.SmellsDetectorRunner
import           Server.Test.RequestValidator

process :: Request -> IO Response
process r
  | (Just message) <- validateRequest r = return $ toResponse (Error (Aborted, message))
  | otherwise = run r

run :: Request -> IO Response
run r = do
  baseResponse <- (fmap toResponse) . runTest . compileRequest $ r
  return baseResponse { expectationResults = totalExpectationResults }

  where expectationResults = runExpectations (expectations r) (content r)
        smellsResuls = runSmellsDetection (content r)
        totalExpectationResults = expectationResults ++ smellsResuls

toResponse :: RunnerResult TestResults -> Response
toResponse (Error (e, o)) = emptyResponse { exit = show e, out = o }
toResponse (Ok trs)   = emptyResponse { testResults = trs }

compileRequest :: Request -> String
compileRequest request = compile (test request) (extra request) (content request)


