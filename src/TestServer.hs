module TestServer where

import qualified Protocol as P
import qualified TestRunner
import qualified TestCompiler
import qualified ExpectationsRunner
import qualified SmellsDetectorRunner


process :: P.Request -> IO P.Response
process r = do
            let expectationResults = ExpectationsRunner.runExpectations (P.expectations r)  (P.content r)
            let smellsResuls = SmellsDetectorRunner.runSmellsDetection (P.content r)
            let totalExpectationResults = expectationResults ++ smellsResuls
            result <- TestRunner.runTest . compile $ r
            case result of
              Left (exit, out) -> return $ P.Response exit out [] totalExpectationResults
              Right testResults -> return $ P.Response "" "" testResults totalExpectationResults

compile :: P.Request -> String
compile request = TestCompiler.compile (P.test request) (P.extra request) (P.content request)


