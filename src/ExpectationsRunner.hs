module ExpectationsRunner where

import qualified Protocol as P
import           Inspector

runExpectations :: [P.Expectation] ->  String -> [P.ExpectationResult]
runExpectations es content = map run es
  where
    run e = P.ExpectationResult e (compileAndEval e)

    compileAndEval (P.Expectation binding inspection) = (compile inspection) binding content

compile :: String -> Inspection
compile "HasLambda" = hasLambda
compile "HasGuards" = hasGuards
compile "HasComposition" = hasComposition
compile "HasBinding" = hasBinding

