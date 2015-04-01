module ExpectationsRunner where

import qualified Protocol as P
import           Language.Haskell.Inspector

runExpectations :: [P.Expectation] ->  String -> [P.ExpectationResult]
runExpectations es content = map run es
  where
    run e = P.ExpectationResult e (compileAndEval e)

    compileAndEval (P.Expectation binding inspection) = (compile inspection) binding content

compile :: String -> Inspection
compile x           | "Not:" `isInfixOf`      x = not.compile (drop 4 x)
compile x           | "HasUsage:" `isInfixOf` x = hasUsage (drop 10 x)
compile "HasLambda"          = hasLambda
compile "HasGuards"          = hasGuards
compile "HasComposition"     = hasComposition
compile "HasBinding"         = hasBinding
compile "HasDirectRecursion" = hasBinding
compile "HasComprehension"   = hasBinding

