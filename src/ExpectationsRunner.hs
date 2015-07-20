module ExpectationsRunner (
  runExpectations) where

import qualified Protocol.Expectation as P
import           Language.Haskell.Inspector
import           Language.Haskell.Inspector.Combiner
import           Data.List (isInfixOf)

runExpectations :: [P.Expectation] ->  String -> [P.ExpectationResult]
runExpectations es content = map run es
  where
    run e = P.ExpectationResult e (compileAndEval e)

    compileAndEval (P.Expectation binding inspection) = (compile inspection) binding content

    failed = not . P.result

compile :: String -> Inspection
compile x | "Not:" `isInfixOf`      x = negative $ compile (remove "Not:" x)
compile x | "HasUsage:" `isInfixOf` x = transitive $ hasUsage (remove "HasUsage:" x)
compile "HasLambda"          = transitive hasLambda
compile "HasGuards"          = transitive hasGuards
compile "HasComposition"     = transitive hasComposition
compile "HasBinding"         = transitive hasBinding
compile "HasDirectRecursion" = transitive hasDirectRecursion
compile "HasComprehension"   = transitive hasComprehension
compile "HasIf"              = transitive hasIf
compile "HasConditional"     = transitive hasConditional
compile "HasTypeDeclaration" = hasTypeDeclaration
compile "HasTypeSignature"   = hasTypeSignature
compile _                    = \_ _ -> True

remove xs = drop (length xs)
