module ExpectationsRunner where

import qualified Protocol as P
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
compile x | "HasUsage:" `isInfixOf` x = hasUsage (remove "HasUsage:" x)
compile "HasLambda"          = hasLambda
compile "HasGuards"          = hasGuards
compile "HasComposition"     = hasComposition
compile "HasBinding"         = hasBinding
compile "HasDirectRecursion" = hasDirectRecursion
compile "HasComprehension"   = hasComprehension
compile "HasIf"              = hasIf
compile "HasConditional"     = hasConditional
compile "HasTypeDeclaration" = hasTypeDeclaration
compile "HasTypeSignature"   = hasTypeSignature
compile _                    = \_ _ -> False

remove xs = drop (length xs)
