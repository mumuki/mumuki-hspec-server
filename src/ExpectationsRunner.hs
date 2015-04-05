module ExpectationsRunner where

import qualified Protocol as P
import           Language.Haskell.Inspector
import           Language.Haskell.Inspector.Smell as S
import           Data.List (isInfixOf)

runExpectations :: [P.Expectation] ->  String -> [P.ExpectationResult]
runExpectations es content = map run es ++ smellsResults
  where
    run e = P.ExpectationResult e (compileAndEval e)

    compileAndEval (P.Expectation binding inspection) = (compile inspection) binding content

    smellsResults = filter isFailed . map run $ smellInspections

smellInspections :: [Inspection]
smellInspections = map negateInspection [hasRedundantIf, hasRedundantLambda, hasRedudantBooleanComparison]

compile :: String -> Inspection
compile x | "Not:" `isInfixOf`      x = negateInspection $ compile (remove "Not:" x)
compile x | "HasUsage:" `isInfixOf` x = hasUsage (remove "HasUsage:" x)
compile "HasLambda"          = hasLambda
compile "HasGuards"          = hasGuards
compile "HasComposition"     = hasComposition
compile "HasBinding"         = hasBinding
compile "HasDirectRecursion" = hasDirectRecursion
compile "HasComprehension"   = hasComprehension
compile "HasIf"              = hasIf
compile "HasConditional"     = hasConditional

remove xs = drop (length xs)

negateInspection :: Inspection -> Inspection
negateInspection f code = not . f code