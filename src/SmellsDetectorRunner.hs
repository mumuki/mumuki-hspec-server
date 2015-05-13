module SmellsDetectorRunner (runSmellsDetection) where

import qualified Protocol as P
import           Language.Haskell.Explorer (Code)
import           Language.Haskell.Inspector
import           Language.Haskell.Inspector.Combiner
import           Language.Haskell.Inspector.Smell as S

type NamedSmell = (String, Inspection)

runSmellsDetection :: Code -> [P.ExpectationResult]
runSmellsDetection code = concatMap (`runSingleSmellDetection` code) smells

runSingleSmellDetection :: NamedSmell -> Code -> [P.ExpectationResult]
runSingleSmellDetection (name, inspection) code =
  map (smellyBindingToResult name) $ detect inspection code

smells :: [NamedSmell]
smells = [
  ("HasRedundantIf", hasRedundantIf),
  ("HasRedundantLambda", hasRedundantLambda),
  ("HasRedundantBooleanComparison", hasRedundantBooleanComparison),
  ("HasRedundantGuards", hasRedundantGuards),
  ("HasRedundantParameters", hasRedundantParameters)]
]

smellyBindingToResult smellName binding =
  P.ExpectationResult (P.Expectation binding ("Not:" ++ smellName)) False
