module SmellsDetectorRunner (runSmellsDetection) where

import           Protocol
import           Language.Haskell.Explorer (Code)
import           Language.Haskell.Inspector
import           Language.Haskell.Inspector.Combiner
import           Language.Haskell.Inspector.Smell as S

type NamedSmell = (String, Inspection)

runSmellsDetection :: Code -> [ExpectationResult]
runSmellsDetection code = concatMap (`runSingleSmellDetection` code) smells

runSingleSmellDetection :: NamedSmell -> Code -> [ExpectationResult]
runSingleSmellDetection (name, inspection) code =
  map (smellyBindingToResult name) $ detect inspection code

smells :: [NamedSmell]
smells = [
  ("HasRedundantIf", hasRedundantIf),
  ("HasRedundantLambda", hasRedundantLambda),
  ("HasRedundantBooleanComparison", hasRedundantBooleanComparison),
  ("HasRedundantGuards", hasRedundantGuards)]

smellyBindingToResult smellName binding =
  ExpectationResult (Expectation binding ("Not:" ++ smellName)) False
