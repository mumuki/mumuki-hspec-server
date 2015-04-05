module SmellsDetectorRunnerSpec (spec) where

import           Test.Hspec
import           SmellsDetectorRunner
import           Protocol

sampleSubmission = "x = m x\n\
                   \y x = if True then True else False\n\
                   \z k = (\\f -> g f) k"

spec :: Spec
spec = do
  describe "runSmellsDetection" $ do
    it "detects bindings with smells" $ do
      runSmellsDetection sampleSubmission `shouldBe` [
        ExpectationResult (Expectation "y" "Not:HasRedundantIf")     False,
        ExpectationResult (Expectation "z" "Not:HasRedundantLambda") False ]
