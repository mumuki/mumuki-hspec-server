module ExpectationsRunnerSpec (spec) where

import           Test.Hspec
import           ExpectationsRunner (runExpectations)
import           Protocol

sampleSubmission = "x = g x\n\
                   \y x | x == 1  = z . k $ m\n\
                   \    | otherwise = 0"
spec :: Spec
spec = do
  describe "ExpectationsRunner.runExpectations" $ do
    let yHasComposition = Expectation "y" "HasComposition"
    let yHasGuards      = Expectation "y" "HasGuards"
    let xHasGuards      = Expectation "x" "HasGuards"
    let expectations = [ yHasComposition, yHasGuards, xHasGuards ]

    it "evals expectations" $ do
      runExpectations expectations sampleSubmission `shouldBe` [
        ExpectationResult yHasComposition True,
        ExpectationResult yHasGuards True,
        ExpectationResult xHasGuards False ]
