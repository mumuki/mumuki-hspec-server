module ExpectationsRunnerSpec (spec) where

import           Test.Hspec
import           ExpectationsRunner (runExpectations)
import           Protocol

sampleSubmission = "x = m x\n\
                   \y x | x == 1  = z . k $ m\n\
                   \    | otherwise = 0"
spec :: Spec
spec = do
  describe "ExpectationsRunner.runExpectations" $ do
    let yHasComposition = Expectation "y" "HasComposition"
    let yHasGuards      = Expectation "y" "HasGuards"
    let xHasGuards      = Expectation "x" "HasGuards"
    let xHasNotGuards   = Expectation "x" "Not:HasGuards"
    let xHasUsageOfM    = Expectation "x" "HasUsage:m"


    it "evals expectations" $ do
      let expectations = [ yHasComposition, yHasGuards, xHasGuards ]

      runExpectations expectations sampleSubmission `shouldBe` [
        ExpectationResult yHasComposition True,
        ExpectationResult yHasGuards True,
        ExpectationResult xHasGuards False ]

    it "evals negated expectations" $ do
      let expectations = [ xHasNotGuards ]

      runExpectations expectations sampleSubmission `shouldBe` [
        ExpectationResult xHasHotGuards True ]


    it "evals parameterized expectations" $ do
      let expectations = [ xHasUsageOfM ]

      runExpectations expectations sampleSubmission `shouldBe` [
        ExpectationResult xHasUsageOfM True ]
