module SmellsDetectorRunnerSpec (spec) where

import           Test.Hspec
import           Server.Test.SmellsDetectorRunner
import           Protocol.Test.Expectation

sampleSubmission = "x = m x\n\
                   \y x = if True then True else False\n\
                   \z k x = (\\f -> g f) x k\n\
                   \w x = y z x"

spec :: Spec
spec = do
  describe "runSmellsDetection" $ do
    it "detects bindings with smells" $ do
      runSmellsDetection sampleSubmission `shouldBe` [
        ExpectationResult (Expectation "y" "Not:HasRedundantIf")            False,
        ExpectationResult (Expectation "z" "Not:HasRedundantLambda")        False
        -- ExpectationResult (Expectation "w" "Not:HasRedundantParameter")     False
        ]
