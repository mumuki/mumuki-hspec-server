module TestServerSpec (spec) where

import           Test.Hspec
import           Protocol.Test
import           Protocol.Test.Expectation
import           Protocol.Test.Test
import           TestServer

spec = describe "TestServer" $ do
  let test = "it \"x\" $ do\n\
              \   x `shouldBe` 1"

  let maliciusCode = "import System.IO.Unsafe\nx = 1"

  let okCode       = "x = 1"

  it "should errored on empty request" $ do
    fmap exit (process emptyRequest) `shouldReturn` "errored"

  it "should abort malicious code" $ do
    let request = emptyRequest { content = maliciusCode, test = test}
    let response = emptyResponse { exit = "aborted", out = "Malicious code in submission detected" }

    process request `shouldReturn` response

  it "should run tests" $ do
    let request = emptyRequest { content = okCode, test = test}
    let response = emptyResponse { testResults = [TestResult "x" "passed" ""] }

    process request `shouldReturn` response

  it "should run expectations" $ do
    let request = emptyRequest {
                        content = okCode,
                        test = test,
                        expectations = [Expectation "x" "HasBinding"]}

    fmap expectationResults (process request) `shouldReturn` [ExpectationResult (Expectation "x" "HasBinding") True]