module QueryRunnerSpec (spec) where

import           Test.Hspec
import           Protocol.Query
import           Server.Query

processExit = fmap exit.process

spec = describe "QueryServer" $ do
  let okQuery = "foo 1"

  let okCode        = "foo = bar"
  let okCodeOnExtra = "foo = (*2)"

  let extraCode     = "bar = (+1)"

  it "should errored on empty request" $ do
    processExit emptyRequest `shouldReturn` "errored"

  it "should pass on ok request" $ do
    processExit (emptyRequest { content = okCode, query = okQuery }) `shouldReturn` "passed"

