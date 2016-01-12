module QueryRunnerSpec (spec) where

import           Test.Hspec
import           Protocol.Query
import           Server.Query

processExit = fmap exit.process
processOut =  fmap out.process

spec = describe "QueryServer" $ do
  let okQuery = "foo 3"
  let okShowFunction = "even"

  let okCodeOnExtra   = "foo = bar"
  let okCode          = "foo = (*2)"

  let extraCode     = "bar = (+1)"

  it "should errored on empty request" $ do
    processExit emptyRequest `shouldReturn` "errored"

  it "should pass on ok request" $ do
    processExit (emptyRequest { content = okCode, query = okQuery }) `shouldReturn` "passed"

  it "should have result on ok request" $ do
    processOut (emptyRequest { content = okCode, query = okQuery }) `shouldReturn` "6"

  it "should have result on ok request with query dependent on extra" $ do
    processOut (emptyRequest { content = okCodeOnExtra, query = okQuery, extra = extraCode }) `shouldReturn` "4"

  it "should pass avoiding show function error" $ do
    processExit (emptyRequest { content = okCode, query = okShowFunction }) `shouldReturn` "passed"
