module FuseSpec (spec) where

import Fuse

describe "Fuse" $ do
  it "should admit non malicious code" $ do
    isMalicious "x = 4" `shouldBe` False

  it "should admit non malicious code with imports" $ do
    isMalicious "import Data.List\nx = 4" `shouldBe` False

  it "should detect System.IO.Unsafe imports" $ do
    isMalicious "import System.IO.Unsafe\nx = 4" `shouldBe` True
