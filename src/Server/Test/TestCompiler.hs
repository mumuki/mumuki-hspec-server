module Server.Test.TestCompiler (compile) where

  compile :: String -> String -> String -> String
  compile testSrc extraSrc contentSrc = unlines [
     "{-# OPTIONS_GHC -fdefer-type-errors #-}",
     "import Test.Hspec",
     "import Test.Hspec.Formatters.Structured",
     "import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))",
     "import Test.QuickCheck",
     contentSrc,
     extraSrc,
     "main :: IO ()",
     "main = hspecWith defaultConfig {configFormatter = Just structured} $ do",
      testSrc ]
