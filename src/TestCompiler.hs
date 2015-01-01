module TestCompiler (compile) where

  compile :: String -> String -> String
  compile testSrc contentSrc =
     "import Test.Hspec\n\
     \import Test.QuickCheck\n" ++
     contentSrc ++ "\n\
     \main :: IO ()\n\
     \main = hspec $ do\n" ++
      testSrc
