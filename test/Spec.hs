module Main where

import Test.Hspec
import TinyEvaluator
import TinyDefinitions


main :: IO ()
main = hspec $ do
    describe "evaluate program" $ do
        it "adds integer literals correctly" $
            compileAndRun "(1 add 0)" `shouldBe` IntegerType 1
