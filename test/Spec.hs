module Main where

import Test.Hspec
import TinyEvaluator
import TinyDefinitions


main :: IO ()
main = hspec $ do
    describe "evaluate program" $ do
        it "correctly adds integer literals" $
            compileAndRun "(1 add 0)" `shouldBe` IntegerType 1
        it "correctly computes the value of 100 factorial" $
            compileAndRun "(let f be (lambda n in (if (n equals 0) then 1 else (n multiply (call f with (n subtract 1))))) in (call f with 100))" `shouldBe` IntegerType 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
        it "correctly preforms all math operations" $
            compileAndRun "(((16 multiply 42) divide 7) subtract (96 add (13 remainder 9)))" `shouldBe` IntegerType (-4)
        it "correctly preforms all Bool operations" $
            compileAndRun "((true or false) and ((not false) and (false equals false)))" `shouldBe` BoolType True