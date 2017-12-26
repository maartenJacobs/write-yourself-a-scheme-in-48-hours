module Scheme.EvaluatorSpec (spec) where

import Test.Hspec
import Scheme.Core
import Scheme.Evaluator

spec :: Spec
spec =
    describe "Scheme evaluator" $ do
        it "unquotes values" $ do
            let val = (Character 'a')
            eval (List [Atom "quote", val]) `shouldBe` val
        it "adds numbers" $ do
            let n1 = Number (Complex (Integer 12) (Integer 0)) Inexact
            let n2 = Number (Complex (Integer 23) (Integer 0)) Inexact
            eval (List [Atom "+", n1, n2]) `shouldBe` Number (Complex (Integer 35) (Integer 0)) Inexact
        it "subtracts numbers" $ do
            let n1 = Number (Complex (Integer 12) (Integer 0)) Inexact
            let n2 = Number (Complex (Integer 23) (Integer 0)) Inexact
            eval (List [Atom "-", n1, n2]) `shouldBe` Number (Complex (Integer (-11)) (Integer 0)) Inexact
        it "identifies bools" $ do
            eval (List [Atom "boolean?", Bool False]) `shouldBe` Bool True
            eval (List [Atom "boolean?", Bool True]) `shouldBe` Bool True
            eval (List [Atom "boolean?", Character 'a']) `shouldBe` Bool False
            eval (List [Atom "boolean?", String "foo"]) `shouldBe` Bool False
        it "identifies dotted lists (pairs)" $ do
            eval (List [Atom "pair?", List [Atom "quote"
                                          , DottedList [Character 'a'] (Bool False)]]) `shouldBe` Bool True
            eval (List [Atom "pair?", Bool False]) `shouldBe` Bool False
            eval (List [Atom "pair?", Character 'a']) `shouldBe` Bool False
        it "identifies lists" $ do
            eval (List [Atom "list?", List [Atom "quote"
                                          , List [Character 'a']]]) `shouldBe` Bool True
            eval (List [Atom "list?", Bool False]) `shouldBe` Bool False
            eval (List [Atom "list?", Character 'a']) `shouldBe` Bool False
        it "confirms exactness" $ do
            let num = Complex (Integer 1234) (Integer 0)
            eval (List [Atom "exact?", Number num Exact]) `shouldBe` Bool True
            eval (List [Atom "exact?", Number num Inexact]) `shouldBe` Bool False
        it "identifies symbols/atoms" $ do
            eval (List [Atom "symbol?", List [Atom "quote", Atom "a"]]) `shouldBe` Bool True
            let num = Complex (Integer 1234) (Integer 0)
            eval (List [Atom "symbol?", Number num Exact]) `shouldBe` Bool False
