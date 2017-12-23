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
