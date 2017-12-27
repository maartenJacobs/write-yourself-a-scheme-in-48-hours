module Scheme.EvaluatorSpec (spec) where

import Test.Hspec
import Scheme.Core
import Scheme.Evaluator

spec :: Spec
spec =
    describe "Scheme evaluator" $ do
        it "unquotes values" $ do
            let val = (Character 'a')
            eval (List [Atom "quote", val]) `shouldBe` Right val
        it "adds numbers" $ do
            let n1 = Number (Complex (Integer 12) (Integer 0)) Inexact
            let n2 = Number (Complex (Integer 23) (Integer 0)) Inexact
            eval (List [Atom "+", n1, n2]) `shouldBe` Right (Number (Complex (Integer 35) (Integer 0)) Inexact)
        it "subtracts numbers" $ do
            let n1 = Number (Complex (Integer 12) (Integer 0)) Inexact
            let n2 = Number (Complex (Integer 23) (Integer 0)) Inexact
            eval (List [Atom "-", n1, n2]) `shouldBe` Right (Number (Complex (Integer (-11)) (Integer 0)) Inexact)
        it "identifies bools" $ do
            eval (List [Atom "boolean?", Bool False]) `shouldBe` Right (Bool True)
            eval (List [Atom "boolean?", Bool True]) `shouldBe` Right (Bool True)
            eval (List [Atom "boolean?", Character 'a']) `shouldBe` Right (Bool False)
            eval (List [Atom "boolean?", String "foo"]) `shouldBe` Right (Bool False)
        it "identifies dotted lists (pairs)" $ do
            eval (List [Atom "pair?", List [Atom "quote"
                                          , DottedList [Character 'a'] (Bool False)]]) `shouldBe` Right (Bool True)
            eval (List [Atom "pair?", Bool False]) `shouldBe` Right (Bool False)
            eval (List [Atom "pair?", Character 'a']) `shouldBe` Right (Bool False)
        it "identifies lists" $ do
            eval (List [Atom "list?", List [Atom "quote"
                                          , List [Character 'a']]]) `shouldBe` Right (Bool True)
            eval (List [Atom "list?", Bool False]) `shouldBe` Right (Bool False)
            eval (List [Atom "list?", Character 'a']) `shouldBe` Right (Bool False)
        it "confirms exactness" $ do
            let num = Complex (Integer 1234) (Integer 0)
            eval (List [Atom "exact?", Number num Exact]) `shouldBe` Right (Bool True)
            eval (List [Atom "exact?", Number num Inexact]) `shouldBe` Right (Bool False)
        it "identifies symbols/atoms" $ do
            eval (List [Atom "symbol?", List [Atom "quote", Atom "a"]]) `shouldBe` Right (Bool True)
            let num = Complex (Integer 1234) (Integer 0)
            eval (List [Atom "symbol?", Number num Exact]) `shouldBe` Right (Bool False)
        it "converts strings to and from symbols" $ do
            eval (List [Atom "symbol->string", List [Atom "quote", Atom "foo"]]) `shouldBe` Right (String "foo")
            eval (List [Atom "string->symbol", String "foo"]) `shouldBe` Right (Atom "foo")
        it "identifies characters" $ do
            eval (List [Atom "char?", Character 'a']) `shouldBe` Right (Bool True)
            eval (List [Atom "char?", String "foo"]) `shouldBe` Right (Bool False)
        it "compares numbers" $ do
            let less = Number (Complex (Integer 42) (Integer 0)) Exact
            let equal = Number (Complex (Integer 1234) (Integer 0)) Exact
            let more = Number (Complex (Integer 54321) (Integer 0)) Exact
            -- Relation of equal to itself.
            eval (List [Atom "=", equal, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "/=", equal, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "<", equal, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "<=", equal, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom ">", equal, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom ">=", equal, equal]) `shouldBe` Right (Bool True)
            -- Relation of less to equal.
            eval (List [Atom "=", less, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "/=", less, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "<", less, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "<=", less, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom ">", less, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom ">=", less, equal]) `shouldBe` Right (Bool False)
            -- Relation of more to equal.
            eval (List [Atom "=", more, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "/=", more, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "<", more, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "<=", more, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom ">", more, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom ">=", more, equal]) `shouldBe` Right (Bool True)
        it "combines bools" $ do
            let true = Bool True
            let false = Bool False
            -- Relation of true to itself.
            eval (List [Atom "||", true, true]) `shouldBe` Right (Bool True)
            eval (List [Atom "&&", true, true]) `shouldBe` Right (Bool True)
            -- Relation of false to itself.
            eval (List [Atom "||", false, false]) `shouldBe` Right (Bool False)
            eval (List [Atom "&&", false, false]) `shouldBe` Right (Bool False)
            -- Relation of false to true.
            eval (List [Atom "||", false, true]) `shouldBe` Right (Bool True)
            eval (List [Atom "&&", false, true]) `shouldBe` Right (Bool False)
        it "compares strings" $ do
            let less = String "foo"
            let equal = String "kyz"
            let more = String "zzz"
            -- Relation of equal to itself.
            eval (List [Atom "string=?", equal, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "string<?", equal, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string<=?", equal, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "string>?", equal, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string>=?", equal, equal]) `shouldBe` Right (Bool True)
            -- Relation of less to equal.
            eval (List [Atom "string=?", less, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string<?", less, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "string<=?", less, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "string>?", less, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string>=?", less, equal]) `shouldBe` Right (Bool False)
            -- Relation of more to equal.
            eval (List [Atom "string=?", more, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string<?", more, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string<=?", more, equal]) `shouldBe` Right (Bool False)
            eval (List [Atom "string>?", more, equal]) `shouldBe` Right (Bool True)
            eval (List [Atom "string>=?", more, equal]) `shouldBe` Right (Bool True)
