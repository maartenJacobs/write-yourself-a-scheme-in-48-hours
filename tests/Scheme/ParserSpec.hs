module Scheme.ParserSpec (spec) where

import Test.Hspec
import Scheme.Parser (
      parseExpr
    , LispVal(..)
    , SimpleNumber(..)
    , ComplexNumber(..)
    , Exactness(..)
    )
import Text.ParserCombinators.Parsec (parse)

exactnessOptions :: [(String, Exactness)]
exactnessOptions = [("", Inexact), ("#i", Inexact), ("#e", Exact)]

testParse :: String -> LispVal -> Expectation
testParse input expected =
    parse parseExpr "(test input)" input `shouldBe` Right expected

assertExactness :: String -> SimpleNumber -> Expectation
assertExactness numInput expected = sequence_ [
        testParse (e ++ numInput) (SimpleLispNum expected repr)
        | (e, repr) <- exactnessOptions
    ]

assertExactnessWithBase :: Char -> String -> SimpleNumber -> Expectation
assertExactnessWithBase base numInput expected = sequence_ [
        testParse (pre ++ numInput) (SimpleLispNum expected repr)
        | (e, repr) <- exactnessOptions,
          beforeBase <- [True, False],
          let pre = if beforeBase then e ++ ['#', base] else ['#', base] ++ e
    ]

spec :: Spec
spec =
    describe "scheme parser" $ do
        context "string parsing" $ do
            it "parses simple strings" $ do
                testParse "\"string\"" (String "string")
                testParse "\"word1 word2 word3\"" (String "word1 word2 word3")
            it "parses escaped characters" $ do
                testParse "\"\\\"string\\\"\"" (String "\"string\"")
                testParse "\"\\n\"" (String "\n")
                testParse "\"\\t\"" (String "\t")
                testParse "\"\\r\"" (String "\r")
                testParse "\"\\\\\"" (String "\\")
        context "bool parsing" $ do
            it "parses bools" $ do
                testParse "#t" (Bool True)
                testParse "#f" (Bool False)
        context "integer parsing" $ do
            it "parses simple integers" $ do
                testParse "1234" (SimpleLispNum (Integer 1234) Inexact)
                testParse "+1234" (SimpleLispNum (Integer 1234) Inexact)
                testParse "-1234" (SimpleLispNum (Integer (-1234)) Inexact)
            it "parses simple integers with exactness" $ do
                assertExactness "1234" (Integer 1234)
                assertExactness "+1234" (Integer 1234)
                assertExactness "-1234" (Integer (-1234))
            it "parses simple integers with different bases" $ do
                testParse "#b1101" (SimpleLispNum (Integer 13) Inexact)
                testParse "#b+1101" (SimpleLispNum (Integer 13) Inexact)
                testParse "#b-1101" (SimpleLispNum (Integer (-13)) Inexact)
                testParse "#h5ab10" (SimpleLispNum (Integer 371472) Inexact)
                testParse "#h+5ab10" (SimpleLispNum (Integer 371472) Inexact)
                testParse "#h-5ab10" (SimpleLispNum (Integer (-371472)) Inexact)
                testParse "#o56" (SimpleLispNum (Integer 46) Inexact)
                testParse "#o+56" (SimpleLispNum (Integer 46) Inexact)
                testParse "#o-56" (SimpleLispNum (Integer (-46)) Inexact)
                testParse "#d123" (SimpleLispNum (Integer 123) Inexact)
                testParse "#d+123" (SimpleLispNum (Integer 123) Inexact)
                testParse "#d-123" (SimpleLispNum (Integer (-123)) Inexact)
            it "parses integers with different bases and exactness" $ do
                assertExactnessWithBase 'b' "1101" (Integer 13)
                assertExactnessWithBase 'b' "1101" (Integer 13)
                assertExactnessWithBase 'b' "-1101" (Integer (-13))
                assertExactnessWithBase 'h' "5ab10" (Integer 371472)
                assertExactnessWithBase 'h' "+5ab10" (Integer 371472)
                assertExactnessWithBase 'h' "-5ab10" (Integer (-371472))
                assertExactnessWithBase 'o' "56" (Integer 46)
                assertExactnessWithBase 'o' "56" (Integer 46)
                assertExactnessWithBase 'o' "-56" (Integer (-46))
                assertExactnessWithBase 'd' "123" (Integer 123)
                assertExactnessWithBase 'd' "123" (Integer 123)
                assertExactnessWithBase 'd' "-123" (Integer (-123))
            it "parses floats" $ do
                testParse "12.34" (SimpleLispNum (Float 12.34) Inexact)
                testParse "+12.34" (SimpleLispNum (Float 12.34) Inexact)
                testParse "-12.34" (SimpleLispNum (Float (-12.34)) Inexact)
                testParse "0.23" (SimpleLispNum (Float 0.23) Inexact)
                testParse "23." (SimpleLispNum (Float 23.0) Inexact)
        context "character parsing" $ do
            it "parses character literals" $ do
                testParse "#\\a" (Character 'a')
                testParse "#\\ " (Character ' ')
                testParse "#\\space" (Character ' ')
                testParse "#\\newline" (Character '\n')
