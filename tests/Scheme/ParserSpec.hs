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

type Lexeme = String

-- | 'addBaseAndExactness' returns a list of possible number prefix forms,
-- consisting of the base and exactness.
addBaseAndExactness :: String -> String -> String -> [Lexeme]
addBaseAndExactness inp base exact = [base ++ exact ++ inp, exact ++ base ++ inp]

-- | 'exactnessOptions' lists the forms of exactness as seen in the
-- source code of a Scheme program and which exactness the form represents.
exactnessOptions :: [(Lexeme, Exactness)]
exactnessOptions = [("", Inexact), ("#i", Inexact), ("#e", Exact)]

testParse :: String -> LispVal -> Expectation
testParse input expected =
    parse parseExpr "(test input)" input `shouldBe` Right expected

assertExactness :: String -> SimpleNumber -> Expectation
assertExactness term expected = sequence_ [
        testParse lexeme (SimpleLispNum expected repr)
        | (e, repr) <- exactnessOptions
        , lexeme <- addBaseAndExactness term "" e
    ]

assertExactnessWithBase :: Char -> String -> SimpleNumber -> Expectation
assertExactnessWithBase base term expected = sequence_ [
        testParse lexeme (SimpleLispNum expected repr)
        | (e, repr) <- exactnessOptions
        , lexeme <- addBaseAndExactness term ['#', base] e
    ]

assertParseSimpleNumber :: String -> SimpleNumber -> Expectation
assertParseSimpleNumber inp expected = testParse inp (SimpleLispNum expected Inexact)

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
        context "number parsing" $ do
            it "parses simple integers" $ do
                assertParseSimpleNumber "1234" (Integer 1234)
                assertParseSimpleNumber "+1234" (Integer 1234)
                assertParseSimpleNumber "-1234" (Integer (-1234))
            it "parses simple integers with exactness" $ do
                assertExactness "1234" (Integer 1234)
                assertExactness "+1234" (Integer 1234)
                assertExactness "-1234" (Integer (-1234))
            it "parses simple integers with different bases" $ do
                assertParseSimpleNumber "#b1101" (Integer 13)
                assertParseSimpleNumber "#b+1101" (Integer 13)
                assertParseSimpleNumber "#b-1101" (Integer (-13))
                assertParseSimpleNumber "#h5ab10" (Integer 371472)
                assertParseSimpleNumber "#h+5ab10" (Integer 371472)
                assertParseSimpleNumber "#h-5ab10" (Integer (-371472))
                assertParseSimpleNumber "#o56" (Integer 46)
                assertParseSimpleNumber "#o+56" (Integer 46)
                assertParseSimpleNumber "#o-56" (Integer (-46))
                assertParseSimpleNumber "#d123" (Integer 123)
                assertParseSimpleNumber "#d+123" (Integer 123)
                assertParseSimpleNumber "#d-123" (Integer (-123))
            it "parses integers with different bases and exactness" $ do
                assertExactnessWithBase 'b' "1101" (Integer 13)
                assertExactnessWithBase 'b' "1101" (Integer 13)
                assertExactnessWithBase 'b' "-1101" (Integer (-13))
                assertExactnessWithBase 'h' "5ab10" (Integer 371472)
                assertExactnessWithBase 'h' "+5ab10" (Integer 371472)
                assertExactnessWithBase 'h' "-5ab10" (Integer (-371472))
                assertExactnessWithBase 'o' "56" (Integer 46)
                assertExactnessWithBase 'o' "+56" (Integer 46)
                assertExactnessWithBase 'o' "-56" (Integer (-46))
                assertExactnessWithBase 'd' "123" (Integer 123)
                assertExactnessWithBase 'd' "+123" (Integer 123)
                assertExactnessWithBase 'd' "-123" (Integer (-123))
            it "parses floats" $ do
                assertParseSimpleNumber "12.34" (Float 12.34)
                assertParseSimpleNumber "+12.34" (Float 12.34)
                assertParseSimpleNumber "-12.34" (Float (-12.34))
                assertParseSimpleNumber "0.23" (Float 0.23)
                assertParseSimpleNumber "23." (Float 23.0)
            it "parses rationals" $ do
                assertParseSimpleNumber "12/5" (Rational 12 5)
                assertParseSimpleNumber "+12/5" (Rational 12 5)
                assertParseSimpleNumber "-12/5" (Rational (-12) 5)
                assertParseSimpleNumber "2/3" (Rational 2 3)
                assertParseSimpleNumber "+2/3" (Rational 2 3)
                assertParseSimpleNumber "-2/3" (Rational (-2) 3)
            it "simplifies rationals" $ do
                assertParseSimpleNumber "12/2" (Integer 6)
                assertParseSimpleNumber "+12/2" (Integer 6)
                assertParseSimpleNumber "-12/2" (Integer (-6))
                assertParseSimpleNumber "15/5" (Integer 3)
                assertParseSimpleNumber "+15/5" (Integer 3)
                assertParseSimpleNumber "-15/5" (Integer (-3))
            it "parses rationals with exactness" $ do
                assertExactness "12/5" (Rational 12 5)
                assertExactness "+12/5" (Rational 12 5)
                assertExactness "-12/5" (Rational (-12) 5)
                assertExactness "12/2" (Integer 6)
                assertExactness "+12/2" (Integer 6)
                assertExactness "-12/2" (Integer (-6))
            it "parses rationals with different bases" $ do
                assertParseSimpleNumber "#b1100/101" (Rational 12 5)
                assertParseSimpleNumber "#b+1100/101" (Rational 12 5)
                assertParseSimpleNumber "#b-1100/101" (Rational (-12) 5)
                assertParseSimpleNumber "#o14/5" (Rational 12 5)
                assertParseSimpleNumber "#o+14/5" (Rational 12 5)
                assertParseSimpleNumber "#o-14/5" (Rational (-12) 5)
                assertParseSimpleNumber "#hc/5" (Rational 12 5)
                assertParseSimpleNumber "#h+c/5" (Rational 12 5)
                assertParseSimpleNumber "#h-c/5" (Rational (-12) 5)
                assertParseSimpleNumber "#d12/5" (Rational 12 5)
                assertParseSimpleNumber "#d+12/5" (Rational 12 5)
                assertParseSimpleNumber "#d-12/5" (Rational (-12) 5)
            it "parses rationals with different bases and exactness" $ do
                assertExactnessWithBase 'b' "1100/101" (Rational 12 5)
                assertExactnessWithBase 'b' "+1100/101" (Rational 12 5)
                assertExactnessWithBase 'b' "-1100/101" (Rational (-12) 5)
                assertExactnessWithBase 'o' "14/5" (Rational 12 5)
                assertExactnessWithBase 'o' "+14/5" (Rational 12 5)
                assertExactnessWithBase 'o' "-14/5" (Rational (-12) 5)
                assertExactnessWithBase 'h' "c/5" (Rational 12 5)
                assertExactnessWithBase 'h' "+c/5" (Rational 12 5)
                assertExactnessWithBase 'h' "-c/5" (Rational (-12) 5)
                assertExactnessWithBase 'd' "12/5" (Rational 12 5)
                assertExactnessWithBase 'd' "+12/5" (Rational 12 5)
                assertExactnessWithBase 'd' "-12/5" (Rational (-12) 5)
        context "character parsing" $ do
            it "parses character literals" $ do
                testParse "#\\a" (Character 'a')
                testParse "#\\ " (Character ' ')
                testParse "#\\space" (Character ' ')
                testParse "#\\newline" (Character '\n')
