module Scheme.ParserSpec (spec) where

import Test.Hspec
import Scheme.Parser (
      parseExpr
    , LispVal(..)
    , SimpleNumber(..)
    , ComplexNumber(..)
    , Exactness(..)
    )
import Text.ParserCombinators.Parsec (parse, ParseError)

type Lexeme = String

-- | 'addBaseAndExactness' returns a list of possible number prefix forms,
-- consisting of the base and exactness.
addBaseAndExactness :: String -> String -> String -> [Lexeme]
addBaseAndExactness inp base exact = [base ++ exact ++ inp, exact ++ base ++ inp]

-- | 'exactnessOptions' lists the forms of exactness as seen in the
-- source code of a Scheme program and which exactness the form represents.
exactnessOptions :: [(Lexeme, Exactness)]
exactnessOptions = [("", Inexact), ("#i", Inexact), ("#e", Exact)]

parse' :: String -> Either ParseError LispVal
parse' input = parse parseExpr "(test input)" input

assertExpandParse :: String -> String -> Expectation
assertExpandParse unexpanded expanded = parse' unexpanded `shouldBe` parse' expanded

testParse :: String -> LispVal -> Expectation
testParse input expected =
    parse' input `shouldBe` Right expected

assertExactness :: String -> ComplexNumber -> Expectation
assertExactness term expected = sequence_ [
        testParse lexeme (LispNumber expected repr)
        | (e, repr) <- exactnessOptions
        , lexeme <- addBaseAndExactness term "" e
    ]

assertExactnessWithBase :: Char -> String -> ComplexNumber -> Expectation
assertExactnessWithBase base term expected = sequence_ [
        testParse lexeme (LispNumber expected repr)
        | (e, repr) <- exactnessOptions
        , lexeme <- addBaseAndExactness term ['#', base] e
    ]

assertParseComplexNumber :: String -> ComplexNumber -> Expectation
assertParseComplexNumber inp expected = testParse inp expectedLispNumber
    where expectedLispNumber = LispNumber expected Inexact

assertParseSimpleNumber :: String -> SimpleNumber -> Expectation
assertParseSimpleNumber inp expected = assertParseComplexNumber inp (Complex expected (Integer 0))

simpleToComplex :: SimpleNumber -> ComplexNumber
simpleToComplex simple = Complex simple (Integer 0)

integerToLisp :: Integer -> LispVal
integerToLisp int = LispNumber (simpleToComplex (Integer int)) Inexact

spec :: Spec
spec =
    describe "scheme parser" $ do
        context "when parsing strings" $ do
            it "parses simple strings" $ do
                testParse "\"string\"" (String "string")
                testParse "\"word1 word2 word3\"" (String "word1 word2 word3")
            it "parses escaped characters" $ do
                testParse "\"\\\"string\\\"\"" (String "\"string\"")
                testParse "\"\\n\"" (String "\n")
                testParse "\"\\t\"" (String "\t")
                testParse "\"\\r\"" (String "\r")
                testParse "\"\\\\\"" (String "\\")
        context "when parsing bools" $ do
            it "parses bools" $ do
                testParse "#t" (Bool True)
                testParse "#f" (Bool False)
        context "when parsing numbers" $ do
            it "parses simple integers" $ do
                assertParseSimpleNumber "1234" (Integer 1234)
                assertParseSimpleNumber "+1234" (Integer 1234)
                assertParseSimpleNumber "-1234" (Integer (-1234))
            it "parses simple integers with exactness" $ do
                assertExactness "1234" (simpleToComplex (Integer 1234))
                assertExactness "+1234" (simpleToComplex (Integer 1234))
                assertExactness "-1234" (simpleToComplex (Integer (-1234)))
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
                assertExactnessWithBase 'b' "1101" (simpleToComplex (Integer 13))
                assertExactnessWithBase 'b' "1101" (simpleToComplex (Integer 13))
                assertExactnessWithBase 'b' "-1101" (simpleToComplex (Integer (-13)))
                assertExactnessWithBase 'h' "5ab10" (simpleToComplex (Integer 371472))
                assertExactnessWithBase 'h' "+5ab10" (simpleToComplex (Integer 371472))
                assertExactnessWithBase 'h' "-5ab10" (simpleToComplex (Integer (-371472)))
                assertExactnessWithBase 'o' "56" (simpleToComplex (Integer 46))
                assertExactnessWithBase 'o' "+56" (simpleToComplex (Integer 46))
                assertExactnessWithBase 'o' "-56" (simpleToComplex (Integer (-46)))
                assertExactnessWithBase 'd' "123" (simpleToComplex (Integer 123))
                assertExactnessWithBase 'd' "+123" (simpleToComplex (Integer 123))
                assertExactnessWithBase 'd' "-123" (simpleToComplex (Integer (-123)))
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
                assertExactness "12/5" (simpleToComplex (Rational 12 5))
                assertExactness "+12/5" (simpleToComplex (Rational 12 5))
                assertExactness "-12/5" (simpleToComplex (Rational (-12) 5))
                assertExactness "12/2" (simpleToComplex (Integer 6))
                assertExactness "+12/2" (simpleToComplex (Integer 6))
                assertExactness "-12/2" (simpleToComplex (Integer (-6)))
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
                assertExactnessWithBase 'b' "1100/101" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'b' "+1100/101" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'b' "-1100/101" (simpleToComplex (Rational (-12) 5))
                assertExactnessWithBase 'o' "14/5" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'o' "+14/5" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'o' "-14/5" (simpleToComplex (Rational (-12) 5))
                assertExactnessWithBase 'h' "c/5" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'h' "+c/5" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'h' "-c/5" (simpleToComplex (Rational (-12) 5))
                assertExactnessWithBase 'd' "12/5" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'd' "+12/5" (simpleToComplex (Rational 12 5))
                assertExactnessWithBase 'd' "-12/5" (simpleToComplex (Rational (-12) 5))
            it "parses purely imaginary numbers with an integer as the imaginary part" $ do
                assertParseComplexNumber "+i" (Complex (Integer 0) (Integer 1))
                assertParseComplexNumber "-i" (Complex (Integer 0) (Integer (-1)))
                assertParseComplexNumber "+1i" (Complex (Integer 0) (Integer 1))
                assertParseComplexNumber "-1i" (Complex (Integer 0) (Integer (-1)))
                assertParseComplexNumber "0+1i" (Complex (Integer 0) (Integer 1))
                assertParseComplexNumber "0-1i" (Complex (Integer 0) (Integer (-1)))
            it "parses purely imaginary numbers with a float as the imaginary part" $ do
                assertParseComplexNumber "+1.i" (Complex (Integer 0) (Float 1))
                assertParseComplexNumber "-1.i" (Complex (Integer 0) (Float (-1)))
                assertParseComplexNumber "0+.2i" (Complex (Integer 0) (Float 0.2))
                assertParseComplexNumber "0-.2i" (Complex (Integer 0) (Float (-0.2)))
            it "parses purely imaginary numbers with a rational as the imaginary part" $ do
                assertParseComplexNumber "+5/2i" (Complex (Integer 0) (Rational 5 2))
                assertParseComplexNumber "-5/2i" (Complex (Integer 0) (Rational (-5) 2))
                assertParseComplexNumber "0+5/2i" (Complex (Integer 0) (Rational 5 2))
                assertParseComplexNumber "0-5/2i" (Complex (Integer 0) (Rational (-5) 2))
            it "parses strictly complex numbers" $ do
                assertParseComplexNumber "1+1i" (Complex (Integer 1) (Integer 1))
                assertParseComplexNumber "1+1.0i" (Complex (Integer 1) (Float 1.0))
                assertParseComplexNumber "1+5/2i" (Complex (Integer 1) (Rational 5 2))
                assertParseComplexNumber "1.0+1i" (Complex (Float 1.0) (Integer 1))
                assertParseComplexNumber "5/2+1i" (Complex (Rational 5 2) (Integer 1))
            it "parses complex numbers with exactness" $ do
                assertExactness "1+1i" (Complex (Integer 1) (Integer 1))
                assertExactness "1+1.0i" (Complex (Integer 1) (Float 1.0))
                assertExactness "1+5/2i" (Complex (Integer 1) (Rational 5 2))
                assertExactness "1.0+1i" (Complex (Float 1.0) (Integer 1))
                assertExactness "5/2+1i" (Complex (Rational 5 2) (Integer 1))
            it "parses complex numbers with different bases" $ do
                assertParseComplexNumber "#d1+1i" (Complex (Integer 1) (Integer 1))
                assertParseComplexNumber "#d1+1.0i" (Complex (Integer 1) (Float 1.0))
                assertParseComplexNumber "#d1+5/2i" (Complex (Integer 1) (Rational 5 2))
                assertParseComplexNumber "#d1.0+1i" (Complex (Float 1.0) (Integer 1))
                assertParseComplexNumber "#d5/2+1i" (Complex (Rational 5 2) (Integer 1))
                assertParseComplexNumber "#b1011+1101i" (Complex (Integer 11) (Integer 13))
                assertParseComplexNumber "#b1011+101/10i" (Complex (Integer 11) (Rational 5 2))
                assertParseComplexNumber "#b101/10+1011i" (Complex (Rational 5 2) (Integer 11))
                assertParseComplexNumber "#o13+15i" (Complex (Integer 11) (Integer 13))
                assertParseComplexNumber "#o13+15/21i" (Complex (Integer 11) (Rational 13 17))
                assertParseComplexNumber "#o15/21+13i" (Complex (Rational 13 17) (Integer 11))
                assertParseComplexNumber "#hb+di" (Complex (Integer 11) (Integer 13))
                assertParseComplexNumber "#hb+d/11i" (Complex (Integer 11) (Rational 13 17))
                assertParseComplexNumber "#hd/11+bi" (Complex (Rational 13 17) (Integer 11))
            it "parses complex numbers with different bases and exactness" $ do
                assertExactnessWithBase 'd' "1+1i" (Complex (Integer 1) (Integer 1))
                assertExactnessWithBase 'd' "1+1.0i" (Complex (Integer 1) (Float 1.0))
                assertExactnessWithBase 'd' "1+5/2i" (Complex (Integer 1) (Rational 5 2))
                assertExactnessWithBase 'd' "1.0+1i" (Complex (Float 1.0) (Integer 1))
                assertExactnessWithBase 'd' "5/2+1i" (Complex (Rational 5 2) (Integer 1))
                assertExactnessWithBase 'b' "1011+1101i" (Complex (Integer 11) (Integer 13))
                assertExactnessWithBase 'b' "1011+101/10i" (Complex (Integer 11) (Rational 5 2))
                assertExactnessWithBase 'b' "101/10+1011i" (Complex (Rational 5 2) (Integer 11))
                assertExactnessWithBase 'o' "13+15i" (Complex (Integer 11) (Integer 13))
                assertExactnessWithBase 'o' "13+15/21i" (Complex (Integer 11) (Rational 13 17))
                assertExactnessWithBase 'o' "15/21+13i" (Complex (Rational 13 17) (Integer 11))
                assertExactnessWithBase 'h' "b+di" (Complex (Integer 11) (Integer 13))
                assertExactnessWithBase 'h' "b+d/11i" (Complex (Integer 11) (Rational 13 17))
                assertExactnessWithBase 'h' "d/11+bi" (Complex (Rational 13 17) (Integer 11))
        context "when parsing character" $ do
            it "parses character literals" $ do
                testParse "#\\a" (Character 'a')
                testParse "#\\ " (Character ' ')
                testParse "#\\space" (Character ' ')
                testParse "#\\newline" (Character '\n')
        -- When lists can be parsed, we can use `parse'` to define our expectations.
        -- This means we do not have to specify `LispVal` values manually, which can get quite
        -- complex to manipulate by hand.
        context "when parsing lists" $ do
            it "parses empty lists" $ do
                testParse "()" (List [])
            it "parses non-empty lists" $ do
                testParse "(#e1 #\\a \"foo\")"
                    (List [ LispNumber (Complex (Integer 1) (Integer 0)) Exact
                          , Character 'a'
                          , String "foo"])
        context "when parsing dotted lists" $ do
            it "parses dotted pairs" $ do
                testParse "(#\\a . 1)"
                          (DottedList [Character 'a']
                                      (LispNumber (Complex (Integer 1) (Integer 0)) Inexact))
                testParse "(\"first\" #\\b . 1)"
                        (DottedList [String "first", Character 'b']
                                    (LispNumber (Complex (Integer 1) (Integer 0)) Inexact))
        context "when parsing quoted expressions" $ do
            it "parses quoted numbers" $ do
                testParse "'1" (List [Atom "quote", integerToLisp 1])
            it "parses quoted lists" $ do
                testParse "'(#\\a 1)"
                    (List [ Atom "quote"
                          , List [Character 'a', LispNumber (Complex (Integer 1) (Integer 0)) Inexact]])
            it "parses quasiquoted lists" $ do
                assertExpandParse "`(list ,(+ 1 2) 4)"
                    "(backquote (list (unquote (+ 1 2)) 4))"
                assertExpandParse "(let ((name 'a)) `(list ,name ',name))"
                    "(let ((name (quote a))) (backquote (list (unquote name) (quote (unquote name)))))"
                assertExpandParse "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"
                    "(backquote (a (unquote (+ 1 2)) (unquote-splicing (map abs (quote (4 -5 6)))) b))"
                assertExpandParse "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))"
                    "(backquote ((foo (unquote (- 10 3))) (unquote-splicing (cdr (quote (c)))) . (unquote (car (quote (cons))))))"
                -- Not yet supported?
                -- testParse "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)"
                --    (parse' "(backquote #(10 5 (unquote (sqrt 4)) (unquote-splicing (map sqrt (quote (16 9)))) 8))")
                assertExpandParse "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"
                    "(backquote (a (backquote (b (unquote (+ 1 2)) (unquote (foo (unquote (+ 1 3)) d)) e)) f))"
                assertExpandParse "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))"
                    "(let ((name1 (quote x)) (name2 (quote y))) (backquote (a (backquote (b ,(unquote name1) (unquote (quote (unquote name2))) d)) e)))"
