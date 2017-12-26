{-|
Module:      Scheme.Core
Description: Core datatypes of Scheme

Scheme.Core defines the datatypes that Scheme can describe.
All of the datatypes are instances of 'Show'. Clients can use the instance
to display result values to the user and for debugging, as the constructed
'String' is similar to Scheme input.
-}
module Scheme.Core where

import qualified Data.Ratio as Ratio
import Data.Ratio ((%)) -- Qualified operators would be a pain.

-- | 'SimpleNumber' describes scalar numbers in increasing order of a number type tower.
data SimpleNumber = Integer Integer
                  | Rational Rational
                  | Float Float
                  deriving (Eq, Ord)

-- | 'Complex' describes a complex number, consisting of a real part and an imaginary part.
data Complex = Complex SimpleNumber SimpleNumber
             deriving (Eq)

data Exactness = Exact | Inexact
               deriving (Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Complex Exactness
             | String String
             | Bool Bool
             | Character Char
             deriving (Eq)

-- | 'LispVal' is an instance of 'Show' to standardise printing of Scheme values.
-- The format is the same as the Scheme input, but with a single space between
-- list elements, and only decimal numbers.
instance Show LispVal where
    -- show :: LispVal -> String
    show (String contents) = "\"" ++ contents ++ "\""
    show (Character c) = ['\'', c, '\'']
    show (Atom name) = name
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number c@(Complex _ _) ex) = show ex ++ show c
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show Complex where
    -- show :: Complex -> String
    show (Complex real (Integer 0)) = show real
    show (Complex real imag)        = show real ++ showImaginary imag
        where showImaginary :: SimpleNumber -> String
              showImaginary n | isPositive n = "+" ++ show n
                              | otherwise    = show n
              isPositive (Integer i) = i >= 0
              isPositive (Float f) = f >= 0
              isPositive (Rational r) = Ratio.numerator r >= 0

instance Show SimpleNumber where
    -- show :: SimpleNumber -> String
    show (Integer i) = show i
    show (Float f) = show f
    show (Rational r) = show (Ratio.numerator r) ++ "/" ++ show (Ratio.denominator r)

instance Show Exactness where
    -- show :: Exactness -> String
    show Inexact = ""
    show Exact = "#e"

unifyNumberTypes :: [SimpleNumber] -> [SimpleNumber]
unifyNumberTypes ns = map (unifyNumberType (maximum ns)) ns

unifyNumberType :: SimpleNumber -> SimpleNumber -> SimpleNumber
unifyNumberType (Integer _) i@(Integer _) = i
unifyNumberType (Rational _) (Integer i) = Rational (i%1)
unifyNumberType (Rational _) r@(Rational _) = r
unifyNumberType (Float _) f@(Float _) = f
unifyNumberType (Float _) (Integer i) = Float (fromIntegral i :: Float)
unifyNumberType (Float _) (Rational r) = Float (fromRational r :: Float)
