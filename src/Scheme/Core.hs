module Scheme.Core where

-- | 'SimpleNumber' describes scalar numbers.
data SimpleNumber = Integer Integer
                  | Float Float
                  | Rational Integer Integer
                  deriving (Show, Eq)

-- | 'ComplexNumber' describes a complex number, consisting of a
-- real part and an imaginary part.
data ComplexNumber = Complex SimpleNumber SimpleNumber
                   deriving (Show, Eq)

data Exactness = Exact | Inexact
               deriving (Show, Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | LispNumber ComplexNumber Exactness
             | String String
             | Bool Bool
             | Character Char
             deriving (Show, Eq)
