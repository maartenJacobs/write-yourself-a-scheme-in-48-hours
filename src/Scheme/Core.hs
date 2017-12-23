module Scheme.Core where

-- | 'SimpleNumber' describes scalar numbers in increasing order of a number type tower.
data SimpleNumber = Integer Integer
                  | Rational Integer Integer
                  | Float Float
                  deriving (Show, Eq, Ord)

-- | 'Complex' describes a complex number, consisting of a real part and an imaginary part.
data Complex = Complex SimpleNumber SimpleNumber
             deriving (Show, Eq)

data Exactness = Exact | Inexact
               deriving (Show, Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Complex Exactness
             | String String
             | Bool Bool
             | Character Char
             deriving (Eq)

unifyNumberTypes :: [SimpleNumber] -> [SimpleNumber]
unifyNumberTypes ns = map (unifyNumberType (maximum ns)) ns

unifyNumberType :: SimpleNumber -> SimpleNumber -> SimpleNumber
unifyNumberType (Integer _) i@(Integer _) = i
unifyNumberType (Rational _ _) (Integer i) = Rational i 1
unifyNumberType (Rational _ _) r@(Rational _ _) = r
unifyNumberType (Float _) f@(Float _) = f
unifyNumberType (Float _) (Integer i) = Float (fromIntegral i :: Float)
unifyNumberType (Float _) (Rational num den) = Float (fromIntegral num / fromIntegral den)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Number (Complex real (Integer 0)) ex) = showExact ex ++ showNumber real
showVal (Number (Complex real imag) ex) = showExact ex ++ showNumber real ++ showImaginary imag
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showNumber :: SimpleNumber -> String
showNumber (Integer i) = show i
showNumber (Float f) = show f
showNumber (Rational num den) = show num ++ "/" ++ show den

showExact :: Exactness -> String
showExact Inexact = ""
showExact Exact = "#e"

showImaginary :: SimpleNumber -> String
showImaginary n | isPositive n = "+" ++ showNumber n
                | otherwise    = showNumber n

isPositive :: SimpleNumber -> Bool
isPositive (Integer i) = i >= 0
isPositive (Float f) = f >= 0
isPositive (Rational num _) = num >= 0