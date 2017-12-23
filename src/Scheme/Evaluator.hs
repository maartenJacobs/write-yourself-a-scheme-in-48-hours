module Scheme.Evaluator where

import Scheme.Core
import Data.Maybe (maybe)

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _ _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", foldl1 addNumber)]

addNumber :: LispVal -> LispVal -> LispVal
addNumber (Number (Complex real1 imag1) ex1) (Number (Complex real2 imag2) ex2) =
    Number (Complex (addSimple real1 real2) (addSimple imag1 imag2)) (combineExact ex1 ex2)

addSimple :: SimpleNumber -> SimpleNumber -> SimpleNumber
addSimple (Integer i) (Integer j) = Integer (i + j)
addSimple (Rational n1 d1) (Rational n2 d2) = Rational (n1' + n2') commonDen
    where commonDen = max d1 d2
          n1' = n1 * commonDen `div` d1
          n2' = n2 * commonDen `div` d2
addSimple (Float i) (Float j) = Float (i + j)
addSimple a b = addSimple a' b'
    where [a', b'] = unifyNumberTypes [a, b]

combineExact :: Exactness -> Exactness -> Exactness
combineExact Exact Exact = Exact
combineExact _ _ = Inexact
