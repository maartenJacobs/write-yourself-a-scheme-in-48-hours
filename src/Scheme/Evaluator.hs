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
primitives = [
        ("+", foldl1 (genericNumOp (+)))
      , ("-", foldl1 (genericNumOp (-)))
      , ("boolean?", booleanOp isBoolean)
      , ("pair?", booleanOp isPair)
      , ("list?", booleanOp isList)
    ]

genericNumOp :: (SimpleNumber -> SimpleNumber -> SimpleNumber) -> LispVal -> LispVal -> LispVal
genericNumOp op (Number (Complex real1 imag1) ex1) (Number (Complex real2 imag2) ex2) =
    Number (Complex (op real1' real2') (op imag1' imag2')) (combineExact ex1 ex2)
    where [real1', real2'] = unifyNumberTypes [real1, real2]
          [imag1', imag2'] = unifyNumberTypes [imag1, imag2]

combineExact :: Exactness -> Exactness -> Exactness
combineExact Exact Exact = Exact
combineExact _ _ = Inexact

incrToCommonBase :: SimpleNumber -> SimpleNumber -> SimpleNumber
incrToCommonBase (Rational srcNum srcDen) (Rational dstNum dstDen) = Rational srcNum' commonDen
    where commonDen = max srcDen dstDen
          srcNum' = srcNum * commonDen `div` srcDen

instance Num SimpleNumber where
    -- + :: SimpleNumber -> SimpleNumber -> SimpleNumber
    (+) (Integer i) (Integer j) = Integer (i + j)
    (+) r1@(Rational _ _) r2@(Rational _ _) =
        let (Rational n1 d) = incrToCommonBase r1 r2
            (Rational n2 _) = incrToCommonBase r1 r2
        in Rational (n1 + n2) d
    (+) (Float i) (Float j) = Float (i + j)

    -- * :: SimpleNumber -> SimpleNumber -> SimpleNumber
    (*) (Integer i) (Integer j) = Integer (i * j)
    (*) r1@(Rational _ _) r2@(Rational _ _) =
        let (Rational n1 d) = incrToCommonBase r1 r2
            (Rational n2 _) = incrToCommonBase r1 r2
        in Rational (n1 * n2) d
    (*) (Float i) (Float j) = Float (i * j)

    -- abs :: SimpleNumber -> SimpleNumber
    abs (Integer i) = Integer (abs i)
    abs (Float f) = Float (abs f)
    abs (Rational n d) = Rational (abs n) d

    -- signum :: SimpleNumber -> SimpleNumber
    signum (Integer i) = Integer (signum i)
    signum (Float f) = Integer (round $ signum f)
    signum (Rational n _) = Integer (signum n)

    -- fromInteger :: Integer -> SimpleNumber
    fromInteger = Integer

    -- negate :: SimpleNumber -> SimpleNumber
    negate (Integer i) = Integer (negate i)
    negate (Float f) = Float (negate f)
    negate (Rational n d) = Rational (negate n) d

booleanOp :: ([LispVal] -> Bool) -> [LispVal] -> LispVal
booleanOp op args = Bool $ op args

isBoolean :: [LispVal] -> Bool
isBoolean [(Bool _)] = True
isBoolean _          = False

isPair :: [LispVal] -> Bool
isPair [(DottedList _ _)] = True
isPair _                = False

isList :: [LispVal] -> Bool
isList [(List _)] = True
isList _          = False
