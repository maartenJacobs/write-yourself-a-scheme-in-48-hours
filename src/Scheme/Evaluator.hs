module Scheme.Evaluator where

import Scheme.Core
import Data.Maybe (maybe)
import qualified Data.Ratio as Ratio
import Control.Lens.Extras (is)
import qualified Control.Monad.Error as MErr -- Deprecated; TODO: use Control.Monad.Except instead

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _ _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = MErr.throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (MErr.throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
        ("+", return . foldl1 (genericNumOp (+)))
      , ("-", return . foldl1 (genericNumOp (-)))
      , ("boolean?", booleanOp $ all (is _Bool))
      , ("pair?", booleanOp $ all (is _DottedList))
      , ("list?", booleanOp $ all (is _List))
      , ("symbol?", booleanOp $ all (is _Atom))
      , ("char?",  booleanOp $ all (is _Character))
      , ("exact?", booleanOp isExact)
      , ("symbol->string", singleArgOp symbolToString)
      , ("string->symbol", singleArgOp stringToSymbol)
    ]

genericNumOp :: (SimpleNumber -> SimpleNumber -> SimpleNumber) -> LispVal -> LispVal -> LispVal
genericNumOp op (Number (Complex real1 imag1) ex1) (Number (Complex real2 imag2) ex2) =
    Number (Complex (op real1' real2') (op imag1' imag2')) (combineExact ex1 ex2)
    where [real1', real2'] = unifyNumberTypes [real1, real2]
          [imag1', imag2'] = unifyNumberTypes [imag1, imag2]

combineExact :: Exactness -> Exactness -> Exactness
combineExact Exact Exact = Exact
combineExact _ _ = Inexact

instance Num SimpleNumber where
    -- + :: SimpleNumber -> SimpleNumber -> SimpleNumber
    (+) (Integer i) (Integer j) = Integer (i + j)
    (+) (Rational r1) (Rational r2) = Rational (r1 + r2)
    (+) (Float i) (Float j) = Float (i + j)

    -- * :: SimpleNumber -> SimpleNumber -> SimpleNumber
    (*) (Integer i) (Integer j) = Integer (i * j)
    (*) (Rational r1) (Rational r2) = Rational (r1 * r2)
    (*) (Float i) (Float j) = Float (i * j)

    -- abs :: SimpleNumber -> SimpleNumber
    abs (Integer i) = Integer (abs i)
    abs (Float f) = Float (abs f)
    abs (Rational r) = Rational (abs r)

    -- signum :: SimpleNumber -> SimpleNumber
    signum (Integer i) = Integer (signum i)
    signum (Float f) = Integer (round $ signum f)
    signum (Rational r) = Integer (Ratio.numerator $ signum r)

    -- fromInteger :: Integer -> SimpleNumber
    fromInteger = Integer

    -- negate :: SimpleNumber -> SimpleNumber
    negate (Integer i) = Integer (negate i)
    negate (Float f) = Float (negate f)
    negate (Rational r) = Rational (negate r)

booleanOp :: ([LispVal] -> Bool) -> [LispVal] -> ThrowsError LispVal
booleanOp op args = return . Bool $ op args

isExact :: [LispVal] -> Bool
isExact [(Number _ Exact)] = True
isExact _                  = False

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom s) = return $ String s

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s

singleArgOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
singleArgOp op [v] = op v
singleArgOp _  vs  = MErr.throwError $ NumArgs 1 vs
