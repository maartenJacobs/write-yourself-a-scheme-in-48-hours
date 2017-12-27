module Scheme.Evaluator where

import Scheme.Core
import Data.Maybe (maybe)
import qualified Data.Ratio as Ratio
import Control.Lens.Extras (is)
import qualified Control.Monad.Error as MErr -- Deprecated; TODO: use Control.Monad.Except instead
import Data.List (find)

type LispOp = [LispVal] -> ThrowsError LispVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _ _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
            Bool False -> eval alt
            otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = MErr.throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> LispOp
apply func args = maybe (MErr.throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, LispOp)]
primitives = [
        ("+", checkTypeOp allNumber $ return . foldl1 (genericNumOp (+)))
      , ("-", checkTypeOp allNumber $ return . foldl1 (genericNumOp (-)))
      , ("boolean?", unaryBooleanOp $ return . is _Bool)
      , ("pair?", unaryBooleanOp $ return . is _DottedList)
      , ("list?", unaryBooleanOp $ return . is _List)
      , ("symbol?", unaryBooleanOp $ return . is _Atom)
      , ("char?", unaryBooleanOp $ return . is _Character)
      , ("exact?", checkTypeOp allNumber (unaryBooleanOp $ return . isExact))
      , ("symbol->string", checkTypeOp allSymbol $ unaryOp symbolToString)
      , ("string->symbol", checkTypeOp allString $ unaryOp stringToSymbol)
      , ("=", numBooleanBinop (==))
      , ("<", numBooleanBinop (<))
      , (">", numBooleanBinop (>))
      , ("/=", numBooleanBinop (/=))
      , (">=", numBooleanBinop (>=))
      , ("<=", numBooleanBinop (<=))
      , ("||", boolBooleanBinOp (\(Bool a) (Bool b) -> a || b))
      , ("&&", boolBooleanBinOp (\(Bool a) (Bool b) -> a && b))
      , ("string=?", stringBooleanBinOp (==))
      , ("string<?", stringBooleanBinOp (<))
      , ("string>?", stringBooleanBinOp (>))
      , ("string<=?", stringBooleanBinOp (<=))
      , ("string>=?", stringBooleanBinOp (>=))
      , ("car", car)
      , ("cdr", cdr)
      , ("cons", cons)
      , ("eq?", eqv)
      , ("eqv?", eqv)
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

isExact :: LispVal -> Bool
isExact (Number _ Exact) = True
isExact _                = False

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom s) = return $ String s

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s

unaryOp :: (LispVal -> ThrowsError LispVal) -> LispOp
unaryOp op [v] = op v
unaryOp _  vs  = MErr.throwError $ NumArgs 1 vs

binaryOp :: (LispVal -> LispVal -> ThrowsError LispVal) -> LispOp
binaryOp op [v1, v2] = op v1 v2
binaryOp _  vs       = MErr.throwError $ NumArgs 2 vs

unaryBooleanOp :: (LispVal -> ThrowsError Bool) -> LispOp
unaryBooleanOp op = unaryOp (fmap Bool . op)

binaryBooleanOp :: (LispVal -> LispVal -> ThrowsError Bool) -> LispOp
binaryBooleanOp op = binaryOp (\arg1 arg2 -> Bool <$> op arg1 arg2)

numBooleanBinop :: (LispVal -> LispVal -> Bool) -> LispOp
numBooleanBinop op = checkTypeOp allNumber (binaryBooleanOp (\arg1 arg2 -> return (op arg1 arg2)))

boolBooleanBinOp :: (LispVal -> LispVal -> Bool) -> LispOp
boolBooleanBinOp op = checkTypeOp allBool (binaryBooleanOp (\arg1 arg2 -> return (op arg1 arg2)))

stringBooleanBinOp :: (LispVal -> LispVal -> Bool) -> LispOp
stringBooleanBinOp op = checkTypeOp allString (binaryBooleanOp (\arg1 arg2 -> return (op arg1 arg2)))

type TypeChecker = [LispVal] -> Maybe LispError

allBool :: TypeChecker
allBool args = maybe Nothing (Just . TypeMismatch "bool") (find (not . is _Bool) args)

allNumber :: TypeChecker
allNumber args = maybe Nothing (Just . TypeMismatch "number") (find (not . is _Number) args)

allString :: TypeChecker
allString args = maybe Nothing (Just . TypeMismatch "string") (find (not . is _String) args)

allSymbol :: TypeChecker
allSymbol args = maybe Nothing (Just . TypeMismatch "symbol") (find (not . is _Atom) args)

checkTypeOp :: TypeChecker -> LispOp -> LispOp
checkTypeOp chkType op args = maybe (op args) MErr.throwError (chkType args)

car :: LispOp
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = MErr.throwError $ TypeMismatch "pair" badArg
car badArgList              = MErr.throwError $ NumArgs 1 badArgList

cdr :: LispOp
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = MErr.throwError $ TypeMismatch "pair" badArg
cdr badArgList              = MErr.throwError $ NumArgs 1 badArgList

cons :: LispOp
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = MErr.throwError $ NumArgs 2 badArgList

eqv :: LispOp
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1 _), (Number arg2 _)]     = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = MErr.throwError $ NumArgs 2 badArgList
