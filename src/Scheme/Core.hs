{-# LANGUAGE TemplateHaskell #-}

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
import qualified Control.Lens as Lens
import Text.ParserCombinators.Parsec (ParseError)
import qualified Control.Monad.Error as MErr -- Deprecated; TODO: use Control.Monad.Except instead
import Data.IORef
import Data.Maybe (maybe)

type Env = IORef [(String, IORef LispVal)]

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               -- deriving (Eq)

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
    show (PrimitiveFunc _) = "<primitive>"
    show (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++ (maybe "" (" . " ++) varargs) ++ ") ...)"

instance Eq LispVal where
    -- (==) :: LispVal -> LispVal -> Bool
    (==) (Atom s1) (Atom s2)     = s1 == s2
    (==) (List l1) (List l2)     = l1 == l2
    (==) (String s1) (String s2) = s1 == s2
    (==) (Bool b1) (Bool b2)     = b1 == b2
    (==) (Character c1) (Character c2)         = c1 == c2
    (==) (DottedList l1 e1) (DottedList l2 e2) = l1 == l2 && e1 == e2
    (==) _ _ = False

instance Ord LispVal where
    -- compare :: LispVal -> LispVal -> Ordering
    compare (Number (Complex real1 (Integer 0)) _) (Number (Complex real2 (Integer 0)) _) = real1 `compare` real2
    compare (String s1) (String s2) = s1 `compare` s2

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
    show _ = ""

instance Show LispError where
    -- show :: LispError -> String
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr

instance MErr.Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
type IOThrowsError = MErr.ErrorT LispError IO

-- | 'trapError' transforms a possible Lisp error into 'Right String'.
-- As a result, the return value will always be 'Right String', as 'Right' values
-- are passed through without modification.
trapError :: ThrowsError String -> ThrowsError String
trapError action = MErr.catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = MErr.runErrorT action >>= return . extractValue . trapError

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = MErr.throwError err
liftThrows (Right val) = return val

unifyNumberTypes :: [SimpleNumber] -> [SimpleNumber]
unifyNumberTypes ns = map (unifyNumberType (maximum ns)) ns

unifyNumberType :: SimpleNumber -> SimpleNumber -> SimpleNumber
unifyNumberType (Integer _) i@(Integer _) = i
unifyNumberType (Rational _) (Integer i) = Rational (i%1)
unifyNumberType (Rational _) r@(Rational _) = r
unifyNumberType (Float _) f@(Float _) = f
unifyNumberType (Float _) (Integer i) = Float (fromIntegral i :: Float)
unifyNumberType (Float _) (Rational r) = Float (fromRational r :: Float)

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- MErr.liftIO $ readIORef envRef
                         maybe (MErr.throwError $ UnboundVar "Getting an unbound variable" var)
                               (MErr.liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- MErr.liftIO $ readIORef envRef
                             maybe (MErr.throwError $ UnboundVar "Setting an unbound variable" var)
                                   (MErr.liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- MErr.liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else MErr.liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = MErr.liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

Lens.makePrisms ''LispVal
