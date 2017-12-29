module Scheme.Runtime where

import Scheme.Core
import Data.IORef
import Data.Maybe (maybe)
import qualified Control.Monad.Error as MErr -- Deprecated; TODO: use Control.Monad.Except instead

type Env = IORef [(String, IORef LispVal)]

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
