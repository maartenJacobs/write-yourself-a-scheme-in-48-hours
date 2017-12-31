import System.Environment
import Scheme.Core
import Scheme.Parser
import Scheme.Evaluator
import Control.Monad (liftM)
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
      env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
      (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
            >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
