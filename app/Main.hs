import System.Environment
import Scheme.Core

main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
