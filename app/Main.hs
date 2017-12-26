import System.Environment
import Scheme.Core
import Scheme.Parser
import Scheme.Evaluator
import Control.Monad (liftM)

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
