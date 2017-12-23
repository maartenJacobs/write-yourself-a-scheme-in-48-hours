import System.Environment
import Scheme.Core
import Scheme.Parser
import Scheme.Evaluator
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
