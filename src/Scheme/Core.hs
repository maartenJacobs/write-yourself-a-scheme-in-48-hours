module Scheme.Core where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many parseStringChar
    char '"'
    return $ String x

parseStringChar :: Parser Char
parseStringChar = parseEscapeChars <|> noneOf "\""

parseEscapeChars :: Parser Char
parseEscapeChars = char '\\' >> oneOf "ntr\\\"" >>= return . unescape
    where unescape 'n' = '\n'
          unescape 'r' = '\r'
          unescape 't' = '\t'
          unescape c   = c

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

readBase :: Integer -> String -> Integer
readBase base = foldl (\acc c -> acc * base + read [c]) 0

parseNumberWithBase :: Parser LispVal
parseNumberWithBase = do
    char '#'
    base <- oneOf "bodh"
    num <- case base of
        'b' -> many1 (oneOf "01") >>= return . readBase 2
        'o' -> many1 (oneOf ['0'..'7']) >>= return . readBase 8
        'd' -> parseDecimal
        'h' -> many1 (oneOf (['0'..'9'] ++ ['A'..'F'])) >>= return . fst . head . readHex
    return $ Number num

parseDecimal :: Parser Integer
parseDecimal = many1 digit >>= return . read

parseNumber :: Parser LispVal
parseNumber = try parseNumberWithBase <|> (parseDecimal >>= return . Number)

parseExpr :: Parser LispVal
parseExpr = parseNumber
         <|> parseAtom
         <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
