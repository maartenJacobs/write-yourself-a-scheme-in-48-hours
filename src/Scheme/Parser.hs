module Scheme.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readFloat)

-- | 'SimpleNumber' describes scalar numbers.
data SimpleNumber = Integer Integer
                  | Float Float
                  | Rational Integer Integer
                  deriving (Show, Eq)

-- | 'ComplexNumber' describes a complex number, consisting of a
-- real part and an imaginary part.
data ComplexNumber = Complex SimpleNumber SimpleNumber
                   deriving (Show, Eq)

data Exactness = Exact | Inexact
               deriving (Show, Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | ComplexLispNum ComplexNumber Exactness
             | SimpleLispNum SimpleNumber Exactness
             | String String
             | Bool Bool
             | Character Char
             deriving (Show, Eq)

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
parseEscapeChars = unescape <$> (char '\\' >> oneOf "ntr\\\"")
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

-- | 'readBase' takes a base of 1 to 10 to convert a string of digits, specific
-- to the base, to an integer. The implementation is currently lazy.
-- For instance, base 8 and "56" is converted to 46. Base 2 and "1101" is converted
-- to 13.
readBase :: Integer -> String -> Integer
readBase base | base >= 1 && base <= 10 =
    foldl (\acc c -> acc * base + read [c]) 0

parseNumberWithBase :: Parser LispVal
parseNumberWithBase = do
    firstModifier <- char '#' >> oneOf "bodhie"
    secondModifier <- (Just <$> parseOtherModifier firstModifier) <|> return Nothing
    let (base, exactness) = determineBaseAndExactness firstModifier secondModifier
    sign <- parseSign
    num <- case base of
        'b' -> readBase 2 <$> many1 (oneOf "01")
        'o' -> readBase 8 <$> many1 (oneOf ['0'..'7'])
        'd' -> parseDecimal
        'h' -> (fst . head . readHex) <$> many1 (oneOf (['0'..'9'] ++ ['a'..'f']))
        _   -> fail "Unexpected base"
    let lispNum = applySign sign (Integer num) 
    let lispVal = SimpleLispNum lispNum exactness
    return lispVal
    where parseOtherModifier :: Char -> Parser Char
          parseOtherModifier firstModifier = 
            if firstModifier `elem` "bodh"
                then char '#' >> oneOf "ie"
                else char '#' >> oneOf "bodh"
          determineBaseAndExactness :: Char -> Maybe Char -> (Char, Exactness)  
          determineBaseAndExactness firstModifier Nothing = (firstModifier, Inexact)
          determineBaseAndExactness firstModifier (Just secondModifier) =
            if firstModifier `elem` "bodh"
                then (firstModifier, exactnessFromModifier secondModifier)
                else (secondModifier, exactnessFromModifier firstModifier)

parseDecimal :: Parser Integer
parseDecimal = read <$> many1 digit

parseNumber :: Parser LispVal
parseNumber = try parseNumberWithBase <|>
    (do exactness <- parseExactness
        sign <- parseSign
        ds <- parseDecimal
        let lispVal = SimpleLispNum (applySign sign (Integer ds)) exactness
        return lispVal)

parseLispFloat :: Parser LispVal
parseLispFloat = do
    exactness <- parseExactness
    sign <- parseSign
    float <- parseFloat
    return $ SimpleLispNum (applySign sign float) exactness

parseFloat :: Parser SimpleNumber
parseFloat = do
    is <- many digit
    char '.'
    let restParse = if null is then many1 else many
    ds <- restParse digit
    return $ readFloat' is ds id

applySign :: Char -> SimpleNumber -> SimpleNumber
applySign sign n =
    case n of
        Integer i    -> Integer (signOp sign i)
        Float f      -> Float (signOp sign f)
        Rational n d -> Rational (signOp sign n) d
    where signOp :: Num a => Char -> a -> a
          signOp '+' = id
          signOp '-' = negate

exactnessFromModifier :: Char -> Exactness
exactnessFromModifier 'i' = Inexact
exactnessFromModifier 'e' = Exact

parseExactness :: Parser Exactness
parseExactness = exactnessFromModifier <$> ((char '#' >> oneOf "ie") <|> return 'i')

parseSign :: Parser Char
parseSign = try (oneOf "+-") <|> return '+'

-- | The readFloat' function takes the digits before the decimal point and the digits
-- after the decimal points, and uses 'Numeric.readFloat' to convert these parts into
-- a 'Float'. The 'sign' is then added to the result and wrapped in the relevant
-- 'LispVal' constructor.
readFloat' :: String -> String -> (Float -> Float) -> SimpleNumber
readFloat' [] ds sign = readFloat' "0" ds sign
readFloat' is [] sign = readFloat' is "0" sign
readFloat' is ds sign = Float . sign . fst . head $ readFloat (is ++ "." ++ ds)

parseCharacter :: Parser LispVal
parseCharacter = string "#\\" >> (characterName <|> character)
    where character = Character <$> anyChar
          characterName = (string "space" >> return (Character ' '))
                        <|> (string "newline" >> return (Character '\n'))

parseExpr :: Parser LispVal
parseExpr = try parseLispFloat
         <|> try parseNumber
         <|> try parseCharacter
         <|> parseAtom
         <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
