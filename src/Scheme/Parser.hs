{-# LANGUAGE MultiWayIf #-}

module Scheme.Parser where

import Scheme.Core
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric    (readHex, readFloat)
import Data.Maybe (fromMaybe, isJust)

data Base = Binary | Octal | Decimal | Hexadecimal
type NumberPrefix = (Base, Exactness)

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

parseNumber :: Parser LispVal
parseNumber = do
    (base, exactness) <- parseNumberPrefix
    complex <- parseComplex base
    return $ LispNumber complex exactness

-- | 'parseNumberPrefix' parses the base of a number and its exactness. Both are
-- optional, and can be ordered as either base and exactness, or exactness and base.
parseNumberPrefix :: Parser NumberPrefix
parseNumberPrefix = parsePrefix <|> noPrefix
    where noPrefix = return (Decimal, Inexact)
          parseFirst = char '#' >> oneOf "bodhie"
          parseSecond :: Char -> Parser (Maybe Char)
          parseSecond first = optionMaybe (char '#' >> oneOf (if first `elem` "bodh" then "ie" else "bodh"))
          parsePrefix = do
            -- (base, exactness) <- orderModifiers <$> parseFirst <*> parseSecond ?
            first  <- parseFirst
            second <- parseSecond first
            let (base, exactness) = orderModifiers first second
            return (baseFromModifier base, exactnessFromModifier exactness)
          -- | 'orderModifiers' creates a pair of the base and exactness, in that order.
          orderModifiers first second =
            if first `elem` "bodh"
                then (first, fromMaybe 'i' second)
                else (fromMaybe 'd' second, first)

parseComplex :: Base -> Parser ComplexNumber
parseComplex base = try parseBoth
                <|> try parseImag
                <|> Complex <$> parseReal base <*> return (Integer 0)
    where parseImagWithReal = do
            imagSign <- oneOf "+-"
            uimag <- option (Integer 1) (parseUnsignedReal base)
            char 'i'
            return $ applySign imagSign uimag
          parseBoth = do
            real <- parseReal base
            imag <- parseImagWithReal
            return $ Complex real imag
          parseImag = do
            imag <- parseImagWithReal
            return $ Complex (Integer 0) imag

parseImaginary :: Base -> Parser ComplexNumber
parseImaginary base = do
    real <- option (Integer 0) (try $ parseReal base)
    imagSign <- oneOf "+-"
    imag <- option (Integer 1) (try $ parseUnsignedReal base)
    char 'i'
    return $ Complex real (applySign imagSign imag)

parseReal :: Base -> Parser SimpleNumber
parseReal base = do
    sign <- option '+' (oneOf "+-")
    applySign sign <$> parseUnsignedReal base

parseUnsignedReal :: Base -> Parser SimpleNumber
parseUnsignedReal Decimal = try (parseRational Decimal) <|> try parseDecimal <|> (Integer <$> parseUnsignedInteger Decimal)
parseUnsignedReal base    = try (parseRational base) <|> (Integer <$> parseUnsignedInteger base)

parseRational :: Base -> Parser SimpleNumber
parseRational base = do
    num <- parseUnsignedInteger base
    char '/'
    den <- parseUnsignedInteger base
    return $ simplifyRational num den

simplifyRational :: Integer -> Integer -> SimpleNumber
simplifyRational num den | num `mod` den == 0 = Integer (num `div` den)
                         | otherwise          = Rational num den

parseUnsignedInteger :: Base -> Parser Integer
parseUnsignedInteger Binary      = readBase 2 <$> many1 (oneOf "01")
parseUnsignedInteger Octal       = readBase 8 <$> many1 (oneOf ['0'..'7'])
parseUnsignedInteger Decimal     = read <$> many1 digit
parseUnsignedInteger Hexadecimal = (fst . head . readHex) <$> many1 (oneOf (['0'..'9'] ++ ['a'..'f']))

parseDecimal :: Parser SimpleNumber
parseDecimal = try parseFloat <|> (Integer <$> parseUnsignedInteger Decimal)

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

baseFromModifier :: Char -> Base
baseFromModifier 'b' = Binary
baseFromModifier 'o' = Octal
baseFromModifier 'd' = Decimal
baseFromModifier 'h' = Hexadecimal

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

parseList :: Parser LispVal
parseList = do
    (exprs, dotEnd) <- parseListElementsUntilEnd
    if | null exprs    -> return (List exprs)
       | isJust dotEnd -> DottedList exprs <$> parseExpr
       | otherwise     -> return $ List exprs

parseListElementsUntilEnd :: Parser ([LispVal], Maybe Char)
parseListElementsUntilEnd = loop ([], Nothing)
    where loop :: ([LispVal], Maybe Char) -> Parser ([LispVal], Maybe Char)
          loop (exprs, _) =
            do
                expr <- optionMaybe parseExpr
                maybe (return (exprs, Nothing))
                      (\expr -> do
                        let exprs' = exprs ++ [expr]
                        sp <- optionMaybe spaces
                        maybe (return (exprs', Nothing))
                              (const $ parseDotOrNext exprs' (loop (exprs', Nothing)))
                              sp)
                      expr

parseDotOrNext :: [LispVal] -> Parser ([LispVal], Maybe Char) -> Parser ([LispVal], Maybe Char)
parseDotOrNext exprs loop = optionMaybe (char '.' >> spaces) >>= maybe loop (const matchedDot)
    where matchedDot = return (exprs, Just '.')

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    splice <- optionMaybe (char '@')
    x <- parseExpr
    let unquoteOp = case splice of (Just _) -> "unquote-splicing"; Nothing  -> "unquote"
    return $ List [Atom unquoteOp, x]

parseBackQuoted :: Parser LispVal
parseBackQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "backquote", x]

parseExpr :: Parser LispVal
parseExpr = try parseNumber
         <|> try parseCharacter
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> parseUnquoted
         <|> parseBackQuoted
         <|> between (char '(') (char ')') parseList

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
