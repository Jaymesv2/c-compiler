module Compiler.Parser.TokenParsers (parseNumConstant) where

import Compiler.Parser.Tokens

import Text.Parsec.Combinator
import Text.Parsec.Prim

import Data.Functor

import Data.List qualified as L

import Data.Text qualified as T

import Data.Char

import Text.Parsec

{-
digit :: Stream s m Char => ParsecT s u m Char
hexDigit :: Stream s m Char => ParsecT s u m Char
octDigit :: Stream s m Char => ParsecT s u m Char
-}

charsToInteger :: Integer -> [Char] -> Integer
charsToInteger base = L.foldl' (\acc a -> (acc * base) + toInteger (digitToInt a)) 0

parseNumConstant :: T.Text -> Either ParseError Constant
parseNumConstant =
    parse
        ( choice
            [ try floatingConstant -- backtracking might be necessary
            , integerConstant
            ]
        )
        ""

integerConstant :: Parsec T.Text () Constant
integerConstant = do
    -- (val, rep) <- choice [(,Octal) <$> octalConstant, (,Decimal) <$> decimalConstant, (,Hex) <$> hexConstant]
    (val, rep) <- choice [octalOrHexConstant, (,Decimal) <$> decimalConstant]
    (size, sign) <- option (Standard, Signed) integerSuffix
    pure (IntConst val rep sign size)

octalOrHexConstant :: Parsec T.Text () (Integer, NumRep)
octalOrHexConstant =
    char '0'
        >> choice
            [ oneOf "xX" >> (,Hex) . charsToInteger 16 <$> many1 hexDigit
            , (,Octal) . charsToInteger 8 <$> many octDigit
            ]

hexPrefix :: Parsec T.Text () ()
hexPrefix = char '0' >> oneOf "xX" $> ()

decimalConstant :: Parsec T.Text () Integer
decimalConstant = charsToInteger 10 <$> liftA2 (:) (oneOf "123456789") (many digit)

unsignedSuffix :: Parsec T.Text () Sign
unsignedSuffix = oneOf "uU" $> Unsigned

longOrLongLongSuffix :: Parsec T.Text () Size
longOrLongLongSuffix = choice [char 'L' >> option Long (char 'L' $> LongLong), char 'l' >> option Long (char 'l' $> LongLong)]

integerSuffix :: Parsec T.Text () (Size, Sign)
integerSuffix = do
    choice
        [ liftA2 (\a b -> (b, a)) unsignedSuffix $ option Standard longOrLongLongSuffix
        , liftA2 (,) longOrLongLongSuffix $ option Signed unsignedSuffix
        ]

floatingConstant :: Parsec T.Text () Constant
floatingConstant = choice [hexFloatingConstant, decimalFloatingConstant] -- , hexFloatingConstant]

decimalFloatingConstant :: Parsec T.Text () Constant
decimalFloatingConstant =
    choice
        [ try $ FloatConst <$> fractionalConstant <*> optionMaybe exponentPart <*> option Double floatingSuffix
        , FloatConst <$> ((,0) <$> decimalDigitSequence) <*> (Just <$> exponentPart) <*> option Double floatingSuffix
        ]

hexFloatingConstant :: Parsec T.Text () Constant
hexFloatingConstant =
    hexPrefix
        >> choice
            [ try $ FloatConst <$> hexFractionalConstant <*> optionMaybe binaryExponentPart <*> option Double floatingSuffix
            , FloatConst <$> ((,0) <$> hexDigitSequence) <*> (Just <$> binaryExponentPart) <*> option Double floatingSuffix
            ]

hexDigitSequence :: Parsec T.Text () Integer
hexDigitSequence = charsToInteger 16 <$> many1 hexDigit

decimalDigitSequence :: Parsec T.Text () Integer
decimalDigitSequence = charsToInteger 10 <$> many1 digit

fractionalConstant :: Parsec T.Text () (Integer, Integer)
fractionalConstant =
    choice
        [ liftA2 (,) (option 0 decimalDigitSequence <* char '.') decimalDigitSequence
        , (,0) <$> decimalDigitSequence <* char '.'
        ]

exponentPart :: Parsec T.Text () (Sign, Integer)
exponentPart = do
    oneOf "eE" >> liftA2 (,) (option Signed floatSign) decimalDigitSequence

hexFractionalConstant :: Parsec T.Text () (Integer, Integer)
hexFractionalConstant =
    choice
        [ liftA2 (,) (option 0 hexDigitSequence <* char '.') hexDigitSequence
        , (,0) <$> hexDigitSequence <* char '.'
        ]

binaryExponentPart :: Parsec T.Text () (Sign, Integer)
binaryExponentPart = oneOf "pP" >> liftA2 (,) (option Signed floatSign) hexDigitSequence

floatSign :: Parsec T.Text () Sign
floatSign = choice [char '+' $> Signed, char '-' $> Unsigned]

floatingSuffix :: Parsec T.Text () FloatSize
floatingSuffix = choice [oneOf "fF" $> Float, oneOf "lL" $> LongDouble]