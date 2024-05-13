module Compiler.Parser.TokenParsers (parseNumConstant) where

import Compiler.Parser.Tokens

import Text.Parsec.Combinator
import Text.Parsec.Prim

import Data.Functor

-- import Data.List qualified as L

import Data.Text qualified as T

import Data.Char

import Text.Parsec

{-
digit :: Stream s m Char => ParsecT s u m Char
hexDigit :: Stream s m Char => ParsecT s u m Char
octDigit :: Stream s m Char => ParsecT s u m Char
-}

parseNumConstant :: T.Text -> Either ParseError Constant
parseNumConstant =
    parse
        ( choice
            [ -- floatingConstant,
              integerConstant
            ]
        )
        ""

integerConstant :: Parsec T.Text () Constant
integerConstant = do
    (val, rep) <- choice [(,Decimal) <$> decimalConstant, (,Octal) <$> octalConstant, (,Hex) <$> hexConstant]
    (size, sign) <- option (Standard, Signed) integerSuffix
    pure (IntConst val rep sign size)

hexPrefix :: Parsec T.Text () ()
hexPrefix = char '0' >> oneOf "xX" $> ()

decimalConstant :: Parsec T.Text () Integer
decimalConstant = read <$> liftA2 (:) (option '0' $ oneOf "123456789") (many digit)

octalConstant :: Parsec T.Text () Integer
octalConstant = choice [char '0' $> 0, toNum octDigit 0]

toNum :: Parsec T.Text () Char -> Integer -> Parsec T.Text () Integer
toNum getDigit i =
    optionMaybe getDigit >>= \case
        Just d -> toNum getDigit (10 * i + toInteger (digitToInt d))
        Nothing -> pure i

-- hexConstant :: (Stream T.Text m Char) => Parsec s () String
hexConstant :: Parsec T.Text () Integer
hexConstant = hexPrefix >> toNum hexDigit 0

unsignedSuffix :: Parsec T.Text () Sign
unsignedSuffix = oneOf "uU" $> Unsigned

longSuffix :: Parsec T.Text () Size
longSuffix = oneOf "lL" $> Long

longlongSuffix :: Parsec T.Text () Size
longlongSuffix = choice [char 'L' >> char 'L', char 'l' >> char 'l'] $> LongLong

integerSuffix :: Parsec T.Text () (Size, Sign)
integerSuffix = do
    choice
        [ liftA2 (\a b -> (b, a)) unsignedSuffix $ choice [option Standard longSuffix, longlongSuffix]
        , liftA2 (,) longSuffix $ option Signed unsignedSuffix
        , liftA2 (,) longlongSuffix $ option Signed unsignedSuffix
        ]

{-
floatingConstant :: Parsec T.Text () Constant
floatingConstant = choice [hexFloatingConstant, decimalFloatingConstant] -- , hexFloatingConstant]

decimalFloatingConstant :: Parsec T.Text () Constant
decimalFloatingConstant =
    choice
        [ FloatConst <$> fractionalConstant <*> optionMaybe exponentPart <*> option Double floatingSuffix
        , FloatConst <$> ((,0) <$> digitSequence digit 10) <*> (Just <$> exponentPart) <*> option Double floatingSuffix
        ]

hexFloatingConstant :: Parsec T.Text () Constant
hexFloatingConstant =
    hexPrefix
        >> choice
            [ FloatConst <$> hexFractionalConstant <*> optionMaybe binaryExponentPart <*> option Double floatingSuffix
            , FloatConst <$> ((,0) <$> hexDigitSequence) <*> (Just <$> binaryExponentPart) <*> option Double floatingSuffix
            ]

-- decimalChar :: Parsec

digitSequence :: Parsec T.Text () Char -> Int -> Parsec T.Text () Int
digitSequence getDigit base = (L.foldl' (\acc a -> (acc * base) + ord a) 0) <$> many1 getDigit

hexDigitSequence = digitSequence hexDigit 16

fractionalConstant :: Parsec T.Text () (Int, Int)
fractionalConstant =
    choice
        [ liftA2 (,) (option 0 (digitSequence digit 10) <* char '.') (digitSequence digit 10)
        , (,0) <$> (digitSequence digit 10) <* char '.'
        ]

exponentPart :: Parsec T.Text () (Sign, Int)
exponentPart = do
    oneOf "eE" >> liftA2 (,) (option Signed floatSign) (digitSequence digit 10)

hexFractionalConstant :: Parsec T.Text () (Int, Int)
hexFractionalConstant =
    choice
        [ liftA2 (,) (option 0 hexDigitSequence <* char '.') hexDigitSequence
        , (,0) <$> hexDigitSequence <* char '.'
        ]

binaryExponentPart :: Parsec T.Text () (Sign, Int)
binaryExponentPart = oneOf "pP" >> liftA2 (,) (option Signed floatSign) hexDigitSequence

floatSign :: Parsec T.Text () Sign
floatSign = choice [char '+' $> Signed, char '-' $> Unsigned]

floatingSuffix :: Parsec T.Text () FloatSize
floatingSuffix = choice [oneOf "fF" $> Float, oneOf "lL" $> LongDouble]

-}