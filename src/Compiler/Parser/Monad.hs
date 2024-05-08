module Compiler.Parser.Monad where

import Control.Applicative as App (Applicative (..))
import Data.Text qualified
import Data.Word (Word8)

import Data.Bits qualified
import Data.Char (ord)

import Data.Text qualified as T

import Data.Map qualified as M

data AlexUserState = AlexUserState
    { symbol_table :: [M.Map T.Text ()]
    , currentFile :: String
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState{symbol_table = [M.empty], currentFile = ""}

alex_tab_size :: Int
alex_tab_size = 4

-- The following is from https://github.com/haskell/alex/blob/master/data/AlexWrappers.hs

-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
    (x, xs) -> (fromIntegral x, map fromIntegral xs)
  where
    go oc
        | oc <= 0x7f =
            ( oc
            , []
            )
        | oc <= 0x7ff =
            ( 0xc0 + (oc `Data.Bits.shiftR` 6)
            ,
                [ 0x80 + oc Data.Bits..&. 0x3f
                ]
            )
        | oc <= 0xffff =
            ( 0xe0 + (oc `Data.Bits.shiftR` 12)
            ,
                [ 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                , 0x80 + oc Data.Bits..&. 0x3f
                ]
            )
        | otherwise =
            ( 0xf0 + (oc `Data.Bits.shiftR` 18)
            ,
                [ 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                , 0x80 + oc Data.Bits..&. 0x3f
                ]
            )

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput =
    ( AlexPosn -- current position,
    , Char -- previous char
    , [Byte] -- pending bytes on current char
    , Data.Text.Text -- current input string
    )

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _ps, s) = (p, c, [], s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p, c, _bs, _s) = c

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, c, b : bs, s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) = case Data.Text.uncons s of
    Just (c, cs) ->
        let p' = alexMove p c
         in case utf8Encode' c of
                (b, bs) -> p' `seq` Just (b, (p', c, bs, cs))
    Nothing ->
        Nothing

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
    deriving stock (Eq, Show, Ord)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a + 1) l (c + alex_tab_size - ((c - 1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
alexMove (AlexPn a l c) _ = AlexPn (a + 1) l (c + 1)

-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)

data AlexState = AlexState
    { alex_pos :: !AlexPosn -- position at current input location
    , alex_inp :: Data.Text.Text
    , alex_chr :: !Char
    , alex_bytes :: [Byte]
    , alex_scd :: !Int -- the current startcode
    , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: Data.Text.Text -> Alex a -> Either String a
runAlex input__ (Alex f) =
    case f
        ( AlexState
            { alex_bytes = []
            , alex_pos = alexStartPos
            , alex_inp = input__
            , alex_chr = '\n'
            , alex_ust = alexInitUserState
            , alex_scd = 0
            }
        ) of
        Left msg -> Left msg
        Right (_, a) -> Right a

newtype Alex a = Alex {unAlex :: AlexState -> Either String (AlexState, a)}

instance Functor Alex where
    fmap f a = Alex $ \s -> case unAlex a s of
        Left msg -> Left msg
        Right (s', a') -> Right (s', f a')

instance Applicative Alex where
    pure a = Alex $ \s -> Right (s, a)
    fa <*> a = Alex $ \s -> case unAlex fa s of
        Left msg -> Left msg
        Right (s', f) -> case unAlex a s' of
            Left msg -> Left msg
            Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
    m >>= k = Alex $ \s -> case unAlex m s of
        Left msg -> Left msg
        Right (s', a) -> unAlex (k a) s'
    return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput =
    Alex $ \s@AlexState{alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp__} ->
        Right (s, (pos, c, bs, inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos, c, bs, inp__) =
    Alex $ \s -> case s{alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp__} of
        state__@(AlexState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust = ss}, ())

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd = sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd = sc}, ())
