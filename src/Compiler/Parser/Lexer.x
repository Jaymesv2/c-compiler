{
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module Compiler.Parser.Lexer (alexMonadScan, runAlex, AlexState, newAlexState, printTokens, alexConduit, alexConduitSource, alexPrintCondTokens, getCurrentFilePath) where

import Compiler.Parser
import Compiler.SymbolTable
--import Control.Applicative as App (Applicative (..))
--import Data.Maybe
import Data.Functor
--import Data.Text qualified
import Data.Word (Word8)

import Data.Bits qualified
import Data.Char (ord)

import Conduit


import qualified Data.Text as T
import qualified Data.Map as M


import Compiler.Parser.Tokens
import Compiler.Parser

import Compiler.Parser.SrcLoc

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
}
-- %action "AlexInput -> Int -> Eff es (Maybe PPToken)"

%action "AlexInput -> Int -> AlexPosn -> Eff es (Maybe (Located PPToken))"
%typeclass "(State AlexState :> es, Error String :> es)"

$digit = [0-9] 
$nzdigit = [1-9]
$octaldigit = [0-7]
$nondigit = [_a-zA-Z]
$hexdigit = [ $digit a-f A-F ]
$identnondigit = [ $digit $nondigit ]
$hchar = [^\n]
$qchar = [^\n\"]

@schar = $printable* 

@hexquad = $hexdigit{4}
@universalCharacterNameInner = "u" @hexquad | "U" @hexquad @hexquad
@universalCharacterName = "\\" @universalCharacterNameInner

@escapesequence = '\\' ([\'\"\?\\abfnrtv] | $octaldigit{1,3} | 'x' $hexdigit+ | @universalCharacterNameInner)
@cchar = [^\'\n\\] | @escapesequence
@schar = [^\"\n\\] | @escapesequence

@sign = "+" | "-"

tokens :-
<0> "{"                         { punctuator LBrace }
<0> "}"                         { punctuator RBrace }
-- 
<0> [$white]^"("                { punctuator LParen }
<0> [^$white]^"("               { mkLocated $ \_ _ -> pure $ Just $ PPSpecial PPSLParen }
<0> ")"                         { punctuator RParen }
<0> "["                         { punctuator LBrack }
<0> "]"                         { punctuator RBrack }
<0> "->"                        { punctuator Arrow  }
<0> "&"                         { punctuator BitAnd }
<0> "|"                         { punctuator BitOr  }
<0> "*"                         { punctuator Times  }
<0> "+"                         { punctuator Plus   }
<0> "-"                         { punctuator Minus  }
<0> "~"                         { punctuator Compliment }
<0> "!"                         { punctuator Not    }
<0> "/"                         { punctuator Divide }
<0> "%"                         { punctuator Modulo }
<0> "<<"                        { punctuator LShift } 
<0> ">>"                        { punctuator RShift } 
<0> "<"                         { punctuator Lt     }
<0> "<="                        { punctuator Le     }
<0> ">"                         { punctuator Gt     }
<0> ">="                        { punctuator Ge     }
<0> "=="                        { punctuator Eq     }
<0> "!="                        { punctuator Neq    }
<0> "^"                         { punctuator BitXor }
<0> "&&"                        { punctuator LAnd   }
<0> "||"                        { punctuator LOr    }
<0> ";"                         { punctuator Semi   }
<0> "="                         { punctuator Assign }
<0> ","                         { punctuator Comma  }
<0> "."                         { punctuator Dot    }
<0> ":"                         { punctuator Colon  }
<0> "++"                        { punctuator PlusPlus    }
<0> "--"                        { punctuator MinusMinus  }
<0> "?"                         { punctuator Question    }
<0> "..."                       { punctuator Variadic    }
<0> "*="                        { punctuator TimesAssign }
<0> "/="                        { punctuator DivAssign   }
<0> "%="                        { punctuator ModAssign   }
<0> "+="                        { punctuator PlusAssign  }
<0> "-="                        { punctuator MinusAssign }
<0> "<<="                       { punctuator LShiftAssign}
<0> ">>="                       { punctuator RShiftAssign}
<0> "&="                        { punctuator AndAssign   }
<0> "^="                        { punctuator XorAssign   }
<0> "|="                        { punctuator OrAssign    }
<0> "#"                         { punctuator Stringize   }
<0> "##"                        { punctuator TokenPaste  }
-- string literals
<0>  \" @schar+ \"              { mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPStringLiteral (T.take (i - 2) (T.tail t)) }
-- wide string literals
<0>  "L\"" @schar+ \"           { mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPStringLiteral (T.take (i-3) (T.tail . T.tail $ t)) } 
<0>  \' @cchar+ \'              { mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPCharConst (T.take (i-2) (T.tail t))}
<0>  $nondigit $identnondigit*  { mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPIdent (T.take i t) }
<0>  ([^$white] # [$nondigit $digit \'\" \( \) \{ \} \[ \] \| \* \+ \~ \- \; \, \? \. \^ \/ \# \> \< & \% ! = : ]) [^$white]*  { mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPOther (T.take i t)}

-- this is a hack and I dont like it but it works :/
-- headernames
<0>  "<" [^\n>]+ ">"            { mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPHeaderName (T.take (i-2) (T.tail t)) }
-- <0>  \" [^\n\"] \"           { mkLocated $ \(_,_,_,t) i -> pure $ Just $ error "e" }

        -- preprocessing numbers
<0>  "."? $digit ($digit | $identnondigit | [eEpP] @sign | ".")* {mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPNumber (T.take i t)}
<0>  ($white # [\n])+;

-- dont emit a newline if (physical line concatenation)
<0>  \\\n ; 
<0>  \n                         {mkLocated $ \_ _ -> pure $ Just $ PPSpecial PPNewline}
-- <0>  [\n]                        {mkLocated $ \(_,_,_,t) i -> pure $ Just $ PPNewline}



        -- matche everything other than 
<0>  "//"                       { begin linecomment }
<0>  "/*"                       { begin blockcomment }

<linecomment> [^\n]* { begin 0 }  -- match non linebreaks

<blockcomment> \n; -- match newlines in block comments
<blockcomment> [^\*]+; -- match everything other than *
<blockcomment> "*" \n; -- match * not followed by /
<blockcomment> "*" [^\/]; -- match * not followed by /
<blockcomment> "*/"             { begin 0 } -- match */

{

mkLocated :: (State AlexState :> es) => (AlexInput -> Int -> Eff es (Maybe PPToken)) -> (AlexInput -> Int -> AlexPosn -> Eff es (Maybe (Located PPToken)))
mkLocated f inp@((AlexPn sAbs sLine sCol ),_,_,_) len (AlexPn eAbs eLine eCol) = do
        srcPath <- getCurrentFilePath <$> get
        let span = SrcSpan srcPath sLine sCol eLine eCol
        fmap (L span) <$> (f inp len)

--basicAction :: (State AlexState :> es) => Token -> (AlexInput -> Int -> Eff es Token)
--basicAction token _ _ = pure token

punctuator :: (State AlexState :> es) => Punctuator -> (AlexInput -> Int -> AlexPosn -> Eff es (Maybe (Located PPToken)))
punctuator token = mkLocated $ \_ _->  pure $ Just $ PPPunctuator token 

alexEOF :: Eff es (Maybe PPToken)
alexEOF = pure Nothing


-- -----------------------------------------------------------------------------
-- Default monad

data AlexState = AlexState {
    alex_pos :: !AlexPosn,  -- position at current input location
    alex_inp :: T.Text, -- the current input
    alex_chr :: !Char,  -- the character before the input
    alex_bytes :: [Byte],        -- rest of the bytes for the current char
    alex_scd :: !Int,    -- the current startcode
    alex_filename :: !FilePath
} deriving stock (Eq, Show)

newAlexState input fileName = AlexState alexStartPos input '\n' [] 0 fileName

getCurrentFilePath :: AlexState -> FilePath
getCurrentFilePath = alex_filename

-- Compile with -funbox-strict-fields for best results!
-- handle the effects here
runAlex :: T.Text -> FilePath -> Eff (State AlexState ': Error String ':  es) a -> Eff es (Either (CallStack, String) a)
runAlex input__ path = runError . evalState (newAlexState input__ path)



printTokens :: (IOE :> es, State AlexState :> es, Error String :> es) => Eff es ()
printTokens = do
    token <- alexMonadScan
    liftIO $ print token
    case token of
        Nothing -> pure ()
        Just _ -> printTokens 



alexGetInput :: State AlexState :> es => Eff es AlexInput
alexGetInput = do
  AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} <- get
  pure (pos,c,bs,inp__)


-- I think this should be right
alexSetInput :: State AlexState :> es => AlexInput -> Eff es ()
alexSetInput (pos, c, bs,inp__) = modify @AlexState (\s -> s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__})

alexGetStartCode :: State AlexState :> es => Eff es Int
alexGetStartCode = get <&> \AlexState{alex_scd=sc} -> sc

alexSetStartCode :: State AlexState :> es => Int -> Eff es ()
alexSetStartCode sc = modify (\s -> s{alex_scd=sc}) 


alexConduitSource :: (Error String :> es) => T.Text -> FilePath -> ConduitT i (Located PPToken) (Eff es) ()
alexConduitSource contents filePath = loop (newAlexState contents filePath)
    where
        loop s = do
            (token,s') <- lift $ runState s alexMonadScan
            case token of
                Nothing -> pure ()
                Just n -> do
                    yield n
                    loop s'

alexConduit :: (Error String :> es, State AlexState :> es) => ConduitT i (Located PPToken) (Eff es) ()
alexConduit = do
    token <- lift alexMonadScan
    case token of
        Nothing -> pure ()
        Just n -> do
            yield n
            alexConduit


print' :: (IOE :> es, Show a) => a -> Eff es ()
print' x = liftIO $ print x

alexPrintCondTokens :: (IOE :> es) => T.Text -> FilePath -> Eff es ()
alexPrintCondTokens inp p = runAlex inp p (runConduit $ (alexConduit .| mapM_C (print' ) .| sinkNull)) $> ()



alexMonadScan :: (Error String :> es, State AlexState :> es) => Eff es (Maybe (Located PPToken))
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    -- TODO: I should probably change this to include actual span info :/
    --AlexEOF -> fmap (L (Unhelpful )) <$> alexEOF
    AlexEOF -> pure Nothing --fmap (L (Unhelpful )) <$> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
      alexSetInput inp__'
      alexMonadScan
    --      AlexToken inp__' len (action :: State AlexState :> es => AlexInput -> Int -> AlexPosn -> Eff es Token) -> do
    AlexToken inp__'@(pos',_,_,_) len action -> do
      alexSetInput inp__'
        --(L (Span "" pos pos' []) ) <$> 
      action (ignorePendingBytes inp__) len pos'



-- -----------------------------------------------------------------------------
-- Useful token actions

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _ _ _ = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _ _ _ = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

type AlexInput = (AlexPosn,       -- current position,
                  Char,           -- previous char
                  [Byte],         -- pending bytes on current char
                  T.Text) -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,_,[],s) = case T.uncons s of
                            Just (c, cs) ->
                              let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, cs))
                            Nothing ->
                              Nothing

-- abs, line, col
data AlexPosn = AlexPn !Int !Int !Int
    deriving stock (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

type Byte = Word8

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

}


