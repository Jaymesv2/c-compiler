{
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module Compiler.Parser.Lexer (alexMonadScan, runAlex, AlexState, newAlexState, printTokens) where

import Compiler.Parser
import Compiler.SymbolTable
--import Control.Applicative as App (Applicative (..))
--import Data.Maybe
import Data.Functor
--import Data.Text qualified
import Data.Word (Word8)

import Data.Bits qualified
import Data.Char (ord)


import qualified Data.Text as T
--import qualified Data.List as L
import qualified Data.Map as M


import Compiler.Parser.Tokens
import Compiler.Parser

--import Compiler.Parser.Monad

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
}

%action "AlexInput -> Int -> Eff es PPToken"
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
<0> "{"                       { punctuator LBrace }
<0> "}"                       { punctuator RBrace }
-- 
<0> [$white]^"("              { punctuator LParen }
<0> [^$white]^"("             { \_ _ -> pure $ PPSpecial PPSLParen }
<0> ")"                       { punctuator RParen }
<0> "["                       { punctuator LBrack }
<0> "]"                       { punctuator RBrack }
<0> "->"                      { punctuator Arrow  }
<0> "&"                       { punctuator BitAnd }
<0> "|"                       { punctuator BitOr  }
<0> "*"                       { punctuator Times  }
<0> "+"                       { punctuator Plus   }
<0> "-"                       { punctuator Minus  }
<0> "~"                       { punctuator Compliment }
<0> "!"                       { punctuator Not    }
<0> "/"                       { punctuator Divide }
<0> "%"                       { punctuator Modulo }
<0> "<<"                      { punctuator LShift } 
<0> ">>"                      { punctuator RShift } 
<0> "<"                       { punctuator Lt     }
<0> "<="                      { punctuator Le     }
<0> ">"                       { punctuator Gt     }
<0> ">="                      { punctuator Ge     }
<0> "=="                      { punctuator Eq     }
<0> "!="                      { punctuator Neq    }
<0> "^"                       { punctuator BitXor }
<0> "&&"                      { punctuator LAnd   }
<0> "||"                      { punctuator LOr    }
<0> ";"                       { punctuator Semi   }
<0> "="                       { punctuator Assign }
<0> ","                       { punctuator Comma  }
<0> "."                       { punctuator Dot    }
<0> ":"                       { punctuator Colon  }
<0> "++"                      { punctuator PlusPlus    }
<0> "--"                      { punctuator MinusMinus  }
<0> "?"                       { punctuator Question    }
<0> "..."                     { punctuator Variadic    }
<0> "*="                      { punctuator TimesAssign }
<0> "/="                      { punctuator DivAssign   }
<0> "%="                      { punctuator ModAssign   }
<0> "+="                      { punctuator PlusAssign  }
<0> "-="                      { punctuator MinusAssign }
<0> "<<="                     { punctuator LShiftAssign}
<0> ">>="                     { punctuator RShiftAssign}
<0> "&="                      { punctuator AndAssign   }
<0> "^="                      { punctuator XorAssign   }
<0> "|="                      { punctuator OrAssign    }
<0> "#"                       { punctuator Stringize   }
<0> "##"                      { punctuator TokenPaste  }
-- string literals
<0>  \" @schar+ \"              { \(_,_,_,t) i -> pure $ PPStringLiteral (T.take (i - 2) (T.tail t)) }
-- wide string literals
<0>  "L\"" @schar+ \"              { \(_,_,_,t) i -> pure $ PPStringLiteral (T.take (i-3) (T.tail . T.tail $ t)) } 

<0>  \' @cchar+ \'          { \(_,_,_,t) i -> pure $ PPCharConst (T.take (i-2) (T.tail t))}


<0>  $nondigit $identnondigit* { \(_,_,_,t) i -> pure $ PPIdent (T.take i t) }

<0>  ([^$white] # [$nondigit $digit \'\" \( \) \{ \} \[ \] \| \* \+ \~ \- \; \, \? \. \^ \/ \# \> \< & \% ! = : ]) [^$white]*  { \(_,_,_,t) i -> pure $ PPOther (T.take i t)}

-- this is a hack and I dont like it but it works :/
-- headernames
<0>  "<" [^\n>]+ ">"              { \(_,_,_,t) i -> pure $ PPHeaderName (T.take (i-2) (T.tail t)) }
-- <0>  \" [^\n\"] \"           { \(_,_,_,t) i -> pure $ error "e" }

        -- preprocessing numbers
<0>  "."? $digit ($digit | $identnondigit | [eEpP] @sign | ".")* {\(_,_,_,t) i -> pure $ PPNumber (T.take i t)}
<0>  ($white # [\n])+;



<0>  \\\n ; -- dont emit a newline if (physical line concatenation)
<0>  \n {\_ _ -> pure $ PPSpecial PPNewline}
-- <0>  [\n] {\(_,_,_,t) i -> pure $ PPNewline}



        -- matche everything other than 
<0>  "//" { begin linecomment }
<0>  "/*" { begin blockcomment }

-- <linecomment> [^\n]+ ;  -- match non linebreaks
-- <linecomment> \n { begin 0 } -- switch back to code
<linecomment> [^\n]+ { begin 0 }  -- match non linebreaks

<blockcomment> [^\*]+; -- match everything other than *
<blockcomment> "*" [^\/]; -- match * not followed by /
<blockcomment> "*/" { begin 0 } -- match */

{

basicAction :: (State AlexState :> es) => Token -> (AlexInput -> Int -> Eff es Token)
basicAction token _ _ = pure token

punctuator :: (State AlexState :> es) => Punctuator -> (AlexInput -> Int -> Eff es PPToken)
punctuator token _ _ = pure $ PPPunctuator token

alexEOF :: Eff es PPToken
alexEOF = pure PPEOF


-- -----------------------------------------------------------------------------
-- Default monad

data AlexState = AlexState {
    alex_pos :: !AlexPosn,  -- position at current input location
    alex_inp :: T.Text, -- the current input
    alex_chr :: !Char,  -- the character before the input
    alex_bytes :: [Byte],        -- rest of the bytes for the current char
    alex_scd :: !Int    -- the current startcode
} deriving stock (Eq, Show)

newAlexState input = AlexState alexStartPos input '\n' [] 0

-- Compile with -funbox-strict-fields for best results!
-- handle the effects here
runAlex :: T.Text -> Eff (State AlexState ': Error String ':  es) a -> Eff es (Either (CallStack, String) a)
runAlex input__ = runError . evalState (newAlexState input__)


printTokens :: (IOE :> es, State AlexState :> es, Error String :> es) => Eff es ()
printTokens = do
    token <- alexMonadScan
    liftIO $ print token
    case token of
        PPEOF -> pure ()
        _ -> printTokens


alexGetInput :: State AlexState :> es => Eff es AlexInput
alexGetInput = do
  AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} <- get
  pure (pos,c,bs,inp__)


-- I think this should be right
-- prob replace this with modify
alexSetInput :: State AlexState :> es => AlexInput -> Eff es ()
alexSetInput (pos, c, bs,inp__) = modify @AlexState (\s -> s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__})
{-alexSetInput (pos, c, bs,inp__) = 
    get >>= \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of state__@(AlexState{}) -> put state__-}

alexGetStartCode :: State AlexState :> es => Eff es Int
alexGetStartCode = get <&> \AlexState{alex_scd=sc} -> sc

alexSetStartCode :: State AlexState :> es => Int -> Eff es ()
alexSetStartCode sc = modify (\s -> s{alex_scd=sc}) 

alexMonadScan :: (Error String :> es, State AlexState :> es) => Eff es PPToken
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
      alexSetInput inp__'
      alexMonadScan
    --AlexToken inp__' len (action :: State AlexState :> es => AlexInput -> Int -> Eff es Token) -> do
    AlexToken inp__' len action -> do
      alexSetInput inp__'
      action (ignorePendingBytes inp__) len


-- -----------------------------------------------------------------------------
-- Useful token actions

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _ _ = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _ _ = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

-- token :: (String -> Int -> token) -> AlexAction token
-- token t input len = return (t input len)

{-
-}
--type AlexAction result = AlexInput -> Int -> result

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


data AlexPosn = AlexPn !Int !Int !Int
    deriving stock (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)




}


