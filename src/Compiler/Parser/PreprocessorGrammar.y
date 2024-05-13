{
-- The dreaded monomorphism restriction
{-# LANGUAGE NoMonomorphismRestriction #-}   
module Compiler.Parser.PreprocessorGrammar(parseLine, PPLine (..), PPIfLine (..), PPControlLine (..)) where

--import Control.Monad
--import Data.Functor
--import Data.Maybe

--import Compiler.Parser.Preprocessor (PreprocessorState)

import Data.Text qualified as T

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.Tokens

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

}


-- %monad {(Error String :> es, State AlexState :> es, State SymbolTable :> es) }  {Eff es} {>>=} {return}
%monad {(IOE :> es, Error String :> es, State AlexState :> es) }  {Eff es} {>>=} {return}

%lexer {lexer} {PPSpecial PPNewline}

%errorhandlertype explist
%error {parseError}
%tokentype { PPToken }

%token
    include     { PPIdent "include"     }
    define      { PPIdent "define"      }
    undef       { PPIdent "undef"       }
    line        { PPIdent "line"        }
    err         { PPIdent "error"       }
    pragma      { PPIdent "pragma"      }
    if          { PPIdent "if"          }
    ifdef       { PPIdent "ifdef"       }
    ifndef      { PPIdent "ifndef"      }
    elif        { PPIdent "elif"        }
    else        { PPIdent "else"        }
    endif       { PPIdent "endif"       }


    '#'         { PPPunctuator Stringize }
    '##'        { PPPunctuator TokenPaste }
    '...'       { PPPunctuator Variadic }
    ','         { PPPunctuator Comma }
    -- The PPSLParen is a '(' without preceding whitespace
    '('         { PPSpecial PPSLParen }
    ')'         { PPPunctuator RParen }


    headername  { PPHeaderName $$ }
    ident       { PPIdent $$ }
    number      { PPNumber $$ }
    char        { PPCharConst $$ }
    stringlit   { PPStringLiteral $$ }
    punct       { PPPunctuator $$ }
    other       { PPOther $$   }
    ppeof       { PPEOF }




{-
How to write the preprocessor:
    The lexer needs handle outputting lines
-}

%partial parseLine Line
%%

-- match anything but newlines
PPToken :: { PPToken }
    : headername    { PPHeaderName $1         }
    | ident         { PPIdent $1              }
    | number        { PPNumber $1             }
    | char          { PPCharConst $1          }
    | stringlit     { PPStringLiteral $1      }
    | punct         { PPPunctuator $1         }
    | '('           { PPPunctuator LParen     }
    --| '('           { PPSpecial PPSLLParen    }
    | '#'           { PPPunctuator Stringize  }
    | '##'          { PPPunctuator TokenPaste }
    | '...'         { PPPunctuator Variadic   }
    | ')'           { PPPunctuator RParen     }
    | ','           { PPPunctuator Comma      }
    | include       { PPIdent "include"       }
    | define        { PPIdent "define"        }
    | undef         { PPIdent "undef"         }
    | line          { PPIdent "line"          }
    | err           { PPIdent "error"         }
    | pragma        { PPIdent "pragma"        }
    | if            { PPIdent "if"            }
    | ifdef         { PPIdent "ifdef"         }
    | ifndef        { PPIdent "ifndef"        }
    | elif          { PPIdent "elif"          }
    | else          { PPIdent "else"          }
    | endif         { PPIdent "endif"         }
    
    | other         { PPOther $1 }

-- list will be reversed
PPTokens :: { [PPToken] }
    : PPToken               { [$1] }
    | PPTokens PPToken      { $2 : $1 }

Line :: { PPLine }
    : IfLine            { IfLine $1 }
    | ControlLine       { ControlLine $1 }
    | TextLine          { TextLine $1  }
    | '#' ident         { NonDirective $2 }
    | ppeof             { PPSEnd }


IfLine :: { PPIfLine }
    : '#' if  PPTokens      { ILIf $3 }
    | '#' ifdef ident       { ILIfDef $3 }
    | '#' ifndef ident      { ILIfNDef $3 }
    | '#' elif PPTokens     { ILElIf  $3 }
    | '#' else              { ILElse     }
    | '#' endif             { ILEndIf    }

TextLine :: { [ PPToken ]}
    : PPTokens  { reverse $1 }
    |           { [] }

ControlLine :: { PPControlLine }
    : '#' include PPTokens          { CLInclude (reverse $3)    }
    
    | '#' define ident PPTokens     { CLDefineObj $3 (reverse $4)  }

    | '#' define ident '(' ')' PPTokens                         { CLDefineFunc $3 [] False (reverse $6)  }
    | '#' define ident '(' '...' ')' PPTokens                   { CLDefineFunc $3 [] True (reverse $7)  }
    | '#' define ident '(' IdentList ')' PPTokens               { CLDefineFunc $3 (reverse $5) False (reverse $7)  }
    | '#' define ident '(' IdentList ',' '...' ')' PPTokens     { CLDefineFunc $3 (reverse $5) True (reverse $9)  }

    | '#' undef ident               { CLUndef $3                }
    | '#' line PPTokens             { CLLine (reverse $3)       }
    | '#' error PPTokens            { CLError (reverse $3)      }
    | '#' pragma PPTokens           { CLPragma (reverse $3)     }
    | '#'                           { CLEmpty                   }


IdentList :: { [Identifier] }
    : ident                 { [$1] }
    | IdentList ',' ident   { $3 : $1 }

{


parseError :: (Error String :> es, State AlexState :> es) => (PPToken, [String]) -> Eff es a
parseError (t, tokens) = throwError $ "something failed :(, failed on token: \"" ++  show t ++ "\"possible tokens: " ++ show tokens  -- throwError "failure :("

lexer :: (IOE :> es, Error String :> es, State AlexState :> es) =>  (PPToken -> Eff es a) -> Eff es a
lexer = ((alexMonadScan) >>=) 
--lexer = ((alexMonadScan >>= (\t -> const t <$> (liftIO $ print ( "before: " ++ show t))) ) >>=)
{-
lexer f = do
    s@PreprocessorState{buf=oq} <- get @PreprocessorState
    case oq of
        -- pull a token from the outQueue
        h S.:<| t -> put (s{buf=t}) >> f h
        S.Empty -> alexMonadScan >>= f
-}


data PPLine
    = IfLine PPIfLine
    | ControlLine PPControlLine
    | TextLine [PPToken]
    | NonDirective Identifier
    | PPSEnd
    deriving stock (Eq, Show)

data PPIfLine
    = ILIf [PPToken]
    | ILIfDef Identifier
    | ILIfNDef Identifier
    | ILElIf [PPToken]
    | ILElse
    | ILEndIf
    deriving stock (Eq, Show)


data PPControlLine
    = CLInclude [PPToken]
    | CLDefineObj Identifier [PPToken] 
    | CLDefineFunc Identifier [Identifier] Bool [PPToken]
    | CLUndef Identifier
    | CLLine [PPToken]
    | CLError [PPToken]
    | CLPragma [PPToken]
    | CLEmpty
    | CLParseError
    deriving stock (Eq, Show)


}
