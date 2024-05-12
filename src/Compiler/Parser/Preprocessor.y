{
    
{-
    This module implements the c preprocessor.
    


-}
-- The dreaded monomorphism restriction
{-# LANGUAGE NoMonomorphismRestriction #-}   

module Compiler.Parser.Preprocessor(preprocess, PreprocessorState, printPPTokens, runPreprocessor) where

import Data.Functor
import Control.Monad

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.Tokens

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import qualified Data.Sequence as S

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO qualified as TIO

--import Text.ParserCombinators.ReadP
}


-- %monad {(Error String :> es, State AlexState :> es, State SymbolTable :> es) }  {Eff es} {>>=} {return}
%monad {(IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) }  {Eff es} {>>=} {return}

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

%partial line Line
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
    : -- IfSection
      ControlLine       { ControlLine $1 }
    | TextLine          { TextLine $1  }
    | '#' ident         { NonDirective $2 }
    | ppeof             { PPSEnd }

{-
IfLine :: { IfLine }
    : '#' if ConstExpr      {}
    | '#' ifdef ident       {}
    | '#' ifndef ident      {}

ElifLine :: { }
    : '#' elif              {  }

ElseLine :: { }
    : '#' else              {  }

EndIfLine :: {}
    : '#' endif             {  }


ConstExpr :: { T.Text }
    | number            {$1}

-}



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

lexer :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) =>  (PPToken -> Eff es a) -> Eff es a
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
    = ILIf 
    | ILIfDef Identifier
    | ILIfNDef Identifier
    | ILElIf 
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


data MacroDef = ObjectMacro [PPToken] | FuncMacro [Identifier] Bool [PPToken]

data PreprocessorState = PreprocessorState
    -- when an include is encountered the current alex state is pushed onto this stack
    -- and a new one for the new file replaces the current AlexState.
    -- When the current file is fully tokenized and processed we return to the last file
    { lexStack :: [AlexState]
    -- The outstack is a buffer of tokens that is popped from 
    , outQueue :: [PPToken]

    -- This queue holds the tokens before they go into the parser
    , buf :: S.Seq PPToken

    , macroSymTbl :: M.Map T.Text MacroDef
    , concatLookahead :: PPToken
    }

newPreprocessorState :: PreprocessorState
newPreprocessorState = PreprocessorState{lexStack = [], macroSymTbl = M.empty, outQueue = [], buf = S.empty, concatLookahead = PPSpecial PPNewline}

handleInclude :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => [PPToken] -> Eff es ()
handleInclude toks = do
    -- TODO: properly concatenate tokens here
    -- TODO: properly resolve headers
    let file = case (head toks) of
            PPStringLiteral s -> s
            _ -> error "partially unimplemented: cannot resolve headers other than string literals"

    inp <- liftIO $ TIO.readFile (T.unpack file)
    old_state <- get @AlexState 
    modify (\s@PreprocessorState{lexStack=ls} -> s{lexStack=(old_state:ls)})
    put @AlexState $ newAlexState inp
    
handleLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPLine -> Eff es [PPToken]
handleLine line = case line of 
        TextLine line -> pure line
        ControlLine (CLInclude toks) -> handleInclude toks >> pure []
        ControlLine (CLDefineObj name val) -> do
            s@PreprocessorState{macroSymTbl = macroSymTbl} <- get
            case M.lookup name macroSymTbl of 
                Nothing -> put (s{macroSymTbl=(M.insert name (ObjectMacro val) macroSymTbl)})
                -- TODO: This should not throw an error if the replacement lists are identical
                Just tbl -> throwError ("macro \"" ++ (T.unpack name) ++ "\" is already defined")
            pure []
        ControlLine (CLDefineFunc name args variadic val) -> do
            s@PreprocessorState{macroSymTbl = macroSymTbl} <- get
            case M.lookup name macroSymTbl of 
                Nothing -> put (s{macroSymTbl=(M.insert name (FuncMacro args variadic val) macroSymTbl)})
                Just tbl -> throwError ("macro \"" ++ (T.unpack name) ++ "\" is already defined")
            pure []
        ControlLine (CLUndef name) -> do
            modify (\s@PreprocessorState{macroSymTbl = macroSymTbl} -> s{macroSymTbl=M.delete name macroSymTbl})
            pure []
        ControlLine (CLLine _) -> do
            pure []
        ControlLine (CLError _) -> do
            pure []
        ControlLine (CLPragma _) -> do
            pure []
        ControlLine CLEmpty -> do
            pure []
        ControlLine CLParseError -> do
            liftIO $ print "Encountered a parse error while parsing a control line"
            pure []
        NonDirective name -> do
            liftIO . print $ "Encountered invalid directive \"" ++ (T.unpack name) ++ "\", ignoring"
            pure []
        PPSEnd -> do
            s@PreprocessorState{lexStack = ls} <- get
            case ls of
                [] -> pure [PPEOF]
                h : t -> do
                    put h
                    put (s{lexStack = t})
                    pure []

expandTokenLine :: M.Map T.Text MacroDef -> [PPToken] -> [PPToken] {- :: (State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es () -}
expandTokenLine macros = go 
    where
        go [] = []
        go ((PPStringLiteral s1):(PPStringLiteral s2):t) = go ((PPStringLiteral (T.append s1 s2)):t)
        go ((PPIdent ident):t) = case M.lookup ident macros of
            Nothing -> (PPIdent ident):(go t)
            Just (ObjectMacro replacementList) -> replacementList ++ go t
            Just (FuncMacro args variadic replacementList) -> [PPIdent ident] ++ go t
        go (h:t) = h:(go t)


-- gets a token from the token queue and refills the queue if it is empty
ppNextToken :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => Eff es PPToken
ppNextToken = do
    s@PreprocessorState{outQueue=oq,macroSymTbl=mst,concatLookahead=lk} <- get @PreprocessorState
    case oq of
        -- pull a token from the outQueue
        h:t -> do
            put (s{outQueue=t,concatLookahead=h})
            pure lk
        [] -> case lk of
            PPEOF -> pure PPEOF
            _ -> do
                tokenLine <- line >>= handleLine 
                modify (\s -> s{outQueue=expandTokenLine mst tokenLine})
                ppNextToken






runPreprocessor :: (State AlexState :> es, Error String :> es) => Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
runPreprocessor = evalState newPreprocessorState . evalState [M.empty]

--runPreprocessor' :: T.Text -> Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
--runPreprocessor' inp = runAlex inp . evalState newPreprocessorState . evalState [M.empty]



preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => Eff es Token
preprocess = do
    nextToken <- ppNextToken
    case nextToken of
        PPHeaderName x -> error ""
        PPOther o -> error ""
        PPNumber n -> error "implement coversion from PP number to num constants"
        PPCharConst c -> pure $ Constant $ CharConst c
        PPStringLiteral s -> pure $ StringLiteral s
        PPPunctuator punct -> pure $ Punctuator punct
        PPIdent id -> pure $ case id of
            "auto" -> Keyword TypeDef
            "break" -> Keyword Break
            "case" -> Keyword Case
            "const" -> Keyword Const
            "continue" -> Keyword Continue
            "default" -> Keyword Default 
            "do" -> Keyword Do 
            "else" -> Keyword Else 
            "enum" -> Keyword Enum 
            "extern" -> Keyword Extern 
            "for" -> Keyword For 
            "goto" -> Keyword Goto
            "if" -> Keyword If 
            "inline" -> Keyword Inline 
            "register" -> Keyword Register 
            "restrict" -> Keyword Restrict 
            "return" -> Keyword Return 
            "static" -> Keyword TStatic 
            "sizeof" -> Keyword Sizeof
            "struct" -> Keyword Struct
            "switch" -> Keyword Switch 
            "typedef" -> Keyword TypeDef 
            "union" -> Keyword Union 
            "volatile" -> Keyword Volatile 
            "while" -> Keyword While
            "void" -> Keyword Void 
            "char" -> Keyword TChar
            "short" -> Keyword TShort
            "int" -> Keyword TInt
            "long" -> Keyword TLong
            "float" -> Keyword TFloat
            "double" -> Keyword TDouble
            "signed" -> Keyword TSigned
            "unsigned" -> Keyword TUnsigned
            "_Bool" -> Keyword TuBool
            "_Complex" -> Keyword TuComplex
            i -> Ident i
        --PPNewline -> preprocess
        PPSpecial PPNewline -> preprocess
        PPSpecial PPSLParen -> pure $ Punctuator LParen
        PPEOF -> pure EOF


printPPTokens :: (IOE :> es, State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es ()
printPPTokens = do
    --token <- preprocess
    token <- ppNextToken
    liftIO $ print token
    case token of
        --EOF -> pure ()
        PPEOF -> pure ()
        _ -> printPPTokens


}

{-
runPreprocessor:: T.Text -> Eff (State AlexState ': Error String ': State SymbolTable ': es) a -> Eff es (Either (CallStack, String) a)
runPreprocessor input__ = evalState [M.empty] . runError . evalState (AlexState alexStartPos input__ '\n' [] 0 Nothing)
-}