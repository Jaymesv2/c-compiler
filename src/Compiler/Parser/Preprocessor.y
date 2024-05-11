{
-- The dreaded monomorphism restriction
{-# LANGUAGE NoMonomorphismRestriction #-}   

module Compiler.Parser.Preprocessor where

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

}


-- %monad {(Error String :> es, State AlexState :> es, State SymbolTable :> es) }  {Eff es} {>>=} {return}
%monad {(IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) }  {Eff es} {>>=} {return}

%lexer {lexer} {PPNewline}
--%lexer {(alexMonadScan >>=)} {EOF}

%errorhandlertype explist
%error {parseError}
%tokentype { PPToken }

%token
    include     { PPIdent "include"      }
    define      { PPIdent "define"       }
    undef       { PPIdent "undef"        }
    line        { PPIdent "line"         }
    err         { PPIdent "error"        }
    pragma      { PPIdent "pragma"       }


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

%partial groupPart GroupPart
%%

-- match anything but newlines
PPToken :: { PPToken }
    : headername    { PPHeaderName $1 }
    | ident         { PPIdent $1 }
    | number        { PPNumber $1 }
    | char          { PPCharConst $1 }
    | stringlit     { PPStringLiteral $1 }
    | punct         { PPPunctuator $1 }
    -- | lparen        { PPSpecial PPSLParen }
    | '('           { PPPunctuator LParen }
    | '#'           { PPPunctuator Stringize }
    | '##'          { PPPunctuator TokenPaste }
    | '...'         { PPPunctuator Variadic }
    | ')'           { PPPunctuator RParen }
    | ','           { PPPunctuator Comma }
    
    | other         { PPOther $1 }

-- list will be reversed
PPTokens :: { [PPToken] }
    : PPToken               { [$1] }
    | PPTokens PPToken      { $2 : $1 }

GroupPart :: { PPSection }
    : -- IfSection
      ControlLine       { ControlLine $1 }
    | TextLine          { TextLine $1  }
    | ppeof             { PPSEnd }

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

data PPSection
    = IfSection 
    | ControlLine PPControlLine
    | TextLine [PPToken]
    | NonDirective
    | PPSEnd
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
    deriving stock (Eq, Show)


data MacroDef = ObjectMacro {} | FuncMacro {}

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
    }

newPreprocessorState :: PreprocessorState
newPreprocessorState = PreprocessorState{lexStack = [], macroSymTbl = M.empty, outQueue = [], buf = S.empty}

{-
-- gets a line of tokens
getTokenLine :: (Error String :> es, State AlexState :> es) => Eff es [PPToken]
getTokenLine = go []
    where 
        go :: (Error String :> es, State AlexState :> es) => [PPToken] -> Eff es [PPToken]
        go toks = do
            tok <- alexMonadScan
            case tok of 
                PPEOF -> pure (reverse $ PPEOF:toks)
                PPNewline -> pure (reverse $ PPNewline:toks)
                other -> go (tok:toks)
-}

printPPTokens :: (IOE :> es, State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es ()
printPPTokens = do
    token <- ppNextToken
    liftIO $ print token
    case token of
        PPEOF -> pure ()
        _ -> printPPTokens

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
    
handleLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPSection -> Eff es [PPToken]
handleLine section = do
    liftIO $ print $ "group part: " ++ show section
    case section of 
        TextLine line -> do
            pure line
        ControlLine (CLInclude toks) -> handleInclude toks >> pure []
        ControlLine (CLDefineObj _ _) -> do
            pure []
        ControlLine (CLDefineFunc _ _ _ _) -> do
            pure []
        ControlLine (CLUndef _) -> do
            pure []
        ControlLine (CLLine _) -> do
            pure []
        ControlLine (CLError _) -> do
            pure []
        ControlLine (CLPragma _) -> do
            pure []
        ControlLine (CLEmpty) -> do
            pure []
        PPSEnd -> do
            liftIO $ print "got an end"
            s@PreprocessorState{lexStack = ls} <- get
            liftIO $ print ls
            case ls of
                [] -> liftIO (print "ending") >> pure [PPEOF]
                h : t -> do
                    liftIO (print "stack nonempty")
                    put h
                    put (s{lexStack = t})
                    pure []

ppNextToken :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => Eff es PPToken
ppNextToken = do
    s@PreprocessorState{outQueue=oq} <- get @PreprocessorState
    case oq of
        -- pull a token from the outQueue
        h:t -> do
            put (s{outQueue=t}) 
            pure h
        [] -> do
            gp <- groupPart
            line <- handleLine gp
            modify (\s -> s{outQueue=line})
            ppNextToken


            -- need to pull more input
    --state @PreprocessorState (\s@PreprocessorState{lookahead = lk} -> (lk, s{lookahead = nextToken}))

--runAlex :: T.Text -> Eff (State AlexState ': Error String ':  es) a -> Eff es (Either (CallStack, String) a)
--runAlex input__ = runError . evalState (AlexState alexStartPos input__ '\n' [] 0)

runPreprocessor :: (State AlexState :> es, Error String :> es) => Eff (State PreprocessorState ': es) a -> Eff es a
runPreprocessor = evalState newPreprocessorState

--getLookahead :: (State PreprocessorState :> es) => Eff es PPToken
--getLookahead = get <&> \s@PreprocessorState{lookahead = lk} -> lk


--handle :: (Error String :> es, State AlexState :> es, State SymbolTable :> es, State PreprocessorState :> es) => Eff es Token
--handle = error ""

preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => Eff es Token
preprocess = do
    nextToken <- ppNextToken
    case nextToken of
        PPHeaderName x -> error ""
        PPIdent id -> pure $ Ident id
        PPCharConst c -> pure $ Constant $ CharConst c
        PPNumber n -> error "implement coversion from PP number to num constants"
        PPStringLiteral s -> pure $ StringLiteral s
        PPPunctuator punct -> pure $ Punctuator punct
        PPOther o -> error "other"
        PPNewline -> preprocess
        PPEOF -> do
            s@PreprocessorState{lexStack = ls} <- get
            case ls of
                [] -> pure EOF
                h : t -> do
                    put (s{lexStack = t})
                    put h
                    preprocess

-- combines PPNumbers and 
combinePPTokens :: Eff es PPToken
combinePPTokens = error "unimplemented"

parseError :: (Error String :> es, State AlexState :> es) => (PPToken, [String]) -> Eff es a
parseError (t, tokens) = error $ "something failed :(, failed on token: \"" ++  show t ++ "\"possible tokens: " ++ show tokens  -- throwError "failure :("





lexer :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) =>  (PPToken -> Eff es a) -> Eff es a
--lexer = ((alexMonadScan >>= (\t -> const t <$> (liftIO $ print ( "before: " ++ show t))) ) >>=)
lexer f = do
    s@PreprocessorState{buf=oq} <- get @PreprocessorState
    case oq of
        -- pull a token from the outQueue
        h S.:<| t -> put (s{buf=t}) >> f h
        S.Empty -> alexMonadScan >>= f

--((alexMonadScan) >>=) 
--lexer :: (IOE :> es, Error String :> es, State AlexState :> es) =>  (PPToken -> Eff es a) -> Eff es a


}

{-
runPreprocessor:: T.Text -> Eff (State AlexState ': Error String ': State SymbolTable ': es) a -> Eff es (Either (CallStack, String) a)
runPreprocessor input__ = evalState [M.empty] . runError . evalState (AlexState alexStartPos input__ '\n' [] 0 Nothing)
-}