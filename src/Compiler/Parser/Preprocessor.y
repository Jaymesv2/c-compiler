
{
    

module Compiler.Parser.Preprocessor where

import Data.Functor

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.Tokens

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import qualified Data.Map as M
import qualified Data.Text as T

}



%name preprocessor 

%monad {(Error String :> es, State AlexState :> es, State SymbolTable :> es) }  {Eff es} {>>=} {return}
%lexer {lexer} {PPEOF}
--%lexer {(alexMonadScan >>=)} {EOF}

%errorhandlertype explist
%error {parseError}
%tokentype { PPToken }

%token
    headername  { PPHeaderName $$ }
    ident       { PPIdent $$ }
    number      { PPNumber $$ }
    char        { PPCharConst $$ }
    stringlit   { PPStringLiteral $$ }
    --punct       { PPPunctuator Punctuator }
    '#'         { PPPunctuator Stringize }
    other       { PPOther $$ }
    '\n'        { PPNewline }

{-
How to write the preprocessor:
    The lexer needs handle outputting lines

-}
%%



PPFile 
    : ident { $1 }

{-
PPFile :: {  }
    : Group

Group  :: {  }
    : GroupPart
    | Group GroupPart

GroupPart  ::  { }
    : IfSection     {  }
    | ControlLine   {  }
    | TextLine      {  }
    | '#' NonDirective {}

IfSection :: {  }
    : '#' 
    | '#'
    | '#'

IfGroup :: {}
    : 
-}



{
data MacroDef = ObjectMacro {} | FuncMacro {}

data PreprocessorState = PreprocessorState
    -- when an include is encountered the current alex state is pushed onto this stack
    -- and a new one for the new file replaces the current AlexState.
    -- When the current file is fully tokenized and processed we return to the last file
    { lexStack :: [AlexState]
    , lookahead :: PPToken
    , macroSymTbl :: M.Map T.Text MacroDef
    , code :: Int
    }

newPreprocessorState :: PreprocessorState
newPreprocessorState = PreprocessorState{lexStack = [], lookahead = PPNewline, macroSymTbl = M.empty, code = 0}

ppNextToken :: (State PreprocessorState :> es, Error String :> es, State AlexState :> es, State SymbolTable :> es) => Eff es PPToken
ppNextToken = do
    nextToken <- alexMonadScan
    state @PreprocessorState (\s@PreprocessorState{lookahead = lk} -> (lk, s{lookahead = nextToken}))

getLookahead :: (State PreprocessorState :> es) => Eff es PPToken
getLookahead = get <&> \s@PreprocessorState{lookahead = lk} -> lk

handle :: (Error String :> es, State AlexState :> es, State SymbolTable :> es, State PreprocessorState :> es) => Eff es Token
handle = error ""

preprocess :: (Error String :> es, State AlexState :> es, State SymbolTable :> es, State PreprocessorState :> es) => Eff es Token
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


parseError :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) => (PPToken, [String]) -> Eff es a
parseError (t, tokens) = error $ "something failed :(, failed on token: \"" ++  show t ++ "\"possible tokens: " ++ show tokens  -- throwError "failure :("


lexer :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) =>  (PPToken -> Eff es a) -> Eff es a
lexer = error ""

}

{-
runPreprocessor:: T.Text -> Eff (State AlexState ': Error String ': State SymbolTable ': es) a -> Eff es (Either (CallStack, String) a)
runPreprocessor input__ = evalState [M.empty] . runError . evalState (AlexState alexStartPos input__ '\n' [] 0 Nothing)
-}