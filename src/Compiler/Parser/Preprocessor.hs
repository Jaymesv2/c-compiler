module Compiler.Parser.Preprocessor (preprocess, PreprocessorState, printPPTokens, runPreprocessor) where

import Compiler.Parser.PreprocessorGrammar
import Control.Monad

import Data.Functor
import Data.Maybe

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.TokenParsers
import Compiler.Parser.Tokens

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import Data.Sequence qualified as S

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data MacroDef = ObjectMacro [PPToken] | FuncMacro [Identifier] Bool [PPToken]

data PPMode
    = SkipBranchMode
    | InBranchMode
    deriving stock (Show, Eq)

data PreprocessorState = PreprocessorState
    -- when an include is encountered the current alex state is pushed onto this stack
    -- and a new one for the new file replaces the current AlexState.
    -- When the current file is fully tokenized and processed we return to the last file
    { lexStack :: [AlexState]
    , -- The outstack is a buffer of tokens that is popped from
      outQueue :: [PPToken]
    , mode :: [PPMode]
    , -- This queue holds the tokens before they go into the parser
      buf :: S.Seq PPToken
    , macroSymTbl :: M.Map T.Text MacroDef
    , concatLookahead :: PPToken
    }

newPreprocessorState :: PreprocessorState
newPreprocessorState = PreprocessorState{lexStack = [], macroSymTbl = M.empty, mode = [], outQueue = [], buf = S.empty, concatLookahead = PPSpecial PPNewline}

handleInclude :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => [PPToken] -> Eff es ()
handleInclude toks = do
    -- TODO: properly concatenate tokens here
    -- TODO: properly resolve headers
    let file = case toks of
            [PPStringLiteral s] -> s
            _ -> error "partially unimplemented: cannot resolve headers other than string literals"

    inp <- liftIO $ TIO.readFile (T.unpack file)
    old_state <- get @AlexState
    modify (\s@PreprocessorState{lexStack = ls} -> s{lexStack = old_state : ls})
    put @AlexState $ newAlexState inp

-- skip to the next corresponding elif/else/endif
skipIfBlock :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => Bool -> Eff es PPIfLine
skipIfBlock skipToEnd = go 0
  where
    -- when skipToEnd is false, return at the next the next elif/else/endif
    -- when skipToEnd is true, return at the next the next endif
    go :: (State PreprocessorState :> es, State AlexState :> es, Error String :> es) => Int -> Eff es PPIfLine
    go nesting =
        parseLine >>= \case
            IfLine l -> case l of
                -- increase nesting when we enter a new block
                ILIf _toks -> go $ nesting + 1
                ILIfDef _ -> go $ nesting + 1
                ILIfNDef _ -> go $ nesting + 1
                ILElIf _ | nesting /= 0 -> go nesting -- ignore if it is nested
                ILElse | nesting /= 0 -> go nesting -- ignore nested

                -- if skipToEnd is false and it is at the same level return it
                ILElIf _toks | not skipToEnd && nesting == 0 -> pure l
                ILElse | not skipToEnd && nesting == 0 -> pure l
                ILElse | skipToEnd && nesting == 0 -> go nesting
                -- corresponding #endif found, return
                ILEndIf | nesting /= 0 -> go (nesting - 1)
                ILEndIf | nesting == 0 -> pure l
                _ -> error "shit and die"
            -- ignore non control flow lines
            _ -> go nesting

handleIfLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPIfLine -> Eff es ()
handleIfLine ifl = do
    PreprocessorState{mode = m} <- get @PreprocessorState
    case ifl of
        ILIf _toks -> error "#if is unimplemented"
        ILIfDef ident ->
            -- if the branch isnt taken skip to the next #elif or
            maybeTakeBranch False . isJust . M.lookup ident . (\PreprocessorState{macroSymTbl = s} -> s) =<< get
        ILIfNDef ident ->
            maybeTakeBranch False . isNothing . M.lookup ident . (\PreprocessorState{macroSymTbl = s} -> s) =<< get
        ILElIf _toks -> case m of
            -- the previous branch was taken so skip to the end
            InBranchMode : _ -> skipIfBlock True $> ()
            -- previous branch was skipped, so check the conditional
            SkipBranchMode : _t -> throwError "Taking #elif branches is unimplemented"
            [] -> throwError "unexpected #elif"
        -- elif can set the state to EndBranchMode when it ends
        ILElse -> case m of
            -- the parser has been in a branch so skip the else
            InBranchMode : _ -> skipIfBlock True $> () -- skip the block
            -- empty implies skippping so take the else
            SkipBranchMode : t -> modify (\s -> s{mode = InBranchMode : t})
            [] -> throwError "unexpected #else encountered"
        ILEndIf -> case m of
            _ : t -> modify (\s -> s{mode = t})
            [] -> throwError "Unexpected #endif"
  where
    maybeTakeBranch shouldSkipToEnd b =
        if b
            then modify (\s@PreprocessorState{mode = m} -> s{mode = InBranchMode : m})
            else
                modify (\s@PreprocessorState{mode = m} -> s{mode = SkipBranchMode : m})
                    >> skipIfBlock shouldSkipToEnd
                    >>= handleIfLine
                    >> pure ()

handleLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPLine -> Eff es [PPToken]
handleLine line = case line of
    TextLine line' -> pure line'
    IfLine l -> handleIfLine l $> []
    ControlLine (CLInclude toks) -> handleInclude toks $> []
    ControlLine (CLDefineObj name val) -> do
        s@PreprocessorState{macroSymTbl = mSymTbl} <- get
        case M.lookup name mSymTbl of
            Nothing -> put s{macroSymTbl = M.insert name (ObjectMacro val) mSymTbl}
            -- TODO: This should not throw an error if the replacement lists are identical
            Just _tbl -> throwError ("macro \"" ++ T.unpack name ++ "\" is already defined")
        pure []
    ControlLine (CLDefineFunc name args variadic val) -> do
        s@PreprocessorState{macroSymTbl = mSymTbl} <- get
        case M.lookup name mSymTbl of
            Nothing -> put s{macroSymTbl = M.insert name (FuncMacro args variadic val) mSymTbl}
            Just _tbl -> throwError ("macro \"" ++ T.unpack name ++ "\" is already defined")
        pure []
    ControlLine (CLUndef name) -> do
        modify (\s@PreprocessorState{macroSymTbl = mSymTbl} -> s{macroSymTbl = M.delete name mSymTbl})
        pure []
    ControlLine (CLLine _) -> do
        throwError "Line control is unimplemented"
    ControlLine (CLError _) -> do
        throwError "Compile errors are is unimplemented"
    ControlLine (CLPragma _) -> do
        throwError "Pragmas are is unimplemented"
    ControlLine CLEmpty -> do
        pure []
    ControlLine CLParseError -> do
        liftIO $ print ("Encountered a parse error while parsing a control line" :: T.Text)
        pure []
    NonDirective name -> do
        liftIO . print $ "Encountered invalid directive \"" ++ T.unpack name ++ "\", ignoring"
        pure []
    PPSEnd -> do
        s@PreprocessorState{lexStack = ls} <- get
        case ls of
            [] -> pure [PPEOF]
            h : t -> do
                put h
                put (s{lexStack = t})
                pure []

expandTokenLine :: M.Map T.Text MacroDef -> [PPToken] -> [PPToken {- :: (State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es () -}]
expandTokenLine macros = go
  where
    go [] = []
    go ((PPStringLiteral s1) : (PPStringLiteral s2) : t) = go $ PPStringLiteral (T.append s1 s2) : t
    go ((PPIdent ident) : t) = case M.lookup ident macros of
        Nothing -> PPIdent ident : go t
        Just (ObjectMacro replacementList) -> replacementList ++ go t
        Just (FuncMacro _args _variadic _replacementList) -> PPIdent ident : go t
    go (h : t) = h : go t

-- gets a token from the token queue and refills the queue if it is empty
ppNextToken :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es, State SymbolTable :> es) => Eff es PPToken
ppNextToken = do
    s@PreprocessorState{outQueue = oq, macroSymTbl = mst, concatLookahead = lk} <- get @PreprocessorState
    case oq of
        -- pull a token from the outQueue
        h : t -> do
            put (s{outQueue = t, concatLookahead = h})
            pure lk
        [] -> case lk of
            PPEOF -> pure PPEOF
            _ -> do
                tokenLine <- parseLine >>= handleLine
                modify (\s' -> s'{outQueue = expandTokenLine mst tokenLine})
                ppNextToken

runPreprocessor :: (State AlexState :> es, Error String :> es) => Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
runPreprocessor = evalState newPreprocessorState . evalState [M.empty]

-- runPreprocessor' :: T.Text -> Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
-- runPreprocessor' inp = runAlex inp . evalState newPreprocessorState . evalState [M.empty]

preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es, State SymbolTable :> es) => Eff es Token
preprocess = do
    nextToken <- ppNextToken
    case nextToken of
        PPHeaderName _x -> error ""
        PPOther _o -> error ""
        PPNumber n -> do
            -- liftIO . print $ "num const" ++ show n
            liftIO . print $ n
            case parseNumConstant n of
                Left _err -> throwError "failed to parse num constant"
                Right c -> pure $ Constant c
        PPCharConst c -> pure $ Constant $ CharConst c
        PPStringLiteral s -> pure $ StringLiteral s
        PPPunctuator punct -> pure $ Punctuator punct
        PPIdent ident -> case ident of
            "auto" -> pure $ Keyword TypeDef
            "break" -> pure $ Keyword Break
            "case" -> pure $ Keyword Case
            "const" -> pure $ Keyword Const
            "continue" -> pure $ Keyword Continue
            "default" -> pure $ Keyword Default
            "do" -> pure $ Keyword Do
            "else" -> pure $ Keyword Else
            "enum" -> pure $ Keyword Enum
            "extern" -> pure $ Keyword Extern
            "for" -> pure $ Keyword For
            "goto" -> pure $ Keyword Goto
            "if" -> pure $ Keyword If
            "inline" -> pure $ Keyword Inline
            "register" -> pure $ Keyword Register
            "restrict" -> pure $ Keyword Restrict
            "return" -> pure $ Keyword Return
            "static" -> pure $ Keyword TStatic
            "sizeof" -> pure $ Keyword Sizeof
            "struct" -> pure $ Keyword Struct
            "switch" -> pure $ Keyword Switch
            "typedef" -> pure $ Keyword TypeDef
            "union" -> pure $ Keyword Union
            "volatile" -> pure $ Keyword Volatile
            "while" -> pure $ Keyword While
            "void" -> pure $ Keyword Void
            "char" -> pure $ Keyword TChar
            "short" -> pure $ Keyword TShort
            "int" -> pure $ Keyword TInt
            "long" -> pure $ Keyword TLong
            "float" -> pure $ Keyword TFloat
            "double" -> pure $ Keyword TDouble
            "signed" -> pure $ Keyword TSigned
            "unsigned" -> pure $ Keyword TUnsigned
            "_Bool" -> pure $ Keyword TuBool
            "_Complex" -> pure $ Keyword TuComplex
            i ->
                M.lookup i . head <$> get @SymbolTable
                    >>= \case
                        Just _ -> pure $ error "cant do things"
                        Nothing -> pure $ Ident i
        -- PPNewline -> preprocess
        PPSpecial PPNewline -> preprocess
        PPSpecial PPSLParen -> pure $ Punctuator LParen
        PPEOF -> pure EOF

printPPTokens :: (IOE :> es, State AlexState :> es, Error String :> es, State PreprocessorState :> es, State SymbolTable :> es) => Eff es ()
printPPTokens = do
    -- token <- preprocess
    token <- ppNextToken
    liftIO $ print token
    case token of
        -- EOF -> pure ()
        PPEOF -> pure ()
        _ -> printPPTokens

{-
runPreprocessor:: T.Text -> Eff (State AlexState ': Error String ': State SymbolTable ': es) a -> Eff es (Either (CallStack, String) a)
runPreprocessor input__ = evalState [M.empty] . runError . evalState (AlexState alexStartPos input__ '\n' [] 0 Nothing)
-}