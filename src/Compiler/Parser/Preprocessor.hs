module Compiler.Parser.Preprocessor (preprocess, PreprocessorState, printPPTokens, runPreprocessor) where

import Compiler.Parser.PreprocessorGrammar

import Data.Functor
import Data.Maybe

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.ParseTree (Expr)
import Compiler.Parser.TokenParsers
import Compiler.Parser.Tokens
import Compiler.SymbolTable (SymbolTable)
import Compiler.SymbolTable qualified as SymTbl

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import Data.Sequence qualified as S

import Data.List qualified as L
import Data.Map qualified as M

import Data.Text qualified as T

import Data.Bits (And (getAnd))
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
        do
            getAndParseLine >>= \case
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
                    ILEndIf | nesting /= 0 -> go (nesting - 1)
                    ILEndIf | nesting == 0 -> pure l
                    _ -> error "shit and die"
                -- ignore non control flow lines
                _ -> go nesting

handleIfLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPIfLine -> Eff es ()
handleIfLine ifl = do
    m <- mode <$> get
    case ifl of
        ILIf toks -> do
            mst <- macroSymTbl <$> get
            case expandTokenLine mst toks of
                Left _err -> throwError "Failed to expand tokens"
                Right expanded -> do
                    error "#if is unimplemented"
        -- error "#if is unimplemented"
        ILIfDef ident ->
            -- if the branch isnt taken skip to the next #elif or
            maybeTakeBranch False . isJust . M.lookup ident . (\PreprocessorState{macroSymTbl = s} -> s) =<< get
        ILIfNDef ident ->
            maybeTakeBranch False . isNothing . M.lookup ident . (\PreprocessorState{macroSymTbl = s} -> s) =<< get
        ILElIf _toks -> case m of
            -- the previous branch was taken so skip to the end
            InBranchMode : _ -> skipIfBlock True $> ()
            -- previous branch was skipped, so check the conditional
            SkipBranchMode : _t -> error "Taking #elif branches is unimplemented"
            [] -> error "unexpected #elif"
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
            then modify (\s -> s{mode = InBranchMode : mode s})
            else
                modify (\s -> s{mode = SkipBranchMode : mode s})
                    >> skipIfBlock shouldSkipToEnd
                    >>= handleIfLine
                    >> pure ()

-- I'm not sure if GHC will tail optimize this
getPPTokenLine :: (Error String :> es, State AlexState :> es) => Eff es [PPToken]
getPPTokenLine = do
    tok <- alexMonadScan
    case tok of
        PPSpecial PPNewline -> pure []
        PPEOF -> pure [tok]
        _ -> fmap (tok :) getPPTokenLine

getAndParseLine :: (Error String :> es, State AlexState :> es) => Eff es PPLine
getAndParseLine =
    getPPTokenLine >>= \case
        [PPEOF] -> pure PPSEnd
        other -> parseLine other

preprocessLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPLine -> Eff es [PPToken]
preprocessLine line = case line of
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
        modify (\s -> s{macroSymTbl = M.delete name (macroSymTbl s)})
        pure []
    ControlLine (CLLine _) -> do
        error "Line control is unimplemented"
    ControlLine (CLError _) -> do
        error "Compile errors are is unimplemented"
    ControlLine (CLPragma _) -> do
        error "Pragmas are is unimplemented"
    ControlLine CLEmpty -> do
        pure []
    ControlLine CLParseError -> do
        liftIO $ print ("Encountered a parse error while parsing a control line" :: T.Text)
        pure []
    NonDirective name -> do
        liftIO . print $ "Encountered invalid directive \"" ++ T.unpack name ++ "\", ignoring"
        pure []
    PPSEnd -> do
        s <- get
        case lexStack s of
            [] -> pure [PPEOF]
            h : t -> do
                put h
                put (s{lexStack = t})
                pure []

mergeStringLiterals :: [PPToken] -> [PPToken]
mergeStringLiterals (PPStringLiteral s1 : PPStringLiteral s2 : t) = PPStringLiteral (T.append s1 s2) : mergeStringLiterals t
mergeStringLiterals (h : t) = h : mergeStringLiterals t
mergeStringLiterals [] = []

{-
takeWhileState :: ((b, a) -> (b, Bool)) -> b -> [a] -> [a]
takeWhileState _ _ [] = []
takeWhileState f st (h : t) = case f (st, h) of
    (newSt, True) -> h : takeWhileState f newSt t
    (_, False) -> []

stateUpdater :: (Int, PPToken) -> (Int, Bool)
stateUpdater inp = case inp of
    -- base cases
    (0, PPPunctuator RParen) -> (0, False) -- reached end of function call
    (0, PPPunctuator Comma) -> (0, False) -- reached comma, stop
    -- handle nesting
    (n, PPPunctuator LParen) -> (n + 1, True) -- increase nesting
    (n, PPPunctuator RParen) -> (n - 1, True) -- decrease nesting
    (n, _) -> (n, True)
-}

{-
expand each argument, then paste each into the stream and expand again
-}

expandTokenLine :: M.Map T.Text MacroDef -> [PPToken] -> Either () [PPToken {- :: (State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es () -}]
expandTokenLine macros = fmap mergeStringLiterals . go []
  where
    go acc [] = Right (reverse acc)
    go acc ((PPIdent ident) : t) = case M.lookup ident macros of
        Nothing -> go (PPIdent ident : acc) t
        Just (ObjectMacro replacementList) -> go (replacementList ++ acc) t
        Just (FuncMacro args variadic replacementList) -> expandFuncMacro args variadic replacementList t >>= \(list, remToks) -> go (list ++ acc) remToks
    go acc (h : t) = go (h : acc) t

    -- tail recursive function which returns tokens until a comma is found.
    -- commas within nested function calls are ignored

    getArg' :: Int -> [PPToken] -> Bool -> [PPToken] -> Either [PPToken] (Bool, [PPToken], [PPToken])
    getArg' 0 acc False (PPPunctuator Comma : t) = Right (True, reverse acc, t)
    getArg' 0 acc _ (PPPunctuator RParen : t) = Right (False, reverse acc, t)
    getArg' n acc ignoreCommas (PPPunctuator RParen : t) = getArg' (n - 1) (PPPunctuator RParen : acc) ignoreCommas t
    getArg' n acc ignoreCommas (PPSpecial PPSLParen : t) = getArg' (n + 1) (PPSpecial PPSLParen : acc) ignoreCommas t
    getArg' n acc ignoreCommas (h : t) = getArg' n (h : acc) ignoreCommas t
    getArg' _ acc _ [] = Left $ reverse acc

    getArg :: Bool -> [PPToken] -> Either [PPToken] (Bool, [PPToken], [PPToken])
    getArg = getArg' 0 []

    -- splits the args
    getFuncMacroArgs' :: [[PPToken]] -> [PPToken] -> Either () ([[PPToken]], [PPToken])
    getFuncMacroArgs' acc toks = case getArg False toks of
        Left _ -> Left ()
        Right (True, arg, remToks) -> getFuncMacroArgs' (arg : acc) remToks
        Right (False, arg, remToks) -> Right (reverse $ arg : acc, remToks)

    expandFuncMacro :: [Identifier] -> Bool -> [PPToken] -> [PPToken] -> Either () ([PPToken], [PPToken])
    expandFuncMacro argNames variadic replacementList toks = do
        toks' <- case toks of
            PPSpecial PPSLParen : t -> Right t
            PPPunctuator LParen : t -> Right t
            _ -> Left ()

        (args, remToks) <- getFuncMacroArgs' [] toks'
        pairs <- zipArgs [] argNames args
        let newMacros = L.foldl' (\acc (name, body) -> M.insert name (ObjectMacro body) acc) macros pairs
        expandedReplacementList <- expandTokenLine newMacros replacementList
        pure (reverse expandedReplacementList, remToks)
      where
        zipArgs :: [(Identifier, [PPToken])] -> [Identifier] -> [[PPToken]] -> Either () [(Identifier, [PPToken])]
        zipArgs acc (hi : ti) (ha : ta) = zipArgs ((hi, ha) : acc) ti ta
        -- correct number of args
        zipArgs acc [] [] | variadic = Right (("__VA_ARGS__", []) : acc)
        zipArgs acc [] [] = Right acc
        -- extra args
        zipArgs acc [] remArgs@(_ : _) | variadic = Right $ ("__VA_ARGS__", L.intercalate [PPPunctuator Comma] remArgs) : acc
        zipArgs _ [] (_ : _) = Left ()
        zipArgs _ _ [] = Left ()

-- gets a token from the token queue and refills the queue if it is empty
ppNextToken :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es, State SymbolTable :> es) => Eff es PPToken
ppNextToken = do
    s@PreprocessorState{outQueue = oq, macroSymTbl = mst, concatLookahead = lk} <- get @PreprocessorState
    case oq of
        -- pull a token from the outQueue
        h : t -> put (s{outQueue = t, concatLookahead = h}) $> lk
        -- end of input so stop
        [] | lk == PPEOF -> pure PPEOF
        -- get more
        [] -> do
            tokenLine <- getAndParseLine >>= preprocessLine
            case expandTokenLine mst tokenLine of
                Left () -> throwError "failed to expand tokens"
                Right expanded -> do
                    modify (\s' -> s'{outQueue = expanded})
                    ppNextToken

runPreprocessor :: (State AlexState :> es, Error String :> es) => Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
runPreprocessor = evalState newPreprocessorState . evalState SymTbl.empty

-- runPreprocessor' :: T.Text -> Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
-- runPreprocessor' inp = runAlex inp . evalState newPreprocessorState . evalState [M.empty]

preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es, State SymbolTable :> es) => Eff es Token
preprocess = do
    nextToken <- ppNextToken
    symTbl <- get
    case ppTokenToToken (convertIdent symTbl) nextToken of
        Left () -> throwError "something"
        Right Nothing -> preprocess
        Right (Just t) -> pure t

convertIdent :: SymbolTable -> Identifier -> Token
convertIdent symtbl ident = case ident of
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
    i ->
        if SymTbl.isType i symtbl
            then TTypeName i
            else Ident i

ppTokenToToken :: (Identifier -> Token) -> PPToken -> Either () (Maybe Token)
ppTokenToToken f tok =
    case tok of
        PPHeaderName _x -> Left ()
        PPOther _o -> Left ()
        PPNumber n ->
            case parseNumConstant n of
                Left _err -> Left () -- throwError "failed to parse num constant"
                Right c -> Right . Just $ Constant c
        PPCharConst c -> Right . Just $ Constant $ CharConst c
        PPStringLiteral s -> Right . Just $ StringLiteral s
        PPPunctuator punct -> Right . Just $ Punctuator punct
        PPIdent ident -> Right . Just $ f ident
        PPSpecial PPNewline -> Right Nothing
        PPSpecial PPSLParen -> Right . Just $ Punctuator LParen
        PPEOF -> Right . Just $ EOF

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
