module Compiler.Parser.Preprocessor (preprocess, PreprocessorState, runPreprocessor) where

import Compiler.Parser.PreprocessorGrammar

import Data.Functor
import Data.Maybe

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.ParseTree (Expr)
import Compiler.Parser.TokenParsers
import Compiler.Parser.Tokens
import Compiler.SymbolTable qualified as SymTbl
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

-- import Data.Sequence qualified as S

import Data.List qualified as L
import Data.Map qualified as M

import Data.Text qualified as T

import Conduit

import Data.Bits (And (getAnd))
import Data.Text.IO qualified as TIO

data MacroDef = ObjectMacro [PPToken] | FuncMacro [Identifier] Bool [PPToken]

data PPMode
    = SkipBranchMode
    | InBranchMode
    deriving stock (Show, Eq)

data PreprocessorState = PreprocessorState
    { macroSymTbl :: M.Map T.Text MacroDef
    }

newPreprocessorState :: PreprocessorState
newPreprocessorState = PreprocessorState{macroSymTbl = M.empty}

handleInclude :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => [PPToken] -> ConduitT i [PPToken] (Eff es) ()
handleInclude toks = do
    -- TODO: properly concatenate tokens here
    -- TODO: properly resolve headers
    let file = case toks of
            [PPStringLiteral s] -> s
            _ -> error "partially unimplemented: cannot resolve headers other than string literals"

    inp <- liftIO $ TIO.readFile (T.unpack file)
    -- save the old state
    -- old_state <- lift $ get @AlexState
    -- lift $ put @AlexState $ newAlexState inp
    -- preprocessInner
    -- lift $ put @AlexState $ old_state
    
    -- run everything through the pipeline again
    alexConduitSource inp .| preprocessInner2
    pure ()

isDefined :: (State PreprocessorState :> es) => Identifier -> Eff es Bool
isDefined ident = (isJust . M.lookup ident . macroSymTbl) <$> get

evalConditional :: (State PreprocessorState :> es) => [PPToken] -> Eff es Bool
evalConditional toks = do
    -- print a warning 
    pure False

-- decides whether to skip a 
handleIfLine :: (State PreprocessorState :> es, Error String :> es) => PPIfLine -> ConduitT PPLine PPLine (Eff es) ()
handleIfLine line = case line of 
    (ILIf toks) -> lift (evalConditional toks) >>= shouldTakeBranch
    (ILIfDef ident) -> lift (isDefined ident) >>= shouldTakeBranch
    (ILIfNDef ident) ->  lift (isDefined ident) >>= shouldTakeBranch . not
    _ -> pure () -- TODO : print warnings for the other cases
    where
        shouldTakeBranch x = if x then takeBranch else skipBranch
        -- this can probably replace 
        {- lineScanner 
            :: (PPIfLine -> ConduitT PPLine PPLine (Eff es) ()) 
            -> (PPIfLine -> ConduitT PPLine PPLine (Eff es) ()) 
            -> ConduitT PPLine PPLine (Eff es) ()
            -> (PPLine -> ConduitT PPLine PPLine (Eff es) ()) 
            -> ConduitT PPLine PPLine (Eff es) ()
        lineScanner newConditionalM elIfM elseM defaultM = await >>= maybe (pure ()) (\case
            IfLine t@(ILIf _) -> newConditionalM t
            IfLine t@(ILIfDef _) -> newConditionalM t
            IfLine t@(ILIfNDef _) -> newConditionalM t
            IfLine t@(ILElIf _) -> elIfM t
            IfLine ILElse -> elseM
            IfLine ILEndIf -> pure ()
            l -> defaultM l) -}

        skipBranch :: (State PreprocessorState :> es, Error String :> es) => ConduitT PPLine PPLine (Eff es) ()
        skipBranch = await >>= \case
            Nothing -> pure () 

            -- an inner conditional was found so ignore it
            Just (IfLine t@(ILIf _)) -> finishBranch >> skipBranch
            Just (IfLine t@(ILIfDef _)) -> finishBranch >> skipBranch
            Just (IfLine t@(ILIfNDef _)) -> finishBranch >> skipBranch
            Just (IfLine t@(ILElIf toks)) -> lift (evalConditional toks) >>= shouldTakeBranch
            Just (IfLine t@(ILElse)) -> takeBranch -- take the else
            Just (IfLine t@(ILEndIf)) -> pure () -- end of the conditional
            Just _ -> skipBranch

        takeBranch :: (State PreprocessorState :> es, Error String :> es) => ConduitT PPLine PPLine (Eff es) ()
        takeBranch = await >>= \case
            Nothing -> pure () -- TODO: issue a warning about not having an ending for an #if 

            -- start a new conditional
            Just (IfLine t@(ILIf _)) -> handleIfLine t >> takeBranch
            Just (IfLine t@(ILIfDef _)) -> handleIfLine t >> takeBranch
            Just (IfLine t@(ILIfNDef _)) -> handleIfLine t >> takeBranch

            Just (IfLine t@(ILElIf _)) -> finishBranch -- 
            Just (IfLine t@(ILElse)) -> finishBranch
            
            Just (IfLine t@(ILEndIf)) -> pure () -- end of the conditional

            Just line -> yield line >> takeBranch


        -- skips the the EndIf
        finishBranch :: (State PreprocessorState :> es, Error String :> es) => ConduitT PPLine PPLine (Eff es) ()
        finishBranch = await >>= \case
            Nothing -> pure () 

            -- an inner conditional was found so ignore it
            Just (IfLine t@(ILIf _)) -> finishBranch >> finishBranch
            Just (IfLine t@(ILIfDef _)) -> finishBranch >> finishBranch
            Just (IfLine t@(ILIfNDef _)) -> finishBranch >> finishBranch
            Just (IfLine t@(ILElIf _)) -> finishBranch
            Just (IfLine t@(ILElse)) -> finishBranch
            Just (IfLine t@(ILEndIf)) -> pure () -- end of the conditional
            Just line -> finishBranch




-- takeIf :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => ConduitT PPLine PPLine (Eff es) PPLine

--awaitOrStop :: Monad m => (i -> ConduitT i o m ()) -> ConduitT i o m ()
--awaitOrStop action = await >>= maybe (pure ()) action


handleControlLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPControlLine -> ConduitT PPLine [PPToken] (Eff es) ()
handleControlLine (CLInclude toks) = handleInclude toks >> pure ()
handleControlLine (CLDefineObj name val) = do
    s@PreprocessorState{macroSymTbl = mSymTbl} <- lift get
    case M.lookup name mSymTbl of
        Nothing -> lift $ put s{macroSymTbl = M.insert name (ObjectMacro val) mSymTbl}
        -- TODO: This should not throw an error if the replacement lists are identical
        Just _tbl -> lift $ throwError ("macro \"" ++ T.unpack name ++ "\" is already defined")
    pure ()
handleControlLine (CLDefineFunc name args variadic val) = do
    s@PreprocessorState{macroSymTbl = mSymTbl} <- lift get
    case M.lookup name mSymTbl of
        Nothing -> lift $ put s{macroSymTbl = M.insert name (FuncMacro args variadic val) mSymTbl}
        Just _tbl -> lift $ throwError ("macro \"" ++ T.unpack name ++ "\" is already defined")
    pure ()
handleControlLine (CLUndef name) = do
    lift $ modify (\s -> s{macroSymTbl = M.delete name (macroSymTbl s)})
handleControlLine (CLLine _) = do
    error "Line control is unimplemented"
handleControlLine (CLError _) = do
    error "Compile errors are is unimplemented"
handleControlLine (CLPragma _) = do
    error "Pragmas are unimplemented"
handleControlLine CLEmpty = pure ()
handleControlLine CLParseError = do
    liftIO $ print ("Encountered a parse error while parsing a control line" :: T.Text)


preprocessLines :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => ConduitT PPLine [PPToken] (Eff es) ()
preprocessLines = awaitForever $ \case
    TextLine line' -> yield line'
    IfLine l -> handleIfLine l  .| preprocessLines
    ControlLine l -> handleControlLine l
    NonDirective name -> liftIO . print $ "Encountered invalid directive \"" ++ T.unpack name ++ "\", ignoring"
    -- PPSEnd -> error ""


preprocessInner2 :: (IOE :> es, Error String :> es, State PreprocessorState :> es, State AlexState :> es) => ConduitT PPToken [PPToken] (Eff es) ()
preprocessInner2 = chunkLines  .| mapMC parseLine .| preprocessLines

preprocess2 :: (IOE :> es, Error String :> es, State PreprocessorState :> es, State AlexState :> es) => ConduitT PPToken Token (Eff es) ()
preprocess2 = preprocessInner2 .| mapMC expandTokenLineC .| mapC mergeStringLiterals .| concatC .| ppTokensToTokens convertIdent


-- runPreprocessor' :: T.Text -> Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
-- runPreprocessor' inp = runAlex inp . evalState newPreprocessorState . evalState [M.empty]
preprocessInner :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => ConduitT i [PPToken] (Eff es) ()
preprocessInner = alexConduit .| chunkLines  .| mapMC parseLine .| preprocessLines

preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => ConduitT i Token (Eff es) ()
preprocess = preprocessInner .| mapMC expandTokenLineC .| mapC mergeStringLiterals .| concatC .| ppTokensToTokens convertIdent

expandTokenLineC :: (Error String :> es, State PreprocessorState :> es) => [PPToken] -> Eff es [PPToken]
expandTokenLineC inp = get >>= (`expandTokenLine` inp) . macroSymTbl 

runPreprocessor :: (State AlexState :> es, Error String :> es) => Eff (State PreprocessorState ': es) a -> Eff es a
runPreprocessor = evalState newPreprocessorState 

-- .| concatC


    
    

chunkLines :: (Error String :> es) => ConduitT PPToken [PPToken] (Eff es) ()
chunkLines = loop []
    where 
        loop xs = await >>= \case
            Nothing -> yield xs
            Just (PPSpecial PPNewline) -> yield (reverse xs) >> loop []
            Just x -> loop (x:xs)

ppTokensToTokens :: (Error String :> es) => (Identifier -> Token) -> ConduitT PPToken Token (Eff es) ()
ppTokensToTokens f = awaitForever $ \case
    PPHeaderName _x -> lift $ throwError ""
    PPOther _o -> lift $ throwError ""
    PPNumber n -> case parseNumConstant n of
      Left _err -> lift $ throwError "failed to parse num constant"
      Right c -> yield (Constant c)
    PPCharConst c -> yield (Constant $ CharConst c)
    PPStringLiteral s -> yield (StringLiteral s) 
    PPPunctuator punct -> yield (Punctuator punct) 
    PPIdent ident -> yield (f ident) 
    PPSpecial PPNewline -> pure ()
    PPSpecial PPSLParen -> yield (Punctuator LParen)


mergeStringLiterals :: [PPToken] -> [PPToken]
mergeStringLiterals (PPStringLiteral s1 : PPStringLiteral s2 : t) = PPStringLiteral (T.append s1 s2) : mergeStringLiterals t
mergeStringLiterals (h : t) = h : mergeStringLiterals t
mergeStringLiterals [] = []

{-
expand each argument, then paste each into the stream and expand again
-}

expandTokenLine :: (Error String :> es) => M.Map T.Text MacroDef -> [PPToken] -> Eff es [PPToken {- :: (State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es () -}]
expandTokenLine macros = fmap mergeStringLiterals . go []
  where
    go acc [] = pure (reverse acc)
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
    getFuncMacroArgs' :: (Error String :> es) => [[PPToken]] -> [PPToken] -> Eff es ([[PPToken]], [PPToken])
    getFuncMacroArgs' acc toks = case getArg False toks of
        Left _ -> throwError ""
        Right (True, arg, remToks) -> getFuncMacroArgs' (arg : acc) remToks
        Right (False, arg, remToks) -> pure (reverse $ arg : acc, remToks)

    expandFuncMacro :: (Error String :> es) => [Identifier] -> Bool -> [PPToken] -> [PPToken] -> Eff es ([PPToken], [PPToken])
    expandFuncMacro argNames variadic replacementList toks = do
        toks' <- case toks of
            PPSpecial PPSLParen : t -> pure t
            PPPunctuator LParen : t -> pure t
            _ -> throwError ""

        (args, remToks) <- getFuncMacroArgs' [] toks'
        pairs <- zipArgs [] argNames args
        let newMacros = L.foldl' (\acc (name, body) -> M.insert name (ObjectMacro body) acc) macros pairs
        expandedReplacementList <- expandTokenLine newMacros replacementList
        pure (reverse expandedReplacementList, remToks)
      where
        zipArgs :: (Error String :> es) =>  [(Identifier, [PPToken])] -> [Identifier] -> [[PPToken]] -> Eff es [(Identifier, [PPToken])]
        zipArgs acc (hi : ti) (ha : ta) = zipArgs ((hi, ha) : acc) ti ta
        -- correct number of args
        zipArgs acc [] [] | variadic = pure (("__VA_ARGS__", []) : acc)
        zipArgs acc [] [] = pure acc
        -- extra args
        zipArgs acc [] remArgs@(_ : _) | variadic = pure $ ("__VA_ARGS__", L.intercalate [PPPunctuator Comma] remArgs) : acc
        zipArgs _ [] (_ : _) = throwError ""
        zipArgs _ _ [] = throwError ""

-- gets a token from the token queue and refills the queue if it is empty
-- ppNextToken :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => Eff es PPToken
-- ppNextToken = do
--     s@PreprocessorState{outQueue = oq, macroSymTbl = mst, concatLookahead = lk} <- get @PreprocessorState
--     case oq of
--         -- pull a token from the outQueue
--         h : t -> put (s{outQueue = t, concatLookahead = h}) $> lk
--         -- end of input so stop
--         [] | lk == PPEOF -> pure PPEOF
--         -- get more
--         [] -> do
--             tokenLine <- getAndParseLine >>= preprocessLine
--             case expandTokenLine mst tokenLine of
--                 Left () -> throwError "failed to expand tokens"
--                 Right expanded -> do
--                     modify (\s' -> s'{outQueue = expanded})
--                     ppNextToken


convertIdent :: Identifier -> Token
convertIdent ident = case ident of
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

