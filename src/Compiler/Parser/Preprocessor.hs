module Compiler.Parser.Preprocessor (preprocess, PreprocessorState, runPreprocessor, PreprocessorOptions (..)) where

--import Compiler.Parser.PreprocessorGrammar

import Compiler.Parser.SrcLoc

import Data.Functor
import Data.Maybe

import Compiler.Parser
import Compiler.Parser.Lexer
import Compiler.Parser.TokenParsers
import Compiler.Parser.Tokens

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Dispatch.Static
-- import Data.Sequence qualified as S

import Data.List qualified as L
import Data.Map qualified as M


import Data.Text qualified as T

import Conduit
import Data.Conduit.Combinators qualified as CC

import Data.Text.IO qualified as TIO

import System.FilePath ()
import Effectful.FileSystem ( runFileSystem, FileSystem, makeAbsolute, findFile )
import Effectful.FileSystem.IO.File


-- example for making an effect
{-
data Logger = Logger { logMessage :: String -> IO () }
data Log :: Effect
type instance DispatchOf Log = Static WithSideEffects
newtype instance StaticRep Log = Log Logger

log :: Log :> es => String -> Eff es ()
log msg = do
  Log logger <- getStaticRep
  unsafeEff_ $ logMessage logger msg

runLog :: (IOE :> es) => Logger -> Eff (Log : es) a -> Eff es a
runLog logger = evalStaticRep (Log logger)
-}

newtype PreprocessorOptions = PPOptions {
    includePaths :: [String]
}





data MacroDef = ObjectMacro [Located PPToken] | FuncMacro [Identifier] Bool [Located PPToken]

data PPMode
    = SkipBranchMode
    | InBranchMode
    deriving stock (Show, Eq)

data PreprocessorState = PreprocessorState
    { macroSymTbl :: M.Map T.Text MacroDef,
      opts :: PreprocessorOptions
    }

--newPreprocessorState :: PreprocessorState


--newPreprocessorState :: PreprocessorState
newPreprocessorState :: PreprocessorOptions -> PreprocessorState
newPreprocessorState opts = PreprocessorState{macroSymTbl = M.empty, opts=opts}


handleInclude :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => [Located PPToken] -> ConduitT i [Located PPToken] (Eff es) ()
handleInclude toks = do
    searchPaths <- includePaths . opts <$> lift get
    
    -- TODO: properly concatenate tokens here
    -- TODO: properly resolve headers
    let (fileName,includeSystem, loc) = case toks of
            -- don't search system directories
            [L s (PPStringLiteral fileName_)] -> (T.unpack fileName_, False,s)
            -- search system directories
            [L s (PPHeaderName fileName_)] -> (T.unpack fileName_, True,s)
            _ -> error "partially unimplemented: cannot resolve headers other than string literals"

    (contents,filePath) <- lift $ runFileSystem $ do
        file <- findFile searchPaths fileName >>= \case
            Nothing -> throwError $ "Cannot find file " ++ fileName ++ " "
            Just f -> pure f
        conts <- liftIO $ TIO.readFile file
        pure (conts, file)
    alexConduitSource contents filePath .|  preprocessInner2
    pure ()

isDefined :: (State PreprocessorState :> es) => Identifier -> Eff es Bool
isDefined ident = isJust . M.lookup ident . macroSymTbl <$> get


evalConditional :: (State PreprocessorState :> es) => [Located PPToken] -> Eff es Bool
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
            Just (IfLine (ILIf _)) -> finishBranch >> skipBranch
            Just (IfLine (ILIfDef _)) -> finishBranch >> skipBranch
            Just (IfLine (ILIfNDef _)) -> finishBranch >> skipBranch
            Just (IfLine (ILElIf toks)) -> lift (evalConditional toks) >>= shouldTakeBranch
            Just (IfLine ILElse) -> takeBranch -- take the else
            Just (IfLine ILEndIf) -> pure () -- end of the conditional
            Just _ -> skipBranch
        
        takeBranch :: (State PreprocessorState :> es, Error String :> es) => ConduitT PPLine PPLine (Eff es) ()
        takeBranch = await >>= \case
            Nothing -> pure () -- TODO: issue a warning about not having an ending for an #if 

            -- start a new conditional
            Just (IfLine t@(ILIf _)) -> handleIfLine t >> takeBranch
            Just (IfLine t@(ILIfDef _)) -> handleIfLine t >> takeBranch
            Just (IfLine t@(ILIfNDef _)) -> handleIfLine t >> takeBranch

            Just (IfLine (ILElIf _)) -> finishBranch -- 
            Just (IfLine ILElse) -> finishBranch
            
            Just (IfLine ILEndIf) -> pure () -- end of the conditional

            Just line' -> yield line' >> takeBranch


        -- skips the the EndIf
        finishBranch :: (State PreprocessorState :> es, Error String :> es) => ConduitT PPLine PPLine (Eff es) ()
        finishBranch = await >>= \case
            Nothing -> pure () 

            -- an inner conditional was found so ignore it
            Just (IfLine (ILIf _)) -> finishBranch >> finishBranch
            Just (IfLine (ILIfDef _)) -> finishBranch >> finishBranch
            Just (IfLine (ILIfNDef _)) -> finishBranch >> finishBranch
            Just (IfLine (ILElIf _)) -> finishBranch
            Just (IfLine ILElse) -> finishBranch
            Just (IfLine ILEndIf) -> pure () -- end of the conditional
            Just _ -> finishBranch




-- takeIf :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => ConduitT PPLine PPLine (Eff es) PPLine

--awaitOrStop :: Monad m => (i -> ConduitT i o m ()) -> ConduitT i o m ()
--awaitOrStop action = await >>= maybe (pure ()) action


handleControlLine :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => PPControlLine -> ConduitT PPLine [Located PPToken] (Eff es) ()
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


preprocessLines :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => ConduitT PPLine [Located PPToken] (Eff es) ()
preprocessLines = awaitForever $ \case
    TextLine line' -> yield line'
    IfLine l -> handleIfLine l  .| preprocessLines
    ControlLine l -> handleControlLine l
    NonDirective name -> liftIO . print $ "Encountered invalid directive \"" ++ T.unpack name ++ "\", ignoring"
    -- PPSEnd -> error ""


preprocessInner2 :: (IOE :> es, Error String :> es, State PreprocessorState :> es, State AlexState :> es) => ConduitT (Located PPToken) [Located PPToken] (Eff es) ()
preprocessInner2 = chunkLines  .| mapMC parseLine .| preprocessLines

--preprocess2 :: (IOE :> es, Error String :> es, State PreprocessorState :> es, State AlexState :> es) => ConduitT (Located PPToken) (Located Token) (Eff es) ()
--preprocess2 = preprocessInner2 .| mapMC expandTokenLineC .| mapC mergeStringLiterals .| concatC .| ppTokensToTokens convertIdent


-- runPreprocessor' :: T.Text -> Eff (State SymbolTable ': State PreprocessorState ': es) a -> Eff es a
-- runPreprocessor' inp = runAlex inp . evalState newPreprocessorState . evalState [M.empty]
preprocessInner :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => ConduitT i [Located PPToken] (Eff es) ()
preprocessInner = alexConduit .|  chunkLines  .| mapMC parseLine .| preprocessLines


--preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => ConduitT i (Located Token) (Eff es) ()
preprocess :: (IOE :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es) => ConduitT i Token (Eff es) ()
preprocess = preprocessInner .| mapMC expandTokenLineC .| mapC mergeStringLiterals .| concatC .| ppTokensToTokens convertIdent .| CC.map dropSpans

dropSpans :: Located a -> a
dropSpans (L _ x) = x

expandTokenLineC :: (Error String :> es, State PreprocessorState :> es) => [Located PPToken] -> Eff es [Located PPToken]
expandTokenLineC inp = get >>= (`expandTokenLine` inp) . macroSymTbl 

runPreprocessor :: (State AlexState :> es, Error String :> es) => PreprocessorOptions -> Eff (State PreprocessorState ': es) a -> Eff es a
runPreprocessor opts = evalState (newPreprocessorState opts)

-- .| concatC


    
    

chunkLines :: (Error String :> es) => ConduitT (Located PPToken) [Located PPToken] (Eff es) ()
chunkLines = loop []
    where 
        loop xs = await >>= \case
            Nothing -> yield xs
            Just (L _ (PPSpecial PPNewline)) -> yield (reverse xs) >> loop []
            Just x -> loop (x:xs)

ppTokensToTokens :: (Error String :> es) => (Identifier -> Token) -> ConduitT (Located PPToken) (Located Token) (Eff es) ()
ppTokensToTokens f = awaitForever $ \case
    L s (PPHeaderName _x) -> lift $ throwError ""
    L s (PPOther _o) -> lift $ throwError ""
    L s (PPNumber n) -> case parseNumConstant n of
      Left _err -> lift $ throwError "failed to parse num constant"
      Right c -> yield (L s $ Constant c)
    L s (PPCharConst c) -> yield (L s $ Constant $ CharConst c)
    L s (PPStringLiteral st) -> yield (L s $ StringLiteral st) 
    L s (PPPunctuator punct) -> yield (L s $ Punctuator punct) 
    L s (PPIdent ident) -> yield (L s $ f ident) 
    L _ (PPSpecial PPNewline) -> pure ()
    L s (PPSpecial PPSLParen) -> yield (L s $ Punctuator LParen)


mergeStringLiterals :: [Located PPToken] -> [Located PPToken]
mergeStringLiterals (L sp1 (PPStringLiteral s1) : (L sp2 (PPStringLiteral s2)) : t) = L (sp1 <> sp2) (PPStringLiteral (T.append s1 s2)) : mergeStringLiterals t
mergeStringLiterals (h : t) = h : mergeStringLiterals t
mergeStringLiterals [] = []

{-
expand each argument, then paste each into the stream and expand again
-}

expandTokenLine :: (Error String :> es) => M.Map T.Text MacroDef -> [Located PPToken] -> Eff es [Located PPToken] {- :: (State AlexState :> es, Error String :> es, State PreprocessorState :> es) => Eff es () -}
expandTokenLine macros = fmap mergeStringLiterals . go []
  where
    go acc [] = pure (reverse acc)
    go acc (L s (PPIdent ident) : t) = case M.lookup ident macros of
        Nothing -> go (L s (PPIdent ident) : acc) t
        Just (ObjectMacro replacementList) -> go (replacementList ++ acc) t
        Just (FuncMacro args variadic replacementList) -> expandFuncMacro args variadic replacementList t >>= \(list, remToks) -> go (list ++ acc) remToks
    go acc (h : t) = go (h : acc) t

    -- tail recursive function which returns tokens until a comma is found.
    -- commas within nested function calls are ignored

    getArg' :: Int -> [Located PPToken] -> Bool -> [Located PPToken] -> Either [Located PPToken] (Bool, [Located PPToken], [Located PPToken])
    getArg' 0 acc False (L _ (PPPunctuator Comma) : t) = Right (True, reverse acc, t)
    getArg' 0 acc _ (L _ (PPPunctuator RParen) : t) = Right (False, reverse acc, t)
    getArg' n acc ignoreCommas (L s (PPPunctuator RParen) : t) = getArg' (n - 1) (L s ( PPPunctuator RParen ) : acc) ignoreCommas t
    getArg' n acc ignoreCommas (L s (PPSpecial PPSLParen) : t) = getArg' (n + 1) (L s ( PPSpecial PPSLParen ) : acc) ignoreCommas t
    getArg' n acc ignoreCommas (h : t) = getArg' n (h : acc) ignoreCommas t
    getArg' _ acc _ [] = Left $ reverse acc

    getArg :: Bool -> [Located PPToken] -> Either [Located PPToken] (Bool, [Located PPToken], [Located PPToken])
    getArg = getArg' 0 []

    -- splits the args
    getFuncMacroArgs' :: (Error String :> es) => [[Located PPToken]] -> [Located PPToken] -> Eff es ([[Located PPToken]], [Located PPToken])
    getFuncMacroArgs' acc toks = case getArg False toks of
        Left _ -> throwError ""
        Right (True, arg, remToks) -> getFuncMacroArgs' (arg : acc) remToks
        Right (False, arg, remToks) -> pure (reverse $ arg : acc, remToks)

    expandFuncMacro :: (Error String :> es) => [Identifier] -> Bool -> [Located PPToken] -> [Located PPToken] -> Eff es ([Located PPToken], [Located PPToken])
    expandFuncMacro argNames variadic replacementList toks = do
        toks' <- case toks of
            L _ ( PPSpecial PPSLParen ) : t -> pure t
            L _ (PPPunctuator LParen) : t -> pure t
            _ -> throwError ""

        (args, remToks) <- getFuncMacroArgs' [] toks'
        pairs <- zipArgs [] argNames args
        let newMacros = L.foldl' (\acc (name, body) -> M.insert name (ObjectMacro body) acc) macros pairs
        expandedReplacementList <- expandTokenLine newMacros replacementList
        pure (reverse expandedReplacementList, remToks)
      where
        zipArgs :: (Error String :> es) =>  [(Identifier, [Located PPToken])] -> [Identifier] -> [[Located PPToken]] -> Eff es [(Identifier, [Located PPToken])]
        zipArgs acc (hi : ti) (ha : ta) = zipArgs ((hi, ha) : acc) ti ta
        -- correct number of args
        zipArgs acc [] [] | variadic = pure (("__VA_ARGS__", []) : acc)
        zipArgs acc [] [] = pure acc
        -- extra args
        zipArgs acc [] remArgs@(_ : _) | variadic = pure $ ("__VA_ARGS__", L.intercalate [L (UnhelpfulSpan UnhelpfulGenerated) $ PPPunctuator Comma] remArgs) : acc
        zipArgs _ [] (_ : _) = throwError ""
        zipArgs _ _ [] = throwError ""

-- gets a token from the token queue and refills the queue if it is empty
-- ppNextToken :: (IOE :> es, State PreprocessorState :> es, Error String :> es, State AlexState :> es) => Eff es (Located PPToken)
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





 --parseLine
--parseLine :: (IOE :> es, Error String :> es, State PreprocessorState :> es, State AlexState :> es) => ConduitT (Located PPToken) [Located PPToken] (Eff es) ()
parseLine :: (Error String :> es) => [Located PPToken] -> Eff es PPLine 
parseLine (L s (PPPunctuator Stringize):xs) = parseControlLine s xs
parseLine xs = pure $ TextLine xs

parseControlLine :: (Error String :> es) => SrcSpan -> [Located PPToken] -> Eff es PPLine
parseControlLine s' (L s (PPIdent "if"):xs) = pure $ IfLine $ ILIf xs

parseControlLine s' [L s (PPIdent "ifdef"),L s2 (PPIdent ident)] = pure $ IfLine $ ILIfDef ident
parseControlLine s' (L s (PPIdent "ifdef"):(L s2 (PPIdent ident)):xs) = throwError "invalid tokens after identifier in ifdef line"

parseControlLine s' [L s (PPIdent "ifndef"),L s2 (PPIdent ident)] = pure $ IfLine $ ILIfNDef ident
parseControlLine s' (L s (PPIdent "ifndef"):(L s2 (PPIdent ident)):xs) = throwError $ "invalid tokens after identifier in ifndef line: " ++ show xs
parseControlLine s' (L s (PPIdent "elif"):xs) = pure $ IfLine $ ILElIf xs
parseControlLine s' [L s (PPIdent "else")] = pure $ IfLine ILElse
parseControlLine s' [L s (PPIdent "endif")] = pure $ IfLine ILEndIf



parseControlLine s' (L s (PPIdent "include"):xs) = pure $ ControlLine $ CLInclude xs
parseControlLine s' [ L s (PPIdent "undef"),L s2 (PPIdent ident) ] = pure $ ControlLine $ CLUndef ident
parseControlLine s' ((L s (PPIdent "undef")):((L s2 (PPIdent ident)):xs)) = error ""--pure $ ControlLine $ CLInclude xs
parseControlLine s' (L s (PPIdent "line"):xs) = pure $ ControlLine $ CLLine xs
parseControlLine s' (L s (PPIdent "error"):xs) = pure $ ControlLine $ CLError xs
parseControlLine s' (L s (PPIdent "pragma"):xs) = pure $ ControlLine $ CLPragma xs

parseControlLine s' ((L s (PPIdent "define")):(L s2 (PPSpecial PPSLParen)):xs) = pure $ ControlLine $ CLPragma xs

parseControlLine s' ((L s (PPIdent "define")):(L s2 (PPIdent ident)):(L s3 (PPSpecial PPSLParen)):xs) = pure $ ControlLine $ CLPragma xs
--parseControlLins' e ((L s (PPIdent "define")):(L s2 (PPPunctuator LParen)):xs) = pure $ ControlLine $ CLPragma xs

parseControlLine s' ((L s (PPIdent "define")):(L s2 (PPIdent ident)):xs) = pure $ ControlLine $ CLDefineObj ident xs

parseControlLine s' [L s (PPIdent ident)] = pure $ NonDirective ident
parseControlLine s' [] = pure $ ControlLine CLEmpty
parseControlLine s' xs = throwError $ "error parsing a control line: " ++ show s' ++ " : " ++ show xs
{-
Line :: { PPLine }
    : IfLine            { IfLine $1 }
    | ControlLine       { ControlLine $1 }
    | TextLine          { TextLine $1  }
    | '#' ident         { NonDirective $2 }


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
-}

data PPLine
    = IfLine PPIfLine
    | ControlLine PPControlLine
    | TextLine [Located PPToken]
    | NonDirective Identifier
    -- | PPSEnd
    deriving stock (Eq, Show)

data PPIfLine
    = ILIf [Located PPToken]
    | ILIfDef Identifier
    | ILIfNDef Identifier
    | ILElIf [Located PPToken]
    | ILElse
    | ILEndIf
    deriving stock (Eq, Show)


data PPControlLine
    = CLInclude [Located PPToken]
    | CLDefineObj Identifier [Located PPToken] 
    | CLDefineFunc Identifier [Identifier] Bool [Located PPToken]
    | CLUndef Identifier
    | CLLine [Located PPToken]
    | CLError [Located PPToken]
    | CLPragma [Located PPToken]
    | CLEmpty
    | CLParseError
    deriving stock (Eq, Show)


