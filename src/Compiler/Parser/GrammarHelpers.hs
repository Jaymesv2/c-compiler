{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes, MultiWayIf #-}
module Compiler.Parser.GrammarHelpers  where

import Compiler.Parser.ParseTree
import Compiler.Types
import Compiler.SymbolTable

import Control.Lens hiding (para, (<&>))

import Data.Foldable
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Dynamic

import Data.Set qualified as Set 
import Data.Set (Set)
--import Data.Sequence qualified as Seq
--import Data.Sequence (Seq)
import Data.Map qualified as M
import Data.Map (Map)
--import Data.Text qualified as T
import Data.Text (Text)
import Data.Bifunctor
--import Data.List.NonEmpty ( NonEmpty(..), (<|) )
--import Data.List.NonEmpty qualified as N
import Data.List qualified as L
--import Data.List (uncons)
import Data.Maybe
import Data.Either

import Data.Functor.Foldable
import Data.Functor
import Control.Monad
--import Data.Coerce
import Compiler.Parser.SrcLoc
import Control.Applicative

-- import Compiler.Parser.Tokens (Identifier)
type Identifier = Text


data DeclaratorParseError

data VariableDef = VariableDef deriving stock (Eq, Show, Ord)
data FunctionDef = FunctionDef deriving stock (Eq, Show, Ord)

newtype FunctionID = MkFunction Int deriving stock (Eq, Show, Ord)




{-
    C has 4 namespaces.
    - labels (only in function scope)
    - tags 
    - members (scoped to )
    - ordinaries
-}



data Ordinaries
    = Variable VariableID
    | TypeAlias TypeID
    | FunctionID FunctionID 
        deriving stock (Eq,Show, Ord)
makeLenses ''Ordinaries

{-
A block scope contains a map of names to identifiers which should be mapped in ParserState.
It also contains `definedInScope` which is a set of the ordinaries defined in the current scope
-}
data BlockScope = BlockScope 
    { _allOrdinaries :: Map Text Ordinaries
    , _definedInScope :: Set Ordinaries
    } deriving stock (Eq, Show)
makeLenses ''BlockScope

emptyBlockScope :: BlockScope
emptyBlockScope = BlockScope M.empty Set.empty


{-
ParserState is what the name suggests.
ordinaryScope contains a BlockScope
-}
data ParserState 
    = ParserState 
        { 
          -- scope stuff  
          -- _ordinaryScope :: !(NonEmpty BlockScope)
          _currentScope :: BlockScope
        , _scopeStack  :: [BlockScope]
        -- , functionScope :: Maybe (Map Text LabelID)
        , _variableDefs :: !(Map VariableID (Text, VariableDef))
        , _functionDefs :: !(Map FunctionID (Text, FunctionDef))
        , _typeDefs :: !(Map TypeID (Text, CType))
        ,  _idGen :: {-# UNPACK #-} !Int
        -- , _variableIdGen :: !UniqueGen
        -- , _functionIdGen :: !UniqueGen
        -- , _typeIdGen :: !UniqueGen
        } deriving stock (Eq, Show)
makeLenses ''ParserState




enterScope :: ParserState -> ParserState
enterScope ps@ParserState{_currentScope=cs@BlockScope{_allOrdinaries,_definedInScope},_scopeStack} 
    = ps{_currentScope=BlockScope{_allOrdinaries,_definedInScope=Set.empty},_scopeStack=cs:_scopeStack}

exitScope :: ParserState -> Maybe (BlockScope, ParserState)
exitScope ps@ParserState{_currentScope,_scopeStack}
    = case _scopeStack of
        [] -> Nothing
        (x:xs) -> Just (_currentScope, ps{_currentScope=x,_scopeStack=xs})

enterScopeE :: (State ParserState :> es) => Eff es ()
enterScopeE = modify enterScope

exitScopeE :: (State ParserState :> es, Error String :> es) => Eff es BlockScope
exitScopeE = get >>= maybe (throwError "Cannot exit top level scope") (\(b,newP) -> pure newP $> b) . exitScope 


-- inScopeE :: (State ParserState :> es) => Eff es a -> Eff es (a, BlockScope)
-- inScopeE f = error ""

defineType :: State ParserState :> es => (Declarator Text -> ParserState  -> (CType, ParserState)) -> Text -> Declarator Text ->  Eff es CType
defineType f name decl = do
    state $ \parserState@ParserState{_typeDefs,_idGen} -> let
            newState = parserState{_idGen=_idGen+1,_typeDefs=M.insert (MkType _idGen) (name, newVal) _typeDefs}
            (newVal,finalState) = f decl newState
        in (newVal, finalState) 




-- newParserState :: ParserState
-- newParserState = ParserState
--     { ordinaryScope=emptyBlockScope:|[]
--     , variableDefs=M.empty
--     , functionDefs=M.empty
--     , typeDefs=M.empty
--     , variableIdGen=newUniqueGen
--     , functionIdGen=newUniqueGen
--     , typeIdGen=newUniqueGen
--     }


-- recMap :: m -> (i -> j -> m -> m) -> i -> x -> (m -> x -> j) -> m
-- recMap omap updateMap idx val f = map2
--     where
--         map2 = updateMap idx newVal omap
--         newVal = f map2 val



getVariableID :: Ordinaries -> Maybe VariableID
getVariableID (Variable v) = Just v
getVariableID _ = Nothing

getTypeAliasID :: Ordinaries -> Maybe TypeID
getTypeAliasID (TypeAlias v) = Just v
getTypeAliasID _ = Nothing

getFunctionID :: Ordinaries -> Maybe FunctionID
getFunctionID (FunctionID v) = Just v
getFunctionID _ = Nothing


--exitScope :: (State ParserState :> es) => Eff es (Set.)

isType :: (State ParserState :> es) => Text -> Eff es Bool
isType name = isJust . (getTypeAliasID <=< M.lookup name . _allOrdinaries . _currentScope) <$> get
    


nextId :: (State ParserState :> es) => Eff es Int
nextId = state (idGen <+~ 1)


defineIdentifier :: (State ParserState :> es, Error String :> es) => Text -> VariableDef -> Eff es VariableID
defineIdentifier name val = do
    -- ordinaries' :: NonEmpty BlockScope <- view ordinaryScope <$> get
    -- let j = ordinaries' ^? _head
    error ""

getCurrentScope :: (State ParserState :> es) => Eff es BlockScope
getCurrentScope = _currentScope <$> get


defineTypeAlias :: (State ParserState :> es, Error String :> es) => Text -> CType -> Eff es TypeID
defineTypeAlias name val = do
    -- ParserState{_ordinaryScope=BlockScope{_allOrdinaries, _definedInScope}:|t, _typeDefs} <- get


    -- case M.lookup name _allOrdinaries of 
    --     Just x -> when (Set.member x _definedInScope) (throwError "cannot redefine a variable in the same scope")
    --     Nothing -> pure ()


    newId <- nextId
    
    
    pure newId
    -- put s{
    --     _ordinaryScope=BlockScope{allOrdinaries=M.insert name (Variable newId) allOrdinaries, definedInScope=Set.insert (Variable newId) definedInScope}:|t, 
    --     _variableIdGen=variableIdGen', 
    --     _variableDefs=M.insert newId (name, val) _variableDefs
    -- }
    -- pure newId

    error ""






{--
-- name and value
defineIdentifier :: (State ParserState :> es, Error String :> es) => Text -> VariableDef -> Eff es VariableID
defineIdentifier name val = do
    s@ParserState{_ordinaryScope=BlockScope{allOrdinaries, definedInScope}:|t, _variableIdGen, _variableDefs} <- get

    case M.lookup name allOrdinaries of 
        Just x -> when (Set.member x definedInScope) (throwError "cannot redefine a variable in the same scope")
        Nothing -> pure ()

    let (newId, variableIdGen') = nextVariableID _variableIdGen
    put s{
        _ordinaryScope=BlockScope{allOrdinaries=M.insert name (Variable newId) allOrdinaries, definedInScope=Set.insert (Variable newId) definedInScope}:|t, 
        _variableIdGen=variableIdGen', 
        _variableDefs=M.insert newId (name, val) _variableDefs
    }
    pure newId

-- name and value
defineTypeAlias :: (State ParserState :> es, Error String :> es) => Text -> CType -> Eff es TypeID
defineTypeAlias name val = do
    s@ParserState{_ordinaryScope=BlockScope{allOrdinaries, definedInScope}:|t, _typeIdGen, _typeDefs} <- get

    case M.lookup name allOrdinaries of 
        Just x -> when (Set.member x definedInScope) (throwError "cannot redefine a variable in the same scope")
        Nothing -> pure ()

    let (newId, typeIdGen') = nextTypeID _typeIdGen

    put s{
        _ordinaryScope=BlockScope{allOrdinaries=M.insert name (TypeAlias newId) allOrdinaries, definedInScope=Set.insert (TypeAlias newId) definedInScope}:|t, 
        _typeIdGen=typeIdGen', 
        _typeDefs=M.insert newId (name, val) _typeDefs
    }

    pure newId
 -}









{- 
compileTranslationUnit :: [ExternDecl Identifier] -> Eff es ()
compileTranslationUnit = foldM compileExternDecl ()

compileExternDecl :: () -> ExternDecl Text -> Eff es ()
compileExternDecl _ (EFunctionDef definition) = error ""
compileExternDecl _ (EDecl declaration) = error ""

compileFunctionDefinition :: () -> FunctionDefinition Text -> Eff es ()
compileFunctionDefinition _ (FunctionDefinition specifiers declarator declarationList body) = error ""

compileEDeclaration :: () -> Declaration Text -> Eff es ()
compileEDeclaration _ (Declaration specifiers initDeclarations) = error "" 
-}









qualToQualifiers :: TypeQualifier -> TypeQualifiers
qualToQualifiers TQConst =  constTypeQualifier
qualToQualifiers TQRestrict = restrictTypeQualifier
qualToQualifiers TQVolatile = volatileTypeQualifier

foldQualifiers :: [TypeQualifier] -> TypeQualifiers
foldQualifiers = foldMap qualToQualifiers


-- parseTopLevelDeclaration :: Declaration i -> [(i, CType, Initializer i)]
-- parseTopLevelDeclaration (Declaration ) =error "" 




--collectDeclSpecifiers :: (Error String :> es) => [DeclarationSpecifiers i] -> Eff es ()
-- TODO: implement warnings 
extractDeclSpecifiers :: (Error String :> es) => [DeclarationSpecifiers i] -> Eff es (Maybe StorageClassSpecifier, [TypeSpecifier i], TypeQualifiers, Maybe FunctionSpecifier)
extractDeclSpecifiers specs = 
        (,,,) 
            <$> atMostOne storSpcs 
            <*> pure typSpcs
            <*> interpretTypQuals typQuals
            <*> atMostOne funcSpecs

    where
        --atMostOne :: [a] -> Either String (Maybe a)
        atMostOne [] = pure Nothing
        atMostOne [x] = pure (Just x)
        atMostOne _ = throwError "too many values provided"

        --interpretTypeSpecs :: [TypeSpecifier i] -> Either DeclaratorParseError CType
        --interpretTypeSpecs _ = Left (error "")

        -- should report warning about 
        --interpretTypQuals :: [TypeQualifier] -> Either String TypeQualifiers
        interpretTypQuals = pure . foldMap qualToQualifiers
        (storSpcs, typSpcs, typQuals, funcSpecs) = foldl' declSpecSplitter ([], [], [], []) specs

        declSpecSplitter (a,b,c,d) x = case x of
            DSStorageSpec storageClassSpec -> (storageClassSpec:a,b,c,d)
            DSTypeSpec typeSpec -> (a, typeSpec:b, c, d) 
            DSTypeQual typeQual -> (a,b,typeQual:c,d)
            DSFuncSpec funcSpec -> (a,b,c,funcSpec:d)


-- should probably make this tail recursive
-- splitDeclarationSpecifiers :: [Located (DeclarationSpecifiers i)] -> ([Located (SpecifierQualifier i)], [Located (Either StorageClassSpecifier FunctionSpecifier)])
-- splitDeclarationSpecifiers (L s (DSStorageSpec spec):xs) = second (L s (Left spec):) $ splitDeclarationSpecifiers xs
-- splitDeclarationSpecifiers (L s (DSTypeSpec spec):xs) = first (L s (Right spec):) $ splitDeclarationSpecifiers xs
-- splitDeclarationSpecifiers (L s (DSTypeQual qual):xs) = first (L s (Left qual):) $ splitDeclarationSpecifiers xs
-- splitDeclarationSpecifiers (L s (DSFuncSpec spec):xs) = second (L s (Right spec):) $ splitDeclarationSpecifiers xs
-- splitDeclarationSpecifiers [] = ([],[])
--



-- parseTypeSpecifiers :: (Error String :> es) => [Located (TypeSpecifier i)] -> Eff es CType
-- parseTypeSpecifiers spcs = do
--     case spcs of
--         [] -> error ""
--         [L _ (StructType s)] -> error ""
--         (L _ (StructType s):_) -> error ""
--         [L _ (EnumType s)] -> error ""
--         ( L _ (EnumType s):_:_) -> error ""
--         [L _ (IdentType s)] -> error ""
--         ( L _ (IdentType s):_) -> error ""
--         -- this should ONLY contain primitive types
--         xs -> error ""
--
--         --[L _ (IdentType s)] -> error ""
--     where
        

parseTypeSpecifiers :: (Error String :> es) => [TypeSpecifier i] -> Eff es CType
parseTypeSpecifiers spcs = do
    --unless (mods == (prims,0,0,0,0,0)) (throwError "modifiers are unimplemented")
    unless (null structs) (throwError "unimplemented")
    unless (null enums) (throwError "unimplemented")
    unless (null idents) (throwError "unimplemented")
    unless (signeds /= 0 && unsigneds /= 0) (throwError "cannot have both `signed` and `unsigned`")
    case prims of 
        [PVoid] -> pure $ PrimTy CVoid
        [PuBool] -> pure $ PrimTy CBool
        [PInt] -> pure $ PrimTy $ CInt True
        [PChar] -> pure $ PrimTy $ CChar True
        [PFloat] -> pure $ PrimTy $ CFloat False
        [PDouble] -> pure $ PrimTy $ CDouble False
        _ -> throwError "unimplemented"


    where   

        --(idents, structs, enums, mods@(prims,signeds, unsigneds, shorts, longs, complexs)) = L.foldl' partitionPrims ([], 0,0,0,0,0) <$> L.foldl' go ([],[],[],[]) spcs
        (idents, structs, enums, mods@(prims,signeds, unsigneds, shorts, longs, complexs)) = L.foldl' (error "") ([], 0,0,0,0,0) <$> L.foldl' go ([],[],[],[]) spcs

        go (a,b,c,d) (IdentType x)  = (x:a,b,c,d)
        go (a,b,c,d) (StructType x) = (a,x:b,c,d)
        go (a,b,c,d) (EnumType x)   = (a,b,x:c,d)
        go (a,b,c,d) (PrimType x)   = (a,b,c,x:d)

-- -- the primary type can always be determined by the first type
-- data CanonicalTypePrimary = CTPVoid | CTPInt | CTPFloat
-- data CanonicalType = CT {
--     hasLong :: [SrcLoc],
--     hasImaginary :: Maybe SrcLoc,
-- }
--                          signedness      ,  int
data CanonicalNumeric = CanonicalNumeric {
    cnSignedness :: Maybe (Bool, SrcSpan),
    cnInt :: Maybe SrcSpan, -- int
    cnChar :: Maybe SrcSpan, -- char
    cnShort :: Maybe SrcSpan, -- short
    cnLongs :: [SrcSpan],     -- long
    cnFloat :: Maybe SrcSpan, -- float
    cnDouble :: Maybe SrcSpan, -- double
    cnComplex :: Maybe SrcSpan, -- complex
    cnImaginary :: Maybe SrcSpan  -- imaginary
}

canonicalNumericToCType :: Error String :> es => CanonicalNumeric -> Eff es (Either (SrcLoc, String) CType)
canonicalNumericToCType CanonicalNumeric{cnSignedness,cnInt,cnChar,cnShort,cnLongs,cnFloat,cnDouble,cnComplex,cnImaginary} 
    | isJust cnDouble || isJust cnFloat = do
        -- when (isJust cnSignedness) $ throwError ""
        -- when (isJust cnInt) $ throwError ""
        -- when (isJust cnShort) $ throwError ""
        -- when (isJust cnChar) $ throwError ""
        -- when (length cnLongs >= 2) $ throwError "" 
        let complex = isJust cnComplex
        case (cnFloat, cnDouble, cnLongs) of
            (Just _floatSpan, Nothing, []) -> pure $ Right $ PrimTy $ CFloat complex
            (Nothing, Just _doubleSpan, []) -> pure $ Right $ PrimTy $ CDouble complex
            (Nothing, Just _doubleSpan, [_longSpan]) -> pure $ Right $ PrimTy $ CLongDouble complex
            _ -> error "invalid double type"
        -- otherwise it should be an int type
    | otherwise = do
        -- when (isJust cnFloat) $ throwError ""
        -- when (isJust cnDouble) $ throwError ""
        -- when (isJust cnImaginary) $ throwError ""
        -- when (isJust cnComplex) $ throwError ""
        -- when (length cnLongs >= 3) $ throwError "" 
        let sign = maybe True fst cnSignedness
        case (cnInt, cnChar,cnShort,cnLongs) of
            (Nothing, Just _charSpan, Nothing, []) -> pure $ Right $ PrimTy $ CChar sign
            (_, Nothing, Just _shortSpann, []) -> pure $ Right $ PrimTy $ CShortInt sign
            (Just _intSpan, Nothing, Nothing, []) -> pure $ Right $ PrimTy $ CInt sign
            (_, Nothing,Nothing,[_longSpan]) -> pure $ Right $ PrimTy $ CLongInt sign
            (_, Nothing,Nothing,[_longSpan1,_longSpan2]) -> pure $ Right $ PrimTy $ CLongLongInt sign
            _ -> error "invalid int type"

-- Void and Bool are the only non numeric types and cannot have any other specifiers.
-- writes warnings into the writer and 
typeOfPrimitives :: Error String :> es => [Located PrimitiveTypes] -> Eff es (Either (SrcLoc, String) CType)
typeOfPrimitives [] = error "zero primitive types"
typeOfPrimitives ps@(L s p:_) = case p of
        -- fail to parse is there are any other ps
        PVoid -> pure $ Right $ PrimTy CVoid 
        PuBool -> pure $ Right $ PrimTy CBool
        _ -> canonicalizeNumeric ps >>= canonicalNumericToCType

canonicalizeNumeric :: (Error String :> es) => [Located PrimitiveTypes] -> Eff es CanonicalNumeric
canonicalizeNumeric = foldlM canonicalizeNumeric' (CanonicalNumeric Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing)
--{cnSignedness=Nothing, cnInt=Nothing, cnChar=Nothing, cnShort=Nothing, cnLongs=[], cnFloat=Nothing, cnDouble=Nothing, cnComplex=Nothing, cnImaginary=Nothing}

canonicalizeNumeric' :: (Error String :> es) => CanonicalNumeric -> Located PrimitiveTypes -> Eff es CanonicalNumeric
canonicalizeNumeric' cn@CanonicalNumeric{cnSignedness,cnInt,cnChar,cnShort,cnLongs,cnFloat,cnDouble,cnComplex,cnImaginary} (L span tok) = let 
    repeatError val = case val of
        Nothing -> pure ()
        Just n -> throwError $ "cannot repeat `" ++  show tok ++ "` at " ++ show n ++ ", first `" ++ show tok ++ "` at " ++ show span ++ "."

    --isIntType :: Eff es ()
    notFloatType = do
        when (isJust cnFloat) $ throwError $ "unexpected `" ++ show tok ++ "`"
        when (isJust cnDouble) $ throwError ""
        when (isJust cnImaginary) $ throwError ""
        when (isJust cnComplex) $ throwError ""
        when (length cnLongs >= 2) $ throwError "" 

    notIntType = do
        when (isJust cnSignedness) $ throwError ""
        when (isJust cnInt) $ throwError ""
        when (isJust cnShort) $ throwError ""
        when (isJust cnChar) $ throwError ""
        when (length cnLongs >= 2) $ throwError "" 

    in case tok of
    PVoid -> throwError "cannot have `void` in numeric type"
    PuBool -> throwError "Cannot have `_Bool` in numeric type"

    PChar -> do
        repeatError cnChar
        notFloatType
        pure cn{cnChar=Just span}
    PInt -> do
        repeatError cnInt
        notFloatType
        pure cn{cnInt=Just span}
    PShort -> do
        repeatError cnShort
        notFloatType
        pure cn{cnShort=Just span}
    PLong -> 
        if  | isJust cnFloat -> throwError "`long` cannot be combined with `float`"
            | length cnLongs >= 2 -> throwError "cannot have more than three `long`s"
            | otherwise -> pure $ cn{cnLongs=span:cnLongs}
    PSigned -> do
        notFloatType
        case cnSignedness of
            Nothing -> pure cn{cnSignedness=Just (True,span)}
            Just (True, _) -> pure cn -- emit warning here
            Just (False, _) -> throwError ""
    PUnsigned -> do
        notFloatType
        case cnSignedness of
            Nothing -> pure cn{cnSignedness=Just (False,span)}
            Just (False, _) -> pure cn -- emit warning here
            Just (True, _) -> throwError ""
    PFloat -> do
        repeatError cnFloat
        notIntType
        pure cn{cnFloat=Just span}
    PDouble -> do
        repeatError cnDouble
        notIntType
        pure cn{cnDouble=Just span}
    PuComplex -> do 
        repeatError cnComplex
        notIntType
        pure cn{cnComplex=Just span}
    PuImaginary -> do
        repeatError cnImaginary
        notIntType
        pure cn{cnImaginary=Just span}

{-
    void

    char
    signed char
    unsigned char

    short, signed short, short int, or signed short int
    unsigned short, or unsigned short int
    int, signed, or signed int
    unsigned, or unsigned int
    long, signed long, long int, or signed long int
    unsigned long, or unsigned long int
    long long, signed long long, long long int, or signed long long int
    unsigned long long, or unsigned long long int


    float
    float _Complex

    double
    long double
    double _Complex
    long double _Complex

    _Bool

    struct or union specifier *
    enum specifier
    typedef name
-}

parseTypeName :: (Error String :> es) => TypeName i -> Eff es CType
parseTypeName (TypeName specquals absdecl) = do
    let (quals', specs') = partitionEithers specquals
        quals = foldQualifiers quals'
    typ <- parseTypeSpecifiers specs'

    case absdecl of
        Just x -> do
            q <- para absDeclAlg x
            pure (q typ)
        Nothing -> pure typ

-- parseFunctionDefinition :: [DeclarationSpecifiers i] -> Declarator i -> Maybe [Declaration i] -> Eff es (i, CType, [(i, CType)], Bool)
parseFunctionDefinition :: FunctionDefinition i -> Eff es (i, CType, [(i, CType)], Bool)
parseFunctionDefinition (FunctionDefinition specifiers declarator arguments body) = error ""

parseDeclaration :: (Error String :> es, State ParserState :> es) => Declaration Identifier -> Eff es [(VariableID, CType, Maybe (Initializer i))]
parseDeclaration (Declaration specifiers initDecls) = do
        (storageClass, typeSpcs, quals, funcSpec) <- extractDeclSpecifiers specifiers

        --tspc <- parseTypeSpecifiers typeSpcs
        
        case storageClass of
            Just SCTypedef -> mapM_ (\(InitDeclaration decl init') -> do
                    when (isJust init') (throwError "")
                    (i, typf) <- parseDeclarator decl
                    tid <- defineTypeAlias i (error "")
                    pure ()
                ) initDecls $> []
            _ -> do
                mapM (\(InitDeclaration decl init') -> do
                        (i',c) <- parseDeclarator decl
                        vid <- defineIdentifier i' (error "")
                        pure (vid, c, init') 
                        --pure (oid, c tspc, init') 
                    ) initDecls $> []





parseDeclarator :: (Error String :> es, State ParserState :> es) => Declarator i -> Eff es (i, CType -> CType)
parseDeclarator = cata declAlg

declAlg :: (State ParserState :> es, Error String :> es) => DeclaratorF i (Eff es (i, CType -> CType)) -> Eff es (i, CType -> CType)
declAlg (DDIdentF i) = pure (i, id)
declAlg (DDPointerF quals inner) = inner <&> fmap (\ic -> PointerTy (foldQualifiers quals) . ic )
declAlg (DDArrF inner isStatic quals size _) = throwError "havent implemented arr declarators yet"
-- inner <&> \(i,ic) -> (i,ic)
declAlg (DDFuncPListF inner params) = do
    (i,ic) <- inner
    params' <- parseParamDecls params
    let params'' = map snd params'
    pure (i, \c ->  FuncTy (CFunc (ic c) params'' False)) 

declAlg (DDFuncIListF _ _) =  throwError "cannot have an identifier list in a declarator"


parseParamDecls :: (State ParserState :> es, Error String :> es) => [ParameterDeclaration i] -> Eff es [(Maybe i, CType)]
parseParamDecls = mapM parseParamDecl

parseParamDecl :: (State ParserState :> es, Error String :> es) => ParameterDeclaration i -> Eff es (Maybe i, CType)
parseParamDecl (ParameterDeclaration specquals decl) = do
    -- storage class is ignored if the params aren't in a function
    (_, typeSpcs, quals, funcSpec) <- extractDeclSpecifiers specquals
    tspc <- parseTypeSpecifiers typeSpcs
    (i,cfn) <- cata declAlg decl
    typ <- case cfn tspc of
        ArrayTy inner _ -> do
            pure $ PointerTy quals inner -- array parameters are adjusted to be pointers 
        x -> pure x
    
    pure (Just i, typ)
parseParamDecl (AbsParameterDeclaration specquals (Just absdecl)) = do 
    -- storage class is ignored if the params aren't in a function
    (_, typeSpcs, quals, funcSpec) <- extractDeclSpecifiers specquals
    tspc <- parseTypeSpecifiers typeSpcs

    cfn <- para absDeclAlg absdecl

    typ <- case cfn tspc of
        ArrayTy inner _ -> do
            pure $ PointerTy quals inner -- array parameters are adjusted to be pointers 
        x -> pure x
    
    pure (Nothing, typ)
parseParamDecl (AbsParameterDeclaration specquals Nothing) = do
    -- storage class is ignored if the params aren't in a function
    (_, typeSpcs, quals, funcSpec) <- extractDeclSpecifiers specquals
    tspc <- parseTypeSpecifiers typeSpcs
    pure (Nothing, tspc)
parseParamDecl VariadicDeclaration = error ""



absDeclAlg :: (Error String :> es) => AbstractDeclaratorF i (AbstractDeclarator i, Eff es (CType -> CType)) -> Eff es (CType -> CType)
absDeclAlg (ADPtrF quals Nothing) = pure $ PointerTy (foldQualifiers quals)
absDeclAlg (ADPtrF quals (Just (_, inner))) = inner <&> \ic -> PointerTy (foldQualifiers quals) . ic 
absDeclAlg (ArrayF _ _) = error ""
absDeclAlg (VarArrayF _) = error ""
absDeclAlg (ParensF _ _) = error ""

