{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes, MultiWayIf #-}
module Compiler.Parser.GrammarHelpers  where

import Compiler.Parser.ParseTree
import Compiler.Types
import Compiler.SymbolTable
import Compiler.Parser.Tokens (Constant (IntConst))

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
import qualified Data.Text as T
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
import Control.Lens hiding (para, (<|))
import qualified Data.List.NonEmpty as NE 
import Data.List.NonEmpty (NonEmpty((:|)))

-- import Compiler.Parser.Tokens (Identifier)
type Identifier = Text


data DeclaratorParseError





--runSymbolTable (EDecl e:xs) = error ""
{-
    C has 4 namespaces.
    - labels (only in function scope)
    - tags 
    - members (scoped to )
    - ordinaries
-}


newtype FunctionID = MkFunction Int deriving stock (Eq, Show, Ord)

newtype VariableID = MkVariable Int deriving stock (Eq, Show, Ord)
newtype TagID = MkData Int deriving stock (Eq, Show, Ord)
newtype TypeID = MkType Int deriving stock (Eq, Show, Ord)


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
    , _tags :: Map Text Int
    , _tagsInScope :: Set Int
    , _definedInScope :: Set Ordinaries
    } deriving stock (Eq, Show)
makeLenses ''BlockScope

emptyBlockScope :: BlockScope
emptyBlockScope = BlockScope M.empty M.empty Set.empty Set.empty


--newtype LabelID = MkLabel Int deriving stock (Eq, Ord)

-- data LabelScope 
--     = LabelScope 
--     { labels :: !(Map LabelID (Text, SrcSpan) )
--     }




data VariableScope
    = VariableScope {
        variableDefs' :: !(Map VariableID VariableDef)
    }

data VariableDef 
    = VariableDef 
    deriving stock (Eq, Show, Ord)

data FunctionDef 
    = FunctionDef {
        funcName :: Text,
        funcSpan :: SrcSpan,
        retType :: CType,
        arguments :: [CType],
        isVariadic :: CType,
        -- if a variable is referenced in the body but is not referenced here it should refer to the parent scope
        localVariables :: !(Map VariableID VariableDef)
    }
    | PrototypeDef Text CType [CType]
    deriving stock (Eq, Show)

data DataDef 
    -- name, fields
    = StructDef' Text SrcSpan CStruct 
    | EnumDef Text  SrcSpan [(Text, Int)]
    | UnionDef' Text SrcSpan CUnion
    | IncompleteStruct Text SrcSpan
    | IncompleteEnum Text SrcSpan
    | IncompleteUnion Text SrcSpan
    deriving stock (Eq, Show)


data SymbolTable = SymbolTable
    -- the block scope
    { _scope :: NonEmpty BlockScope
    -- maps a variable id to a name, source span, and definition
    , _variableDefs :: !(Map VariableID (Text, SrcSpan, VariableDef))
    , _functionDefs :: !(Map FunctionID (Text, SrcSpan, FunctionDef))
    -- 
    , _typeDefs :: !(Map TypeID (Text, SrcSpan, CType))
    -- tag scopes
    --
    , _dataDefs :: !(Map Int DataDef)
    --, _structDefs :: !(Map Int CStruct)
    --, _unionDefs :: !(Map Int CUnion)
    --, _enumDefs :: !(Map Int Int)
    --, _labelDefs :: !(Map Labe)
    , _idGen :: {-# UNPACK #-} !Int
    }
makeLenses ''SymbolTable



emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable {
        _scope=emptyBlockScope:|[],
        _variableDefs=M.empty,
        _functionDefs=M.empty,
        _typeDefs=M.empty,
        _dataDefs=M.empty,
        _idGen=0
    }


enterScope_ :: SymbolTable -> SymbolTable
enterScope_ s@SymbolTable{_scope=(h:|t)} = s{_scope=h:|h:t} 

enterScope :: State SymbolTable :> es => Eff es ()
enterScope = modify enterScope_

exitScope_ :: SymbolTable -> SymbolTable
exitScope_ st = case st^.scope of
    _:|[] -> error "cannot exit the top level scope"
    _:|x:y -> st & scope .~ x:|y

exitScope :: (State SymbolTable :> es) => Eff es ()
exitScope = modify exitScope_

nextID :: Int -> (Int,Int)
nextID i = (i,i+1)

defineIdentifier :: (State SymbolTable :> es, Error String :> es) => Text -> VariableDef -> Eff es VariableID
defineIdentifier name val = do
    s@SymbolTable{_scope=bs@BlockScope{_allOrdinaries, _definedInScope}:|t, _idGen,_variableDefs} <- get

    case M.lookup name _allOrdinaries of 
        Just x -> when (Set.member x _definedInScope) (throwError "cannot redefine a variable in the same scope")
        Nothing -> pure ()

    let (newId, idGen') = nextID _idGen
    put s{
        _scope=bs{_allOrdinaries=M.insert name (Variable (MkVariable newId)) _allOrdinaries, _definedInScope=Set.insert (Variable (MkVariable newId)) _definedInScope}:|t, 
        _idGen=idGen', 
        _variableDefs=M.insert (MkVariable newId) (name, error "", val) _variableDefs
    }
    pure (MkVariable newId)

defineTypeAlias :: (State SymbolTable :> es, Error String :> es) => Text -> CType -> Eff es VariableID
defineTypeAlias name val = do
    s@SymbolTable{_scope=bs@BlockScope{_allOrdinaries, _definedInScope}:|t, _idGen,_typeDefs} <- get
    let spn = error ""
    case M.lookup name _allOrdinaries of 
        Just x -> when (Set.member x _definedInScope) (throwError "cannot redefine a variable in the same scope")
        Nothing -> pure ()

    let (newId, idGen') = nextID _idGen
    put s{
        _scope=bs{_allOrdinaries=M.insert name (TypeAlias (MkType newId)) _allOrdinaries, _definedInScope=Set.insert (TypeAlias (MkType newId)) _definedInScope}:|t, 
        _idGen=idGen', 
        _typeDefs=M.insert (MkType newId) (name, spn, val) _typeDefs
    }
    pure (MkVariable newId)

getOrdinaryId :: State SymbolTable :> es => Text -> Eff es (Maybe Ordinaries)
getOrdinaryId name = M.lookup name . _allOrdinaries . NE.head . _scope <$> get 


-- partial function, will explode if an invalid type id is supplied
getTypeFromId :: State SymbolTable :> es => TypeID -> Eff es CType
getTypeFromId tid = do
    SymbolTable{_typeDefs} <- get
    let (_,_,ty) = fromJust $ M.lookup tid _typeDefs 
    pure ty


-- defines an incomplete struct.
-- Used when defining a struct like `struct i {} j;`
-- used when defining a struct prototype: `struct i`;
defineIncompleteStruct :: (State SymbolTable :> es, Error String :> es) => Text -> SrcSpan -> Eff es Int
defineIncompleteStruct name spn = do
    s@SymbolTable{_scope=bs@BlockScope{_tags, _tagsInScope}:|t, _idGen, _dataDefs} <- get
    case M.lookup name _tags of 
        Just tagID -> case (Set.member tagID _tagsInScope, M.lookup tagID _dataDefs) of
            (_,Nothing) -> error "Internal error: TagID does not have associated definition in data definitions table"
            (True, Just (StructDef' {} )) -> throwError "Cannot redefine type in the same scope"
            (True, Just (IncompleteStruct _ _)) -> do
                error ""
            (True, _) -> do
                throwError ""

            (False, _) -> do
                -- shadow previous definition
                put s{
                    _scope=bs{_tags=M.insert name _idGen _tags}:|t,
                    _dataDefs=M.insert _idGen (IncompleteStruct name spn) _dataDefs,
                    _idGen=_idGen+1
                }
                pure _idGen
        Nothing -> do
            put s{
                _scope=bs{_tags=M.insert name _idGen _tags}:|t,
                --_structDefs=M.insert _structDefs (IncompleteStruct ),
                _dataDefs=M.insert _idGen (IncompleteStruct name spn) _dataDefs,
                _idGen=_idGen+1
            }
            pure _idGen



-- happens if an incomplete struct is redefined in the same scope
replaceStructDefinition :: (State SymbolTable :> es, Error String :> es) => Int -> [(Text,TypeQualifiers,CType,Maybe Int)] ->  Eff es ()
replaceStructDefinition tid fields = do
    s@SymbolTable{_dataDefs} <- get
    (name,spn) <- case M.lookup tid _dataDefs of
        Nothing -> error "Internal error, struct id invalid"
        Just (IncompleteStruct name spn) -> pure (name,spn)
        Just (StructDef' name spn _) -> throwError ("cannot redefine struct `" ++ T.unpack name ++ "` originally defined at " ++ show spn)
        Just (IncompleteUnion name _span) -> throwError $ "cannot redefine union `" ++ T.unpack name ++ "` as a struct"
        Just (UnionDef' name _span _) -> throwError $ "cannot redefine union `" ++ T.unpack name ++ "` as a struct"
        Just (IncompleteEnum name _span) -> throwError $ "cannot redefine enum `" ++ T.unpack name ++ "` as a struct"
        Just (EnumDef name _span _) -> throwError $ "cannot redefine enum `" ++ T.unpack name ++ "` as a struct"

    put s{
        _dataDefs=M.insert tid (StructDef' name spn (CStruct fields)) _dataDefs
    }



defineFunction :: (State SymbolTable :> es, Error String :> es) => Text -> CType -> [CType] -> Bool ->  Eff es ()
defineFunction name ret args variadic = do
    error ""
    -- put s{
    --     _dataDefs=M.insert tid (StructDef' name spn (CStruct fields)) _dataDefs
    -- }


{- -- given a tagid 
defineStruct :: Text -> SrcSpan -> [(Text,TypeQualifiers,CType,Maybe Int)] ->  Eff es ()
defineStruct name fields = do
    s@SymbolTable{_scope=bs@BlockScope{_tags}:|t, _idGen, _dataDefs} <- get

    let _ = s{
        _scope=bs{_tags=M.insert name _idGen _tags}:|t,
        --_structDefs=M.insert _structDefs (IncompleteStruct ),
        _dataDefs=M.insert _idGen (IncompleteStruct name spn) _dataDefs,
        _idGen=_idGen+1
    }
    pure _idGen

    s@SymbolTable{_dataDefs} <- get
    case M.lookup name _allOrdinaries of 
        Just x -> when (Set.member x _definedInScope) (throwError "cannot redefine a variable in the same scope")
        Nothing -> pure ()
-}

{-
struct shadowing:

A struct is incomplete. 
-}



--getStructTagId :: State SymbolTable :> es => Text -> Eff es (Maybe TagID)
--getStructTagId 



qualToQualifiers :: TypeQualifier -> TypeQualifiers
qualToQualifiers TQConst =  constTypeQualifier
qualToQualifiers TQRestrict = restrictTypeQualifier
qualToQualifiers TQVolatile = volatileTypeQualifier

foldQualifiers :: [TypeQualifier] -> TypeQualifiers
foldQualifiers = foldMap qualToQualifiers

foldLocatedQualifiers :: [Located TypeQualifier] -> (TypeQualifiers, [(SrcSpan, String)])
foldLocatedQualifiers = foldl' go (emptyQualifier, [])
    where
        go :: (TypeQualifiers, [(SrcSpan, String)]) -> Located TypeQualifier -> (TypeQualifiers, [(SrcSpan, String)])
        go (quals, warnings) (L s TQConst) = if constq quals then (quals, (s, "Repeat `const` qualifier"):warnings) else (quals{constq=True}, warnings)
        go (quals, warnings) (L s TQRestrict) = if restrict quals then (quals, (s, "Repeat `restrict` qualifier"):warnings) else (quals{restrict=True}, warnings)
        go (quals, warnings) (L s TQVolatile) = if volatile quals then (quals, (s, "Repeat `volatile` qualifier"):warnings) else (quals{volatile=True}, warnings)

-- parseTopLevelDeclaration :: Declaration i -> [(i, CType, Initializer i)]
-- parseTopLevelDeclaration (Declaration ) =error "" 


-- Unpacking these might make sense but I'm not sure if that would make the size too large and make copying it too expensive
data CanonicalNumeric = CanonicalNumeric {
    _cnSignedness :: !(Maybe (Bool, SrcSpan)),
    _cnInt :: !(Maybe SrcSpan), -- int
    _cnChar :: !(Maybe SrcSpan), -- char
    _cnShort :: !(Maybe SrcSpan), -- short
    --_cnLongs :: ![SrcSpan],     -- long
    _cnLongs :: !(Maybe (Either SrcSpan (SrcSpan, SrcSpan))),     -- long
    _cnFloat :: !(Maybe SrcSpan), -- float
    _cnDouble :: !(Maybe SrcSpan), -- double
    _cnComplex :: !(Maybe SrcSpan), -- complex
    _cnImaginary :: !(Maybe SrcSpan)  -- imaginary
}

makeLenses ''CanonicalNumeric

--collectDeclSpecifiers :: (Error String :> es) => [DeclarationSpecifiers i] -> Eff es ()
-- TODO: implement warnings 
extractDeclSpecifiers :: (State SymbolTable :> es, Error String :> es) => [DeclarationSpecifier Text] -> Eff es (Maybe (Located StorageClassSpecifier), CType, TypeQualifiers, Maybe (Located FunctionSpecifier))
extractDeclSpecifiers specs = 
        (,,,) 
            <$> atMostOne storSpcs 
            <*> parseTypeSpecifiers typSpcs
            <*> interpretTypQuals typQuals
            <*> atMostOne funcSpecs
    where
        atMostOne [] = pure Nothing
        atMostOne [x] = pure (Just x)
        atMostOne _ = throwError "too many values provided"

        interpretTypQuals :: [Located TypeQualifier] -> Eff es TypeQualifiers
        interpretTypQuals = pure . foldMap (qualToQualifiers . getInner)

        (storSpcs, typSpcs, typQuals, funcSpecs) = (\(a,b,c,d) -> (reverse a, reverse b, reverse c, reverse d)) $ foldl' declSpecSplitter ([], [], [], []) specs
        declSpecSplitter (a,b,c,d) x = case x of
            DSStorageSpec s storageClassSpec -> (L s storageClassSpec:a,b,c,d)
            DSTypeSpec typeSpec -> (a, typeSpec:b, c, d) 
            DSTypeQual s typeQual -> (a,b,L s typeQual:c,d)
            DSFuncSpec s funcSpec -> (a,b,c,L s funcSpec:d)


parseStructDeclarations :: (State SymbolTable :> es, Error String :> es) => [StructDeclaration Text] -> Eff es [(Text, TypeQualifiers, CType, Maybe Int)] 
parseStructDeclarations = foldM (\f s -> (++f) <$> parseStructDeclaration s) [] 


--evaluateConstantExpr :: (State SymbolTable :> es, Error String :> es) => Expr i -> Eff es Constant
evaluateConstantExpr (EConstant _ c) = pure c
evaluateConstantExpr _expr = error ""

parseStructDeclaration :: (State SymbolTable :> es, Error String :> es) => StructDeclaration Text -> Eff es [(Text, TypeQualifiers, CType, Maybe Int)]
parseStructDeclaration (StructDeclaration spcs sdeclarators) = do
    let (quals', specs') = partitionEithers spcs
        (quals,_warns) = foldLocatedQualifiers quals'
    baseType <- parseTypeSpecifiers specs'

    forM sdeclarators $ \(StructDeclarator declarator sizeExpr) -> do
        (name, f) <- parseDeclarator declarator
        size <- mapM evaluateConstantExpr sizeExpr
        case size of 
            Just (IntConst i _ _ _) -> pure (name, quals, f baseType, Just $ fromIntegral i)
            Nothing -> pure (name, quals, f baseType, Nothing)
            _ -> error "only constant expressions which evaluate to integers can be used to specify bitfields"

parseDataLayoutSpec :: (State SymbolTable :> es, Error String :> es) => DataLayoutSpec Text -> Eff es CType
parseDataLayoutSpec (StructDef _ss (Just (L sn name)) fields) = do 
    -- define the struct name as an incomplete struct
    --tag <- defineIncompleteStructTag name
    tag <- defineIncompleteStruct name sn
    -- parse the field declarations
    decls <- parseStructDeclarations fields
    -- redefine the struct 
    --defineStruct tag decls
    replaceStructDefinition tag decls
    pure $ StructTy $ CStruct decls
parseDataLayoutSpec (StructDef _s Nothing fields) = do
    StructTy . CStruct  <$> parseStructDeclarations fields
parseDataLayoutSpec (StructRef s name) = do 
    --getType
    error ""
parseDataLayoutSpec (UnionDef s name fields) = do 
    error ""
parseDataLayoutSpec (UnionRef s name) = do 
    error ""


-- simple parse
parseTypeSpecifiers :: (State SymbolTable :> es, Error String :> es) => [TypeSpecifier Text] -> Eff es CType
parseTypeSpecifiers spcs = do
    case spcs of
        [] -> throwError "no type specifiers supplied"
        [IdentType (L _ name)] -> do
            getOrdinaryId name >>= \case
                Just (TypeAlias  tid) -> getTypeFromId tid
                Just (Variable   _vid) -> throwError "variable names are not allowed in type specifiers"
                Just (FunctionID _fid) -> throwError "function names are not allowed in type specifiers"
                Nothing -> throwError "Type is undefined"

        (IdentType (L _ _):_) -> error "identifier types cannot be followed by more type specifiers"
        [StructType spec] -> do
            parseDataLayoutSpec spec
        (StructType x:xs) -> error "struct types cannot be followed by more type specifiers"
        (EnumType x:xs) -> error "enums are unimplemented"
        (PrimType (L s PVoid):xs) -> pure $ PrimTy CVoid
        (PrimType (L s PuBool):xs) -> pure $ PrimTy CBool
        -- if its none of these then it must be a numeric type
        _ -> canonicalizeNumeric spcs >>= canonicalNumericToCType

        --(idents, structs, enums, mods@(prims,signeds, unsigneds, shorts, longs, complexs)) = L.foldl' partitionPrims ([], 0,0,0,0,0) <$> L.foldl' go ([],[],[],[]) spcs





canonicalNumericToCType :: Error String :> es => CanonicalNumeric -> Eff es CType
canonicalNumericToCType cn --CanonicalNumeric{cnSignedness,cnInt,cnChar,cnShort,cnLongs,cnFloat,cnDouble,cnComplex,cnImaginary} 
    | isJust (cn^.cnDouble) || isJust (cn^.cnFloat) = do
        let complex = isJust (cn^.cnComplex)
        PrimTy <$> case (cn^.cnFloat, cn^.cnDouble, cn^.cnLongs) of
            (Just _floatSpan, Nothing, Nothing) -> pure $ CFloat complex
            (Nothing, Just _doubleSpan, Nothing) -> pure $ CDouble complex
            (Nothing, Just _doubleSpan, Just (Left _longSpan)) -> pure $ CLongDouble complex
            _ -> error "invalid double type"
        -- otherwise it should be an int type
    | otherwise = do
        -- when (isJust cnFloat) $ throwError ""
        -- when (isJust cnDouble) $ throwError ""
        -- when (isJust cnImaginary) $ throwError ""
        -- when (isJust cnComplex) $ throwError ""
        -- when (length cnLongs >= 3) $ throwError "" 
        let sign = maybe True fst (cn^.cnSignedness)
        PrimTy  <$> case (cn^.cnInt, cn^.cnChar,cn^.cnShort,cn^.cnLongs) of
            (Nothing, Just _charSpan, Nothing, Nothing) -> pure $ CChar sign
            (_, Nothing, Just _shortSpann, Nothing) -> pure $  CShortInt sign
            (Just _intSpan, Nothing, Nothing, Nothing) -> pure $ CInt sign
            (_, Nothing,Nothing,Just (Left _longSpan)) -> pure $ CLongInt sign
            (_, Nothing,Nothing,Just (Right (_longSpan1,_longSpan2 ))) -> pure  $ CLongLongInt sign
            _ -> error "invalid int type"

canonicalizeNumeric :: (Error String :> es) => [TypeSpecifier i] -> Eff es CanonicalNumeric
canonicalizeNumeric = foldlM canonicalizeNumeric' (CanonicalNumeric Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

canonicalizeNumeric' :: (Error String :> es) => CanonicalNumeric -> TypeSpecifier i -> Eff es CanonicalNumeric
canonicalizeNumeric' cn tok = let 
    repeatError :: (Show a, Error String :> es) => Getting (Maybe a) b (Maybe a) -> b -> Eff es ()
    repeatError getter c = case c^.getter of
        Nothing -> pure ()
        Just n -> throwError $ "repeat type specifier not allowed at " ++ show n --throwError $ "cannot repeat  at " ++ show n ++ ", first `" ++ show tok ++ "` at " ++ show primSpan ++ "."

    notIntType :: Error String :> es => CanonicalNumeric -> Eff es ()
    notIntType c = do
        when (isJust (c^.cnSignedness)) $ throwError ""
        when (isJust (c^.cnInt)) $ throwError ""
        when (isJust (c^.cnShort)) $ throwError ""
        when (isJust (c^.cnChar)) $ throwError ""
        when (length (c^.cnLongs) >= 2) $ throwError "" 

    notFloatType :: Error String :> es => CanonicalNumeric -> Eff es ()
    notFloatType c = do
        when (isJust (c^.cnFloat)) $ throwError ""
        when (isJust (c^.cnDouble)) $ throwError ""
        when (isJust (c^.cnImaginary)) $ throwError ""
        when (isJust (c^.cnComplex)) $ throwError ""
        when (length (c^.cnLongs) >= 2) $ throwError "" 
    
    getConstraints :: Error String :> es => Located PrimitiveTypes -> Eff es CanonicalNumeric
    getConstraints (L primSpan x) = case x of
        PVoid -> throwError "cannot have `void` in numeric type"
        PuBool -> throwError "Cannot have `_Bool` in numeric type"

        PChar -> do
            repeatError cnChar cn
            notFloatType cn
            pure (cnChar ?~ primSpan $ cn)
        PInt -> do
            repeatError cnInt cn
            notFloatType cn
            pure (cnInt ?~ primSpan $ cn)
        PShort -> do
            repeatError cnShort cn
            notFloatType cn
            pure (cnShort ?~ primSpan $ cn)
        PLong -> do
            when (isJust (cn^.cnFloat)) $ throwError "`long` cannot be combined with `float`"
            case cn^.cnLongs of
                Nothing -> pure (cnLongs ?~ Left primSpan $ cn)
                Just (Left l) -> pure (cnLongs ?~ Right (l,primSpan) $ cn)
                Just (Right (_s1,_s2)) -> throwError "cannot have more than two `long` specifiers"
        PSigned -> do
            notFloatType cn
            case cn^.cnSignedness of
                Nothing -> pure (cnSignedness ?~ (True,primSpan) $ cn)
                Just (True, _) -> pure cn -- emit warning here
                Just (False, _) -> throwError ""
        PUnsigned -> do
            notFloatType cn
            case cn^.cnSignedness of
                Nothing -> pure (cnSignedness ?~ (False,primSpan) $ cn)
                Just (False, _) -> pure cn -- emit warning here
                Just (True, _) -> throwError ""
        PFloat -> do
            repeatError cnFloat cn
            notIntType cn
            pure (cnFloat ?~ primSpan $ cn)

        PDouble -> do
            repeatError cnDouble cn
            notIntType cn
            pure (cnDouble ?~ primSpan $ cn)
        PuComplex -> do
            repeatError cnComplex cn
            notIntType cn
            pure (cnComplex ?~ primSpan $ cn)
        PuImaginary -> do
            repeatError cnImaginary cn
            notIntType cn
            pure (cnImaginary ?~ primSpan $ cn)
    
    in case tok of 
        PrimType p -> getConstraints p
        i -> throwError "encountered non numeric type specifier while parsing a numeric type"

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

parseTypeName :: (State SymbolTable :> es, Error String :> es) => TypeName Text -> Eff es CType
parseTypeName (TypeName specquals absdecl) = do
    let (quals', specs') = partitionEithers specquals
        (quals,warns) = foldLocatedQualifiers quals'
    -- typ <- parseTypeSpecifiers specs'
    -- case absdecl of
    --     Just x -> ($ typ) <$> para absDeclAlg x
    --     Nothing -> pure typ
    typ <- parseTypeSpecifiers specs'
    case absdecl of
        Just x -> ($ typ) <$> para absDeclAlg x
        Nothing -> pure typ






{-
steps:
1. rename variables and resolve scope.
    - Function definitions will be collected into a function definition map which maps the functions ID to its name, type, metadata, and a compound statement (if its not a prototype)
    -

-}
defineExternDecl :: (State SymbolTable :> es, Error String :> es) => ExternDecl Text -> Eff es ()
defineExternDecl (EDecl decl) = error ""
defineExternDecl (EFunctionDef decl) = error ""


-- parseFunctionDefinition :: [DeclarationSpecifiers i] -> Declarator i -> Maybe [Declaration i] -> Eff es (i, CType, [(i, CType)], Bool)
parseFunctionDefinition :: (State SymbolTable :> es, Error String :> es) => FunctionDefinition Text -> Eff es (VariableID, CType, [(VariableID, CType)], Bool)
parseFunctionDefinition (FunctionDefinition specifiers declarator arguments body) = do
    (storageSpecs,baseType,qualifiers,funcSpecs) <- extractDeclSpecifiers specifiers

    when (isJust storageSpecs) $ throwError "functions cannot have storage specifiers"

    (_name, tyModif) <- parseDeclarator declarator
    let _returnType = tyModif baseType
    _args <- case arguments of
        Just args -> mapM parseDeclaration args
        Nothing -> pure []
    enterScope
    _res <- parseCompoundStatement body
    exitScope
    error ""

parseCompoundStatement :: (State SymbolTable :> es, Error String :> es) => CompoundStatement Text -> Eff es [Statement VariableID]
parseCompoundStatement (CompoundStatement stmts) = do
    foldM (\s -> fmap (++s) . parseBlockItem) [] stmts

{-
Should I delete declarations and lift them 
-}
parseBlockItem :: (State SymbolTable :> es, Error String :> es) => BlockItem Text -> Eff es [Statement VariableID]
parseBlockItem (BDecl decl) = do
    decls <- parseDeclaration decl
    pure $ error ""
parseBlockItem (BStmt stmt) = parseStatement stmt

parseStatement :: (State SymbolTable :> es, Error String :> es) => Statement Text -> Eff es [Statement VariableID]
parseStatement (CompoundStmt stmts) = pure []
parseStatement (ExpressionStmt expr) = pure []
parseStatement (IfStmt selector ifStmt elseStmt) = pure []
parseStatement s@(ReturnStmt _) = pure []

parseStatement (WhileStmt _cond _body) = do
    throwError "while statements aren't implemeted"
parseStatement (DoStmt _body _cond) = do
    throwError "do while statements aren't implemeted"
parseStatement (ForStmt _initi _cond _update _body) = do
    throwError "for statements aren't implemeted"
parseStatement (ForDeclStmt _initi _cond _update _body) = do
    throwError "for statements aren't implemeted"
parseStatement ContinueStmt = do
    throwError "continue statements aren't implemeted"
parseStatement BreakStmt = do
    throwError "break statements aren't implemeted"

parseStatement (LabeledStmt _lbl _stmt) = do
    throwError "goto statements are unimplemented and evil"
parseStatement (GotoStmt _lbl) = do
    throwError "goto statements are unimplemented and evil"

parseStatement (SwitchStmt _selector _body) = do
    throwError "switch statements aren't implemented"
parseStatement (CaseStmt _expr _stmt) = do
    throwError "switch statements aren't implemented"
parseStatement (DefaultStmt _stmt) = do
    throwError "switch statements aren't implemented"





parseDeclaration :: (Error String :> es, State SymbolTable :> es) => Declaration Identifier -> Eff es [(VariableID, CType, Maybe (Initializer Text))]
parseDeclaration (Declaration specifiers initDecls) = do
        (storageClass, baseTy, quals, funcSpec) <- extractDeclSpecifiers specifiers

        --tspc <- parseTypeSpecifiers typeSpcs
        case storageClass of
            Just (L _ SCTypedef) -> mapM_ (\(InitDeclaration decl init') -> do
                    when (isJust init') (throwError "cannot have initializers in a type definition")
                    (i, typf) <- parseDeclarator decl
                    tid <- defineTypeAlias i (typf baseTy)
                    pure ()
                ) initDecls $> []
            _ -> do
                mapM (\(InitDeclaration decl init') -> do
                        (i',typf) <- parseDeclarator decl
                        vid <- defineIdentifier i' VariableDef --(typf baseTy)
                        pure (vid, typf baseTy, init') 
                        --pure (oid, c tspc, init') 
                    ) initDecls


parseDeclarator :: (Error String :> es, State SymbolTable :> es) => Declarator i -> Eff es (i, CType -> CType)
parseDeclarator = cata declAlg

declAlg :: (State SymbolTable :> es, Error String :> es) => DeclaratorF i (Eff es (i, CType -> CType)) -> Eff es (i, CType -> CType)
declAlg (DDIdentF i) = pure (i, id)
declAlg (DDPointerF quals inner) = do
    i <- inner
    let (qs,_warns) = foldLocatedQualifiers quals
    pure $ (\ic -> PointerTy qs . ic) <$> i
declAlg (DDArrF inner isStatic quals size _) = throwError "havent implemented arr declarators yet"
-- inner <&> \(i,ic) -> (i,ic)
declAlg (DDFuncPListF inner params) = error "unimplemented" {-do
    (i,ic) <- inner
    params' <- parseParamDecls params
    let params'' = map snd params'
    pure (i, \c ->  FuncTy (CFunc (ic c) params'' False)) -}
declAlg (DDFuncIListF _ _) =  throwError "cannot have an identifier list in a declarator"


absDeclAlg :: (Error String :> es) => AbstractDeclaratorF i (AbstractDeclarator i, Eff es (CType -> CType)) -> Eff es (CType -> CType)
absDeclAlg (ADPtrF quals Nothing) = pure $ PointerTy (foldQualifiers $ error "") --quals)
absDeclAlg (ADPtrF quals (Just (_, inner))) = inner <&> \ic -> PointerTy (foldQualifiers $ error "") . ic--quals) . ic 
absDeclAlg (ArrayF _ _) = error ""
absDeclAlg (VarArrayF _) = error ""
absDeclAlg (ParensF _ _) = error ""



{-
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


 -}


runSymbolTable :: (IOE :> es) => Eff es (TranslationUnit Text) -> Eff es ()
runSymbolTable i = do
    ii <- i
    x <- runError $ runState emptySymbolTable $ runSymbolTable' ii
    case x of
        Left err -> liftIO $ print err
        Right (val,tbl) -> liftIO $ print val
            

runSymbolTable' :: (IOE :> es, Error String :> es, State SymbolTable :> es) => TranslationUnit Text -> Eff es ()
runSymbolTable' [] = pure ()
runSymbolTable' (EDecl d:xs) = do
    u <- parseDeclaration d
    liftIO $ print u
    runSymbolTable' xs
runSymbolTable' (EFunctionDef d:xs) = do
    u <- parseFunctionDefinition d
    liftIO $ print u
    runSymbolTable' xs
    error ""
