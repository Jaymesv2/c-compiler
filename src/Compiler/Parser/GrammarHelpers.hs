module Compiler.Parser.GrammarHelpers  where

import Compiler.Parser.ParseTree
import Compiler.Types
import Compiler.SymbolTable

import Data.Foldable

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local


data DeclaratorParseError



qualToQualifiers :: TypeQualifier -> TypeQualifiers
qualToQualifiers TQConst =  constTypeQualifier
qualToQualifiers TQRestrict = restrictTypeQualifier
qualToQualifiers TQVolatile = volatileTypeQualifier

foldQualfiers :: [TypeQualifier] -> TypeQualifiers
foldQualfiers = foldMap qualToQualifiers

parseFunctionDefinition :: [DeclarationSpecifiers i] -> Declarator i -> Maybe [Declaration i] -> Eff es (i, CType, [(i, CType)], Bool)
parseFunctionDefinition = error ""


parseTopLevelDeclaration :: Declaration i -> [(i, CType, Initializer i)]
parseTopLevelDeclaration = error ""


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

        {- declSpecSplitter :: ([StorageClassSpecifier], [TypeSpecifier i], [TypeQualifier], [FunctionSpecifier])  
            -> DeclarationSpecifiers i 
            -> ([StorageClassSpecifier], [TypeSpecifier i], [TypeQualifier], [FunctionSpecifier]) -}
        declSpecSplitter (a,b,c,d) x = case x of
            DSStorageSpec storageClassSpec -> (storageClassSpec:a,b,c,d)
            DSTypeSpec typeSpec -> (a, typeSpec:b, c, d) 
            DSTypeQual typeQual -> (a,b,typeQual:c,d)
            DSFuncSpec funcSpec -> (a,b,c,funcSpec:d)


-- TODO
parseDeclaration :: (Error String :> es) => Declaration i -> Eff es [(i, CType, Maybe (Initializer i))]
parseDeclaration (Declaration specifiers initDecls) = do
        (storageClass, typeSpcs, quals, funcSpec) <- extractDeclSpecifiers specifiers
        tspc <- parseTypeSpecifiers typeSpcs
        {- 
         case storageClass of
            SCTypeDef -> error ""
            _ -> error "" 
        -}

        mapM (parseInitDeclaration tspc) initDecls
    where
        parseInitDeclaration :: (Error String :> es) => CType -> InitDeclaration i -> Eff es (i, CType, Maybe (Initializer i))
        parseInitDeclaration ty (InitDeclaration decl init') = 
            (\(i, c) -> (i, c, init')) <$> parseInnerDeclarator decl ty

parseTypeSpecifiers :: (Error String :> es) => [TypeSpecifier i] -> Eff es CType
parseTypeSpecifiers spc = pure (error "unimplemented")

--parseDataLayoutSpec :: DataLayoutSpec i -> Either DeclaratorParseError CType
--parseDataLayoutSpec d = case 




parseInnerDeclarator :: (Error String :> es) => Declarator i -> CType -> Eff es (i, CType)
parseInnerDeclarator (DDIdent i) ty = pure (i, ty)
parseInnerDeclarator (DDPointer quals inner) ty = parseInnerDeclarator inner ty >>= (\(i, ty') -> pure (i, PointerTy (foldMap qualToQualifiers quals) ty'))

parseInnerDeclarator (DDArr inner isStatic quals size isUnsized) ty = 
    parseInnerDeclarator inner ty >>= (\(i, ty') -> pure (i, ArrayTy ty' (error "havent added const exprs yet")))

parseInnerDeclarator (DDFuncPList inner' params) ty = error ""

-- identifier lists aren't allowed
parseInnerDeclarator (DDFuncIList inner params) ty =  (throwError "Identifier lists are not permitted in inner declarators")




parseAbstractDeclarator :: (Error String :> es) => AbstractDeclarator i -> CType -> Eff es CType
-- ADPtr [TypeQualifier] (Maybe (AbstractDeclarator i))
parseAbstractDeclarator (ADPtr quals inner) = \c -> PointerTy (foldMap qualToQualifiers quals) <$> maybe (pure c) (`parseAbstractDeclarator` c) inner
-- Array (Maybe (AbstractDeclarator i)) (Maybe (Expr i))
parseAbstractDeclarator (Array (Just inner) size) =  error ""
parseAbstractDeclarator (Array Nothing size) =  error ""
-- VarArray (Maybe (AbstractDeclarator i))
parseAbstractDeclarator (VarArray inner) = error ""
-- Parens (Maybe (AbstractDeclarator i)) [ParameterType i]
parseAbstractDeclarator (Parens inner params) = error ""


