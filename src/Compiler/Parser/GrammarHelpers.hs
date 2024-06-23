module Compiler.Parser.GrammarHelpers  where

import Compiler.Parser.ParseTree
import Compiler.Types

import Data.Foldable

import Effectful
import Effectful.State.Static.Local

data DeclaratorParseError

qualToQualifiers :: TypeQualifier -> TypeQualifiers
qualToQualifiers TQConst =  constTypeQualifier
qualToQualifiers TQRestrict = restrictTypeQualifier
qualToQualifiers TQVolatile = volatileTypeQualifier

--foldQualfiers = foldMap qualToQualifiers

parseFunctionDeclarator :: [DeclarationSpecifiers i] -> Declarator i -> Maybe [Declaration i] -> (i, CType, [(i, CType)], Bool)
parseFunctionDeclarator = error ""


parseTopLevelDeclaration :: Declaration i -> [(i, CType, Initializer i)]
parseTopLevelDeclaration = error ""


-- TODO: implement warnings 
extractDeclSpecifiers :: [DeclarationSpecifiers i] -> Either DeclaratorParseError (Maybe StorageClassSpecifier, [TypeSpecifier i], TypeQualifiers, Maybe FunctionSpecifier)
extractDeclSpecifiers specs = 
        Right (,,,) 
            <*> atMostOne storSpcs 
            <*> pure typSpcs
            <*> interpretTypQuals typQuals
            <*> atMostOne funcSpecs

    where
        --atMostOne :: [a] -> Either String (Maybe a)
        atMostOne [] = Right Nothing
        atMostOne [x] = Right (Just x)
        atMostOne _ = Left (error "too many values provided")

        --exactlyOne :: [a] -> Either String a
        exactlyOne [x] = Right x
        exactlyOne _ = Left (error "not enough or too many values provided")


        --interpretTypeSpecs :: [TypeSpecifier i] -> Either DeclaratorParseError CType
        --interpretTypeSpecs _ = Left (error "")

        -- should report warning about 
        --interpretTypQuals :: [TypeQualifier] -> Either String TypeQualifiers
        interpretTypQuals = Right . foldMap qualToQualifiers

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
parseDeclaration :: Declaration i -> Either DeclaratorParseError [(i, CType, Maybe (Initializer i))]
parseDeclaration (Declaration specifiers initDecls) = do
        (storageClass, typeSpcs, quals, funcSpec) <- extractDeclSpecifiers specifiers
        tspc <- parseTypeSpecifiers typeSpcs
        {- case storageClass of
            SCTypeDef -> error ""
            _ -> error "" -}
        mapM (parseInitDeclaration tspc) initDecls
    where
        parseInitDeclaration :: CType -> InitDeclaration i -> Either DeclaratorParseError (i, CType, Maybe (Initializer i))
        parseInitDeclaration ty (InitDeclaration decl init') = 
            ( \(i, c) -> (i, c, init') ) <$> parseInnerDeclarator decl ty  

parseTypeSpecifiers :: [TypeSpecifier i] -> Either DeclaratorParseError CType
parseTypeSpecifiers spc = Left (error "unimplemented")

parseInnerDeclarator :: Declarator i -> CType -> Either DeclaratorParseError (i, CType)
parseInnerDeclarator (DDIdent i) ty = Right (i, ty)
parseInnerDeclarator (DDPointer quals inner) ty = case parseInnerDeclarator inner ty of
        Right (i, ty') -> Right (i, PointerTy (foldMap qualToQualifiers quals) ty')
        Left e -> Left e


parseInnerDeclarator (DDArr inner isStatic quals size isUnsized) ty = case parseInnerDeclarator inner ty of
    Right (i, ty') -> Right (i, ArrayTy ty' (error "havent added const exprs yet"))
    Left e -> Left e

parseInnerDeclarator (DDFuncPList inner' params) ty = error ""

-- identifier lists aren't allowed
parseInnerDeclarator (DDFuncIList inner params) ty = Left (error "Identifier lists are not permitted in inner declarators")







parseAbstractDeclarator :: AbstractDeclarator i -> Either () (CType -> CType)
-- ADPtr [TypeQualifier] (Maybe (AbstractDeclarator i))
parseAbstractDeclarator (ADPtr quals (Just inner)) = error ""
parseAbstractDeclarator (ADPtr quals Nothing) = \c -> PointerTy (foldMap _ quals)
-- Array (Maybe (AbstractDeclarator i)) (Maybe (Expr i))
parseAbstractDeclarator (Array (Just inner) size) =  error ""
parseAbstractDeclarator (Array Nothing size) =  error ""
-- VarArray (Maybe (AbstractDeclarator i))
parseAbstractDeclarator (VarArray inner) = error ""
-- Parens (Maybe (AbstractDeclarator i)) [ParameterType i]
parseAbstractDeclarator (Parens inner params) = error ""


