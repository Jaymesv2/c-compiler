module Compiler.Parser.GrammarHelpers () where

import Compiler.Parser.ParseTree
import Compiler.Types

data DeclaratorParseError




declAppendArray :: CType -> Either DeclaratorParseError CType -> Either DeclaratorParseError CType
declAppendArray startingType inner = inner >>= (\ty -> Right ty)

declAppendFunc :: CType -> Either DeclaratorParseError CType -> Either DeclaratorParseError CType
declAppendFunc staringType inner = inner >>= \ty -> Right ty


declAppendPointer :: CType -> Either DeclaratorParseError CType -> Either DeclaratorParseError CType
declAppendPointer staringType inner = inner >>= \ty -> Right ty


-- declIdent :: CType -> Either DeclaratorParseError CType
-- declIdent ty inner = 
