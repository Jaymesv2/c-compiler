module Compiler.SymbolTable (SymbolTable, SymbolType (..), empty, enterScope, exitScope, getIdentifier, isType) where

import Compiler.Parser

-- import Compiler.Parser.Tokens (Keyword (Enum))
import Data.List.NonEmpty
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T

data SymbolType
    = Type
    | Struct
    | Union
    | Enum
    | Label
    | Identifier

empty :: SymbolTable
empty = SymbolTable $ (SymbolTableScope{identifiers = M.empty, types = M.empty, structunionenum = M.empty, labels = M.empty}) :| []

enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable (sc :| lst)) = SymbolTable (sc :| (sc : lst))

exitScope :: SymbolTable -> Maybe SymbolTable
exitScope (SymbolTable (_ :| (h : t))) = Just $ SymbolTable (h :| t)
exitScope (SymbolTable (_ :| [])) = Nothing

newtype SymbolTable = SymbolTable (NonEmpty SymbolTableScope)

data SymbolTableScope = SymbolTableScope
    { identifiers :: M.Map T.Text ()
    , types :: M.Map T.Text ()
    , structunionenum :: M.Map T.Text ()
    , labels :: M.Map T.Text ()
    }

getIdentifier :: Identifier -> SymbolTable -> Maybe ()
getIdentifier ident (SymbolTable (SymbolTableScope{identifiers = idents} :| _)) = M.lookup ident idents

isType :: Identifier -> SymbolTable -> Bool
isType ident (SymbolTable (SymbolTableScope{types = typs} :| _)) = isJust $ M.lookup ident typs