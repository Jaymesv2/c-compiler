module Compiler.SymbolTable (SymbolTable, SymbolType (..), empty, enterScope, exitScope, getIdentifier, isType) where

import Compiler.Parser

import Compiler.Types
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

{-
The symbol table contains all the avaliable symbols, types, structs/unions/enums, and struct field members
  -}

empty :: SymbolTable
empty = SymbolTable $ (SymbolTableScope{identifiers = M.empty, types = M.empty, structunionenum = M.empty, labels = M.empty}) :| []

enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable (sc :| lst)) = SymbolTable (sc :| (sc : lst))

exitScope :: SymbolTable -> Maybe SymbolTable
exitScope (SymbolTable (_ :| (h : t))) = Just $ SymbolTable (h :| t)
exitScope (SymbolTable (_ :| [])) = Nothing

{-
inscopeM :: (SymbolTable -> m a) -> SymbolTable -> m a
inscope :: (SymbolTable -> a) -> SymbolTable -> a
-}

data SymbolTable = SymbolTable
  { symbols :: NonEmpty SymbolTableScope
  }

data SymbolTableScope = SymbolTableScope
  { identifiers :: M.Map T.Text ()
  , types :: M.Map T.Text CType
  , structunionenum :: M.Map T.Text CType
  , labels :: M.Map T.Text ()
  -- , members :: M.Map T.Text CType
  }

getIdentifier :: Identifier -> SymbolTable -> Maybe ()
getIdentifier ident (SymbolTable (SymbolTableScope{identifiers = idents} :| _)) = M.lookup ident idents

isType :: Identifier -> SymbolTable -> Bool
isType ident (SymbolTable (SymbolTableScope{types = typs} :| _)) = isJust $ M.lookup ident typs

--
enterFunctionScope :: SymbolTable -> SymbolTable
enterFunctionScope = error ""

exitFunctionScope :: SymbolTable -> SymbolTable
exitFunctionScope = error ""