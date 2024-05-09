module Compiler.Parser (SymbolTable) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

type SymbolTable = [M.Map T.Text ()]