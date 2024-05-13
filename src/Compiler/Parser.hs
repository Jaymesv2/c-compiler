module Compiler.Parser (SymbolTable) where

-- import qualified Data.List as L
import Data.Map qualified as M
import Data.Text qualified as T

type SymbolTable = [M.Map T.Text ()]