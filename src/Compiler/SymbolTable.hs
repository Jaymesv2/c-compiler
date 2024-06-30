module Compiler.SymbolTable  where

import Compiler.Parser

import Compiler.Types
import Data.List.NonEmpty ( NonEmpty(..), (<|) )
--import Data.List.NonEmpty qualified as N
import Data.List (uncons)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local

{-

-- data SymbolType
--   = Type
--   | Struct
--   | Union
--   | Enum
--   | Label
--   | Identifier


{-
The symbol table contains all the avaliable symbols, types, structs/unions/enums, and struct field members
  -}

--empty :: SymbolTable
--empty = SymbolTable $ (SymbolTableScope{identifiers_ = M.empty, typ = M.empty, structunionenum = M.empty, labels = M.empty}) :| []

-- inScopeE :: (State SymbolTable :> es) => Eff es a -> Eff es (S.Set OrdinaryID, a)
-- inScopeE m = do
--     oldState <- get
--     modify enterScope
--     a <- m
--     state <- get
--     ()
--
--     put oldState
--     
--
--     pure $ error ""

enterScope :: SymbolTable -> SymbolTable
enterScope s@SymbolTable{scope= sc :| lst } = s{scope=sc :| (sc : lst)}

-- inScope :: (SymbolTable -> (SymbolTable, a)) -> SymbolTable -> (SymbolTable, S.Set OrdinaryID, a)
-- inScope f s@SymbolTable{scope=sc:|lst} = error ""

exitScope :: SymbolTable -> Maybe SymbolTable
exitScope s@SymbolTable{scope=(h:|t)} = (\(_,t') -> s{scope=h:|t'}) <$> uncons t


defineIdent :: Identifier -> OrdinaryDef -> Eff es OrdinaryID
defineIdent ident def = error ""



-- exitScope s@SymbolTable{scope=(_ :| (h : t))} = Just $ s{scope= h :| t }
-- exitScope _ = Nothing

{-
inscopeM :: (SymbolTable -> m a) -> SymbolTable -> m a
inscope :: (SymbolTable -> a) -> SymbolTable -> a
-}

data SymbolTable = SymbolTable
  { scope :: NonEmpty (M.Map T.Text (OrdinaryID, OrdinaryDef), S.Set OrdinaryID)
    -- map from unique ids to their values.
  , identifiers :: M.Map OrdinaryID OrdinaryDef
  -- , types :: M.Map TypeID CType
  --, tags :: M.Map TagID ()
  -- , labels :: M.Map LabelID ()
  }

empty :: SymbolTable
empty = SymbolTable { scope = (M.empty, S.empty):|[],  identifiers = M.empty }

data OrdinaryDef 
    = VariableDef 
        { name :: T.Text
        , varType :: CType
        --, storeageClass :: StorageClass
        }
    | TypeAlias 
        { name :: T.Text
        , ty :: CType
        }
    {- | EnumConst 
        { name :: T.Text
        , enumName :: TagID
        , value :: Int
        }
    | FunctionDef 
        { name :: T.Text
        , inline :: Bool
        , currentDefinition :: Bool
        } -}

-- enterFunctionScope :: (State SymbolTable :> es) => Eff es ()
-- enterFunctionScope = error ""


-- getIdentifier :: Identifier -> SymbolTable -> Maybe ()
-- getIdentifier ident (SymbolTable (SymbolTableScope{identifiers = idents} :| _)) = M.lookup ident idents

isType :: Identifier -> SymbolTable -> Bool
isType ident (SymbolTable {scope = ((map,_):|_)}) = case M.lookup ident map of
        Just (_, TypeAlias _ _) -> True
        _ -> False
    


-- SymbolTableScope{types = typs} :| _)) = isJust $ M.lookup ident typs





-- enterFunctionScope :: SymbolTable -> SymbolTable
-- enterFunctionScope = error ""
-- exitFunctionScope :: SymbolTable -> SymbolTable
-- exitFunctionScope = error ""
-}
