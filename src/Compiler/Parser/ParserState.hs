-- {-# LANGUAGE MagicHash #-}

module Compiler.Parser.ParserState where

-- import Compiler.Parser (Unique (Uni)
import GHC.Exts

-- data ParseError = ParseError

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

data ParserState = ParserState
    { uniqueCounter :: Int
    }

newtype Unique = MkUnique Int

getUnique :: (State ParserState :> es) => Eff es Unique
getUnique = state (\p@ParserState{uniqueCounter = uctr} -> (MkUnique $ uctr + 1, p{uniqueCounter = uctr + 1}))
