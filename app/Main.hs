
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Compiler.Parser.Grammar
import Compiler.Parser.Lexer
import Compiler.Parser.Preprocessor
import Compiler.SymbolTable 

import Data.Text.IO qualified as TIO
import System.Environment

import Data.Kind
import GHC.TypeLits


-- import System.IO.Error

import Effectful
import Effectful.State.Static.Local

import Conduit

main :: IO ()
main = do
  args <- getArgs
  source <- case args of
    [] -> error "Cannot find a program to parse"
    h : _ -> TIO.readFile h -- will throw exception if the file doesn't exist
    -- let source = "int func(int i) { int i; }"
    -- _ <- runEff $ runAlex source printTokens
    -- _ <- runEff $ runAlex source $ runPreprocessor printPPTokens
    --
  --runEff (alexPrintCondTokens source)
  --res <- runEff (evalState empty (runAlex source $ runPreprocessor $ runConduit (preprocess .| injectTypeNameTokens .| sinkList)))
  res <- runEff (evalState empty (runAlex source $ runPreprocessor $ runConduit (preprocess .| injectTypeNameTokens .| clike)))
  print res

  {-case res of
      Left (_, err) -> error $ "failed to parse with error: " ++ err
      Right tree -> print $ head tree-}

  pure ()

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8

--type Effec = (Type -> Type) -> Type -> Type

