
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Compiler.Parser.Grammar
import Compiler.Parser.Lexer
import Compiler.Parser.Preprocessor
import Compiler.SymbolTable 

import Data.Text.IO qualified as TIO
import System.Environment



-- import System.IO.Error

import Effectful
import Effectful.State.Static.Local

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
  runEff (alexPrintCondTokens source)
  --res <- runEff (evalState empty (runAlex source $ runPreprocessor clike))
  --print res

  {-case res of
      Left (_, err) -> error $ "failed to parse with error: " ++ err
      Right tree -> print $ head tree-}

  pure ()

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
