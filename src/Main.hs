module Main where

import Compiler.Parser.Grammar
import Compiler.Parser.Lexer
import Compiler.Parser.Preprocessor

import Data.Text.IO qualified as TIO
import System.Environment

-- import System.IO.Error

import Effectful

main :: IO ()
main = do
  args <- getArgs
  source <- case args of
    [] -> error "Cannot find a program to parse"
    h : _ -> TIO.readFile h -- will throw exception if the file doesn't exist
    -- let source = "int func(int i) { int i; }"
    -- _ <- runEff $ runAlex source printTokens
    -- _ <- runEff $ runAlex source $ runPreprocessor printPPTokens
  runEff (runAlex source $ runPreprocessor clike) >>= print

  pure ()

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8