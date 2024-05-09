module Main where

import Compiler.Parser.Grammar
import Compiler.Parser.Lexer

import Data.Text.IO qualified as TIO
import System.Environment

-- import System.IO.Error

import Effectful

main :: IO ()
main =  do
  args <- getArgs
  source <- case args of
    [] -> error "Cannot find a program to parse"
    h : _ -> TIO.readFile h -- will throw exception if the file doesn't exist
  --x <- runEff $ runAlex source clike
  --let source = "//int func(int i) { int i; }"
  -- print source
  runEff $ runAlex source printTokens
  pure ()
  --print x
 --where
  -- let x =
  --      "\
  --      \ i32 main(i32 x) { \
  --      \   string s = \"bob\"; \
  --      \   char c = 'b'; \
  --      \   i32 y = 99; \
  --      \   return ( y - x + 5 ); \
  --      \ } \
  --      \ "
  -- let x = "int i;" -- works
  -- let x = "struct x {};"

  {-
  let plist = "int i"
  let tokens = alexScanTokens plist
  let tree = parameterList tokens
  -}
  {-
  let decl = "i()"
  let tokens = alexScanTokens decl
  let tree = directDeclarator tokens
  -}
  -- let x = "int main(int argc, char** argv) { println(\"hello world\\n\"); }"
  -- let x = "typedef struct j { int i; long q; } j_t; int main(int argc, char **argv) { println(\"hello world\");}"
  -- let tokens = alexScanTokens x
  -- let tree = clike tokens

  --t1 = do
    --print "hi"

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
