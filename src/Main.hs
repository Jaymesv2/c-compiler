module Main where

import Compiler.Parser.Grammar
import Compiler.Parser.Lexer

main :: IO ()
main = do
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

  --let x = "int func(int i) { int i; }"
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
  let x = "int main(int argc, char** argv) { println(\"hello world\\n\"); }"
  --let tokens = alexScanTokens x
  --let tree = clike tokens
  let tree =  runAlex x clike
  print $ tree
 where
  t1 = do
    print "hi"

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
