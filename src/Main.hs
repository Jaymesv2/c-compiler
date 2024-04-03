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
  -- let x = "int main(int argc) { println(\"hello world\\n\"); }"
  -- let x = "int i;" -- works
  let x = "struct x {};"

  -- let x = "i32 test_fn() {}"

  let tokens = alexScanTokens x
  let tree = clike tokens
  print $ tree

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
