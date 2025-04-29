module Compiler.AST where

import Compiler.Parser.SrcLoc

{-
Translations Phases:
1. map source file multibyte characters to source character set. (introduce newlines for end-of-line indicators if necessary)
2. remove instances of a backslash (\) immediately followed by a new line, splicing source lines to form logical source lines.
3. decompose source file into preprocessing tokens (6) and sequences of whitespace chars (including comments), each comment is replace by a space char.
    nenempty sequences of whitespace chars other than newlines can be replaced by a space if wanted.
4. execute preprocessing directives, expand macros, apply _Pragma s, recursively include '#include's
5. each source char and escape sequence is converted to the execution character set.
6. adjacent string literals are concatenated
7. whitespace tokens are removed, preprocessing tokens become tokens, parsing and what not happens
8. link time :)
-}


-- page
