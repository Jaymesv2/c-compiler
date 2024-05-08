module Compiler.Parser.Tokens (Identifier, Literal (..), Token (..)) where

import Data.Text qualified as T

type Identifier = T.Text

data Literal
    = LString T.Text
    | LChar T.Text
    | LNum T.Text
    | LFloat T.Text
    deriving stock (Eq, Show)

data Token
    = -- keywords
      Ident Identifier -- T.Text
    | TTypeName Identifier
    | Lit Literal
    | -- keywords
      Auto
    | Break
    | Case
    | Const
    | Continue
    | Default
    | Do
    | Else
    | Enum
    | Extern
    | For
    | Goto
    | If
    | Inline
    | Register
    | Restrict
    | Return
    | Sizeof
    | Static
    | Struct
    | Switch
    | TypeDef
    | Union
    | Volatile
    | While
    | -- operations
      LBrace
    | RBrace
    | LParen
    | RParen
    | LBrack
    | RBrack
    | Semi
    | Colon
    | Assign
    | Comma
    | Dot
    | Arrow
    | --
      BitAnd
    | BitOr
    | BitXor
    | Compliment
    | LShift
    | RShift
    | -- arith
      Times
    | Plus
    | Minus
    | Not
    | Divide
    | Modulo
    | -- comparison
      Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Neq
    | LAnd
    | LOr
    | -- primitive types
      Void
    | TChar
    | TShort
    | TInt
    | TLong
    | TFloat
    | TDouble
    | TuBool
    | -- type qualifiers
      TSigned
    | TUnsigned
    | TuComplex
    | TuImaginary
    | EOF
    deriving stock (Eq, Show)
