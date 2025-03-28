module Compiler.Parser.Tokens (PPToken (..), PPSpecial (..), Punctuator (..), Keyword (..), Token (..), Size (..), FloatSize (..), NumRep (..), Sign (..), Constant (..)) where

import Compiler.Parser ( Identifier )
import Data.Text qualified as T

data PPToken
  = PPHeaderName T.Text
  | PPIdent Identifier
  | PPNumber T.Text
  | PPCharConst T.Text
  | PPStringLiteral T.Text
  | PPPunctuator Punctuator
  | PPOther T.Text
  | -- | PPNewline
    PPSpecial PPSpecial
  -- | PPEOF
  deriving stock (Eq)

data PPSpecial
  = PPSLParen
  | PPNewline
  deriving stock (Eq, Show)

data Token
  = Ident Identifier -- T.Text
  | TTypeName Identifier
  | TEnumConst Identifier
  | StringLiteral T.Text
  | Constant Constant
  | Punctuator Punctuator
  | Keyword Keyword
  -- | EOF
  deriving stock (Eq, Show)

-- operations

data Sign = Signed | Unsigned
  deriving stock (Eq, Show)

data Size = Standard | Long | LongLong
  deriving stock (Eq, Show)

data FloatSize = Float | Double | LongDouble
  deriving stock (Eq, Show)

data NumRep = Hex | Decimal | Octal
  deriving stock (Eq, Show)

data Constant
  = IntConst Integer NumRep Sign Size
  | FloatConst (Integer, Integer) (Maybe (Sign, Integer)) FloatSize
  | EnumConst Identifier
  | CharConst T.Text
  deriving stock (Eq, Show)

-- page 55

{-data Literal
  = LString T.Text
  | LChar T.Text
  | LNum T.Text
  | LFloat T.Text
  deriving stock (Eq, Show)-}

data Keyword
  = Auto
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
  | TStatic
  | Struct
  | Switch
  | TypeDef
  | Union
  | Volatile
  | While
  | Void
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
  deriving stock (Eq)

data Punctuator
  = LBrace
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
  | -- project1.c
    Stringize
  | TokenPaste
  | BitXor
  | BitOr
  | BitAnd
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
  | --
    PlusPlus
  | MinusMinus
  | Question
  | Variadic
  | TimesAssign
  | DivAssign
  | ModAssign
  | PlusAssign
  | MinusAssign
  | LShiftAssign
  | RShiftAssign
  | AndAssign
  | XorAssign
  | OrAssign
  | -- comparison
    Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | LAnd
  | LOr
  -- primitive types
  deriving stock (Eq)

instance Show Keyword where
  show kw =
    case kw of
      Auto -> "auto"
      Break -> "break"
      Case -> "case"
      Const -> "const"
      Continue -> "continue"
      Default -> "default"
      Do -> "do"
      Else -> "else"
      Enum -> "enum"
      Extern -> "extern"
      For -> "for"
      Goto -> "goto"
      If -> "if"
      Inline -> "inline"
      Register -> "register"
      Restrict -> "restrict"
      Return -> "return"
      Sizeof -> "sizeof"
      TStatic -> "tstatic"
      Struct -> "struct"
      Switch -> "switch"
      TypeDef -> "typedef"
      Union -> "union"
      Volatile -> "volatile"
      While -> "while"
      Void -> "void"
      TChar -> "tchar"
      TShort -> "tshort"
      TInt -> "int"
      TLong -> "long"
      TFloat -> "float"
      TDouble -> "double"
      TuBool -> "bool"
      TSigned -> "signed"
      TUnsigned -> "unsigned"
      TuComplex -> "_Complex"
      TuImaginary -> "_Imaginary"

instance Show Punctuator where
  show punct =
    case punct of
      LBrace -> "{"
      RBrace -> "}"
      LParen -> "("
      RParen -> ")"
      LBrack -> "["
      RBrack -> "]"
      Semi -> ";"
      Colon -> ":"
      Assign -> "="
      Comma -> ","
      Dot -> "."
      Arrow -> "->"
      Stringize -> "#"
      TokenPaste -> "##"
      BitXor -> "^"
      BitOr -> "|"
      BitAnd -> "&"
      Compliment -> "~"
      LShift -> ">>"
      RShift -> "<<"
      Times -> "*"
      Plus -> "+"
      Minus -> "-"
      Not -> "!"
      Divide -> "/"
      Modulo -> "%"
      PlusPlus -> "++"
      MinusMinus -> "--"
      Question -> "?"
      Variadic -> "..."
      TimesAssign -> "*="
      DivAssign -> "/="
      ModAssign -> "%="
      PlusAssign -> "+="
      MinusAssign -> "-="
      LShiftAssign -> "<<="
      RShiftAssign -> ">>="
      AndAssign -> "&="
      XorAssign -> "^="
      OrAssign -> "|="
      Lt -> "<"
      Le -> "<="
      Gt -> ">"
      Ge -> ">="
      Eq -> "=="
      Neq -> "!="
      LAnd -> "&&"
      LOr -> "||"

instance Show PPToken where
  show (PPHeaderName n) = T.unpack n
  show (PPIdent n) = T.unpack n
  show (PPNumber n) = T.unpack n
  show (PPCharConst c) = T.unpack c
  show (PPStringLiteral s) = "\"" ++ T.unpack s ++ "\""
  show (PPPunctuator p) = show p
  show (PPOther t) = T.unpack t
  show (PPSpecial PPSLParen) = "("
  show (PPSpecial PPNewline) = "NL"
  -- show PPEOF = "EOF"
