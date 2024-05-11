module Compiler.Parser.Tokens (Identifier, PPToken(..), PPSpecial (..), Punctuator (..), Keyword (..), Token (..), FloatingConstant (..), Size (..), FloatSize (..), NumRep (..), Sign (..), Constant (..)) where

import Data.Text qualified as T

type Identifier = T.Text

data PPToken 
  = PPHeaderName T.Text
  | PPIdent Identifier
  | PPNumber T.Text
  | PPCharConst T.Text
  | PPStringLiteral T.Text
  | PPPunctuator Punctuator
  | PPOther T.Text
  | PPNewline
  | PPSpecial PPSpecial
  | PPEOF
  deriving stock (Eq)


instance Show PPToken where
  show (PPHeaderName n) = T.unpack n
  show (PPIdent n) = T.unpack n
  show (PPNumber n) = T.unpack n
  show (PPCharConst c) = T.unpack c
  show (PPStringLiteral s) = "\"" ++ T.unpack s ++ "\""
  show (PPPunctuator p) = show p
  show (PPOther t) = T.unpack t
  show (PPSpecial s) = "special"
  show PPEOF = "EOF"




data PPSpecial
  = PPSLParen
  deriving stock (Eq, Show)

data Token
  = 
    Ident Identifier -- T.Text
  | TTypeName Identifier
  | TEnumConst Identifier
  | StringLiteral T.Text
  | Constant Constant
  | Punctuator Punctuator
  | Keyword Keyword
  | EOF
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
  = IntConst T.Text NumRep Sign Size
  | FloatConst FloatingConstant
  | EnumConst Identifier
  | CharConst T.Text
  deriving stock (Eq, Show)

-- page 55
data FloatingConstant
  = FracFloatingConstant
      { significand :: (Maybe T.Text, Maybe T.Text)
      , exponentialPart :: Maybe (Sign, T.Text)
      , suffix :: FloatSize
      }
  | DigitFloatingConstant
      { significandd :: T.Text
      , exponentialPart :: Maybe (Sign, T.Text)
      , suffix :: FloatSize
      }
  deriving stock (Eq, Show)

{-data Literal
  = LString T.Text
  | LChar T.Text
  | LNum T.Text
  | LFloat T.Text
  deriving stock (Eq, Show)-}

data Keyword = 
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
  deriving stock (Eq, Show)

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
  --project1.c
  | Stringize
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
  deriving stock Eq
instance Show Punctuator where
  show punct = 
    case punct of
      LBrace -> "{"
      RBrace -> "}"
      LParen -> "("
      RParen -> ")"
      LBrack -> "["
      RBrack -> "]"
      Semi -> ","
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