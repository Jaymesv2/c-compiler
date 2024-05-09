module Compiler.Parser.Tokens (Identifier, Token (..), FloatingConstant (..), Size (..), FloatSize (..), NumRep (..), Sign (..), Constant (..)) where

import Data.Text qualified as T

type Identifier = T.Text

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

data Token
  = -- keywords
    Ident Identifier -- T.Text
  | TTypeName Identifier
  | TEnumConst Identifier
  | StringLiteral T.Text
  | Constant Constant
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
  | TStatic
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
  | Stringize
  | TokenPaste
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
