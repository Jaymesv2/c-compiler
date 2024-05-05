{
module Compiler.Parser.Lexer (alexMonadScan, Token (..), Literal (..), Identifier) where
import qualified Data.Text as T
import qualified Data.List as L
-- import Compiler.Parser.Monad
}

%wrapper "monadUserState-strict-text"

-- %encoding "latin-1"

-- %wrapper "strict-text"

$digit = [0-9]    -- digits
$nzdigit = [1-9]
$nondigit = [_a-zA-Z]
$identcont = [ $digit $nondigit ]

@schar = $printable* 

-- todo: 
--    escape sequences in strings and chars
--    float literals
--    hex literals
--    octal literals
tokens :-
    break                   { \(_,_,_,s) _ -> pure $ Break }
    case                    { \(_,_,_,s) _ -> pure $ Case }
    while                   { \(_,_,_,s) _ -> pure $ While}
    for                     { \(_,_,_,s) _ -> pure $ For }
    else                    { \(_,_,_,s) _ -> pure $ Else }
    goto                    { \(_,_,_,s) _ -> pure $ Goto}
    if                      { \(_,_,_,s) _ -> pure $ If }
    return                  { \(_,_,_,s) _ -> pure $ Return }
    sizeof                  { \(_,_,_,s) _ -> pure $ Sizeof}
    struct                  { \(_,_,_,s) _ -> pure $ Struct}
    switch                  { \(_,_,_,s) _ -> pure $ Switch }
    union                   { \(_,_,_,s) _ -> pure $ Union }
    void                    { \(_,_,_,s) _ -> pure $ Void }
    static                  { \(_,_,_,s) _ -> pure $ Static }
    inline                  { \(_,_,_,s) _ -> pure $ Inline }
    extern                  { \(_,_,_,s) _ -> pure $ Extern }
    enum                    { \(_,_,_,s) _ -> pure $ Enum }
    const                   { \(_,_,_,s) _ -> pure $ Const }
    default                 { \(_,_,_,s) _ -> pure $ Default }
    do                      { \(_,_,_,s) _ -> pure $ Do }
    continue                { \(_,_,_,s) _ -> pure $ Continue}
    char                    { \(_,_,_,s) _ -> pure $ TChar}
    short                   { \(_,_,_,s) _ -> pure $ TShort}
    int                     { \(_,_,_,s) _ -> pure $ TInt}
    long                    { \(_,_,_,s) _ -> pure $ TLong}
    float                   { \(_,_,_,s) _ -> pure $ TFloat}
    double                  { \(_,_,_,s) _ -> pure $ TDouble}
    signed                  { \(_,_,_,s) _ -> pure $ TSigned}
    unsigned                { \(_,_,_,s) _ -> pure $ TUnsigned}
    _Bool                   { \(_,_,_,s) _ -> pure $ TuBool}
    _Complex                { \(_,_,_,s) _ -> pure $ TuComplex}
    _Imaginary              { \(_,_,_,s) _ -> pure $ TuImaginary}
    "{"                     { \(_,_,_,s) _ -> pure $ LBrace }
    "}"                     { \(_,_,_,s) _ -> pure $ RBrace }
    "("                     { \(_,_,_,s) _ -> pure $ LParen }
    ")"                     { \(_,_,_,s) _ -> pure $ RParen }
    "["                     { \(_,_,_,s) _ -> pure $ LBrack }
    "]"                     { \(_,_,_,s) _ -> pure $ RBrack }
    "->"                    { \(_,_,_,s) _ -> pure $ Arrow  }
    "&"                     { \(_,_,_,s) _ -> pure $ BitAnd }
    "|"                     { \(_,_,_,s) _ -> pure $ BitOr   }
    "*"                     { \(_,_,_,s) _ -> pure $ Times }
    "+"                     { \(_,_,_,s) _ -> pure $ Plus }
    "-"                     { \(_,_,_,s) _ -> pure $ Minus }
    "~"                     { \(_,_,_,s) _ -> pure $ Compliment }
    "!"                     { \(_,_,_,s) _ -> pure $ Not }
    "/"                     { \(_,_,_,s) _ -> pure $ Divide }
    "%"                     { \(_,_,_,s) _ -> pure $ Modulo }
    "<<"                    { \(_,_,_,s) _ -> pure $ LShift } 
    ">>"                    { \(_,_,_,s) _ -> pure $ RShift } 
    "<"                     { \(_,_,_,s) _ -> pure $ Lt     }
    "<="                    { \(_,_,_,s) _ -> pure $ Le     }
    ">"                     { \(_,_,_,s) _ -> pure $ Gt     }
    ">="                    { \(_,_,_,s) _ -> pure $ Ge     }
    "=="                    { \(_,_,_,s) _ -> pure $ Eq     }
    "!="                    { \(_,_,_,s) _ -> pure $ Neq    }
    "^"                     { \(_,_,_,s) _ -> pure $ BitXor }
    "&&"                    { \(_,_,_,s) _ -> pure $ LAnd   }
    "||"                    { \(_,_,_,s) _ -> pure $ LOr    }
    ";"                     { \(_,_,_,s) _ -> pure $ Semi   }
    "="                     { \(_,_,_,s) _ -> pure $ Assign }
    ","                     { \(_,_,_,s) _ -> pure $ Comma  }
    "."                     { \(_,_,_,s) _ -> pure $ Dot    }
    ":"                     { \(_,_,_,s) _ -> pure $ Colon  }
    $nondigit $identcont*   { \(_,_,_,s) _ -> pure $ Ident s}
    $nzdigit $digit*        { \(_,_,_,s) _ -> pure $ Lit (LNum s)}
    \" @schar \"            { \(_,_,_,s) _ -> pure $ Lit (LString s)}
    \' $printable \'        { \(_,_,_,s) _ -> pure $ Lit (LChar s)}
    $white+;
{

type Identifier = T.Text

data Literal =
    LString T.Text
  | LChar   T.Text
  | LNum    T.Text
  | LFloat  T.Text
  deriving (Eq, Show)

data Token = 
    -- keywords
      Ident Identifier --T.Text
    | Lit Literal

    -- keywords
    | Break
    | Case
    | While
    | For
    | Else
    | Goto
    | If
    | Return
    | Sizeof
    | Struct
    | Enum
    | Switch
    | Union
    | Void
    | Static
    | Inline
    | Extern
    | Default
    | Do
    | Continue

    -- operations
    | LBrace
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
    | Const

    -- 
    | BitAnd
    | BitOr
    | BitXor
    | Compliment
    | LShift
    | RShift

    -- arith
    | Times
    | Plus
    | Minus
    | Not
    | Divide
    | Modulo

    -- comparison
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Neq
    | LAnd
    | LOr
    | TChar
    | TShort
    | TInt
    | TLong
    | TFloat
    | TDouble
    | TSigned
    | TUnsigned
    | TuBool
    | TuComplex
    | TuImaginary
    | EOF
    deriving (Eq, Show)

{-
data AlexState = AlexState
  { alex_pos   :: !AlexPosn  -- position at current input location
  , alex_inp   :: String     -- the current input
  , alex_chr   :: !Char      -- the character before the input
  , alex_bytes :: [Byte]     -- rest of the bytes for the current char
  , alex_scd   :: !Int       -- the current startcode
  } 

newtype Alex a = Alex { unAlex :: AlexState
                               -> Either String (AlexState, a) }

-- instance Functor     Alex where ...
-- instance Applicative Alex where ...
-- instance Monad       Alex where ...

runAlex          :: String -> Alex a -> Either String a

type AlexInput =
  ( AlexPosn                 -- current position,
  , Char                     -- previous char
  , [Byte]                   -- rest of the bytes for the current char
  , String                   -- current input string
  )

alexGetInput     :: Alex AlexInput
alexSetInput     :: AlexInput -> Alex ()

alexError        :: String -> Alex a

alexGetStartCode :: Alex Int
alexSetStartCode :: Int -> Alex ()


--alexMonadScan :: Alex result
alexMonadScan :: Alex Token


--alexEOF :: Alex result
-}

--type AlexAction a = AlexInput -> Int -> Alex a
-- { ... }  :: AlexAction result
alexEOF :: Alex Token
alexEOF = pure EOF



data AlexUserState = AlexUserState ()

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState ()


}
