{
module Compiler.Parser.Lexer (alexScanTokens, Token (..), Literal (..), Identifier) where
import qualified Data.Text as T
import qualified Data.List as L

}

%wrapper "basic"

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
    break                   { stupid $ \(_,_,_,s) _ -> Break }
    case                    { stupid $ \(_,_,_,s) _ -> Case }
    while                   { stupid $ \(_,_,_,s) _ -> While}
    for                     { stupid $ \(_,_,_,s) _ -> For }
    else                    { stupid $ \(_,_,_,s) _ -> Else }
    goto                    { stupid $ \(_,_,_,s) _ -> Goto}
    if                      { stupid $ \(_,_,_,s) _ -> If }
    return                  { stupid $ \(_,_,_,s) _ -> Return }
    sizeof                  { stupid $ \(_,_,_,s) _ -> Sizeof}
    struct                  { stupid $ \(_,_,_,s) _ -> Struct}
    switch                  { stupid $ \(_,_,_,s) _ -> Switch }
    union                   { stupid $ \(_,_,_,s) _ -> Union }
    void                    { stupid $ \(_,_,_,s) _ -> Void }
    static                  { stupid $ \(_,_,_,s) _ -> Static }
    inline                  { stupid $ \(_,_,_,s) _ -> Inline }
    extern                  { stupid $ \(_,_,_,s) _ -> Extern }
    enum                    { stupid $ \(_,_,_,s) _ -> Enum }
    const                   { stupid $ \(_,_,_,s) _ -> Const }
    default                 { stupid $ \(_,_,_,s) _ -> Default }
    do                      { stupid $ \(_,_,_,s) _ -> Do }
    continue                { stupid $ \(_,_,_,s) _ -> Continue}
    char                    { stupid $ \(_,_,_,s) _ -> TChar}
    short                   { stupid $ \(_,_,_,s) _ -> TShort}
    int                     { stupid $ \(_,_,_,s) _ -> TInt}
    long                    { stupid $ \(_,_,_,s) _ -> TLong}
    float                   { stupid $ \(_,_,_,s) _ -> TFloat}
    double                  { stupid $ \(_,_,_,s) _ -> TDouble}
    signed                  { stupid $ \(_,_,_,s) _ -> TSigned}
    unsigned                { stupid $ \(_,_,_,s) _ -> TUnsigned}
    _Bool                   { stupid $ \(_,_,_,s) _ -> TuBool}
    _Complex                { stupid $ \(_,_,_,s) _ -> TuComplex}
    _Imaginary              { stupid $ \(_,_,_,s) _ -> TuImaginary}
    "{"                     { stupid $ \(_,_,_,s) _ -> LBrace }
    "}"                     { stupid $ \(_,_,_,s) _ -> RBrace }
    "("                     { stupid $ \(_,_,_,s) _ -> LParen }
    ")"                     { stupid $ \(_,_,_,s) _ -> RParen }
    "["                     { stupid $ \(_,_,_,s) _ -> LBrack }
    "]"                     { stupid $ \(_,_,_,s) _ -> RBrack }
    "->"                    { stupid $ \(_,_,_,s) _ -> Arrow  }
    "&"                     { stupid $ \(_,_,_,s) _ -> BitAnd }
    "|"                     { stupid $ \(_,_,_,s) _ -> BitOr   }
    "*"                     { stupid $ \(_,_,_,s) _ -> Times }
    "+"                     { stupid $ \(_,_,_,s) _ -> Plus }
    "-"                     { stupid $ \(_,_,_,s) _ -> Minus }
    "~"                     { stupid $ \(_,_,_,s) _ -> Compliment }
    "!"                     { stupid $ \(_,_,_,s) _ -> Not }
    "/"                     { stupid $ \(_,_,_,s) _ -> Divide }
    "%"                     { stupid $ \(_,_,_,s) _ -> Modulo }
    "<<"                    { stupid $ \(_,_,_,s) _ -> LShift } 
    ">>"                    { stupid $ \(_,_,_,s) _ -> RShift } 
    "<"                     { stupid $ \(_,_,_,s) _ -> Lt     }
    "<="                    { stupid $ \(_,_,_,s) _ -> Le     }
    ">"                     { stupid $ \(_,_,_,s) _ -> Gt     }
    ">="                    { stupid $ \(_,_,_,s) _ -> Ge     }
    "=="                    { stupid $ \(_,_,_,s) _ -> Eq     }
    "!="                    { stupid $ \(_,_,_,s) _ -> Neq    }
    "^"                     { stupid $ \(_,_,_,s) _ -> BitXor }
    "&&"                    { stupid $ \(_,_,_,s) _ -> LAnd   }
    "||"                    { stupid $ \(_,_,_,s) _ -> LOr    }
    ";"                     { stupid $ \(_,_,_,s) _ -> Semi   }
    "="                     { stupid $ \(_,_,_,s) _ -> Assign }
    ","                     { stupid $ \(_,_,_,s) _ -> Comma  }
    "."                     { stupid $ \(_,_,_,s) _ -> Dot    }
    ":"                     { stupid $ \(_,_,_,s) _ -> Colon  }
    $nondigit $identcont*   { stupid $ \(_,_,_,s) _ -> Ident s}
    $nzdigit $digit*        { stupid $ \(_,_,_,s) _ -> Lit (LNum s)}
    \" @schar \"            { stupid $ \(_,_,_,s) _ -> Lit (LString s)}
    \' $printable \'        { stupid $ \(_,_,_,s) _ -> Lit (LChar s)}
    $white+;

{

stupid :: ((Int,Int,Int,String) -> Int -> Token) -> (String -> Token)
stupid f = \s -> f (0,0,0,s) 0

--stupid :: (AlexInput -> Int -> Alex a) -> (AlexInput -> Int -> Alex a)
--stupid a = a


type Identifier = String

data Literal =
    LString String
  | LChar String
  | LNum String
  | LFloat String
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
alexEOF :: Alex Token
alexEOF = pure EOF
-}

--type AlexAction a = AlexInput -> Int -> Alex a
-- { ... }  :: AlexAction result

}
