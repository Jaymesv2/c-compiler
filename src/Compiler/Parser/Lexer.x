{
module Compiler.Parser.Lexer (alexScanTokens, Token (..), Literal (..), Identifier) where
import qualified Data.Text as T
import qualified Data.List as L
}

-- %encoding "latin-1"

-- %wrapper "strict-text"
%wrapper "basic"

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
    $white+;
    $nondigit $identcont*   { \s -> Ident s}
    $nzdigit $digit*        { \s -> Lit (LNum s)}
    \" @schar \"            { \s -> Lit (LString s)}
    \' $printable \'        { \s -> Lit (LChar s)}
    break   { \s -> Break }
    case    { \s -> Case }
    while   { \s -> While}
    for     { \s -> For }
    else    { \s -> Else }
    goto    { \s -> Goto}
    if      { \s -> If }
    return  { \s -> Return }
    sizeof  { \s -> Sizeof}
    struct  { \s -> Struct}
    switch  { \s -> Switch }
    union   { \s -> Union }
    void    { \s -> Void }
    static  { \s -> Static }
    inline  { \s -> Inline }
    extern  { \s -> Extern }
    enum    { \s -> Enum }
    const   { \s -> Const }
    default { \s -> Default }
    do      { \s -> Do }
    continue{ \s -> Continue}
    char    { \s -> TChar}
    short   { \s -> TShort}
    int     { \s -> TInt}
    long    { \s -> TLong}
    float   { \s -> TFloat}
    double  { \s -> TDouble}
    signed  { \s -> TSigned}
    unsigned{ \s -> TUnsigned}
    _Bool   { \s -> TuBool}
    _Complex{ \s -> TuComplex}
    _Imaginary{ \s -> TuImaginary}
    "{"     { \s -> LBrace }
    "}"     { \s -> RBrace }
    "("     { \s -> LParen }
    ")"     { \s -> RParen }
    "["     { \s -> LBrack }
    "]"     { \s -> RBrack }
    "->"    { \s -> Arrow  }
    "&"     { \s -> BitAnd }
    "|"     { \s -> BitOr   }
    "*"     { \s -> Times }
    "+"     { \s -> Plus }
    "-"     { \s -> Minus }
    "~"     { \s -> Compliment }
    "!"     { \s -> Not }
    "/"     { \s -> Divide }
    "%"     { \s -> Modulo }
    "<<"    { \s -> LShift } 
    ">>"    { \s -> RShift } 
    "<"     { \s -> Lt     }
    "<="    { \s -> Le     }
    ">"     { \s -> Gt     }
    ">="    { \s -> Ge     }
    "=="    { \s -> Eq     }
    "!="    { \s -> Neq    }
    "^"     { \s -> BitXor }
    "&&"    { \s -> LAnd   }
    "||"    { \s -> LOr    }
    ";"     { \s -> Semi   }
    "="     { \s -> Assign }
    ","     { \s -> Comma  }
    "."     { \s -> Dot    }
    ":"     { \s -> Colon  }

{

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
    

    deriving (Eq, Show)

}
