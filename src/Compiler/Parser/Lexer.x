{
module Compiler.Parser.Lexer (alexMonadScan, runAlex, alexEOF, Alex, AlexUserState(..)) where
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Functor

import Compiler.AST 
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
    auto                    { \(_,_,_,_) _ -> pure $ TypeDef }
    break                   { \(_,_,_,_) _ -> pure $ Break }
    case                    { \(_,_,_,_) _ -> pure $ Case }
    const                   { \(_,_,_,_) _ -> pure $ Const }
    continue                { \(_,_,_,_) _ -> pure $ Continue}
    default                 { \(_,_,_,_) _ -> pure $ Default }
    do                      { \(_,_,_,_) _ -> pure $ Do }
    else                    { \(_,_,_,_) _ -> pure $ Else }
    enum                    { \(_,_,_,_) _ -> pure $ Enum }
    extern                  { \(_,_,_,_) _ -> pure $ Extern }
    for                     { \(_,_,_,_) _ -> pure $ For }
    goto                    { \(_,_,_,_) _ -> pure $ Goto}
    if                      { \(_,_,_,_) _ -> pure $ If }
    inline                  { \(_,_,_,_) _ -> pure $ Inline }
    register                { \(_,_,_,_) _ -> pure $ Register }
    restrict                { \(_,_,_,_) _ -> pure $ Restrict }
    return                  { \(_,_,_,_) _ -> pure $ Return }
    static                  { \(_,_,_,_) _ -> pure $ Static }
    sizeof                  { \(_,_,_,_) _ -> pure $ Sizeof}
    struct                  { \(_,_,_,_) _ -> pure $ Struct}
    switch                  { \(_,_,_,_) _ -> pure $ Switch }
    typedef                 { \(_,_,_,_) _ -> pure $ TypeDef }
    union                   { \(_,_,_,_) _ -> pure $ Union }
    volatile                { \(_,_,_,_) _ -> pure $ Volatile }
    while                   { \(_,_,_,_) _ -> pure $ While}
    
    void                    { \(_,_,_,_) _ -> pure $ Void }
    char                    { \(_,_,_,_) _ -> pure $ TChar}
    short                   { \(_,_,_,_) _ -> pure $ TShort}
    int                     { \(_,_,_,_) _ -> pure $ TInt}
    long                    { \(_,_,_,_) _ -> pure $ TLong}
    float                   { \(_,_,_,_) _ -> pure $ TFloat}
    double                  { \(_,_,_,_) _ -> pure $ TDouble}
    signed                  { \(_,_,_,_) _ -> pure $ TSigned}
    unsigned                { \(_,_,_,_) _ -> pure $ TUnsigned}
    _Bool                   { \(_,_,_,_) _ -> pure $ TuBool}
    _Complex                { \(_,_,_,_) _ -> pure $ TuComplex}
    _Imaginary              { \(_,_,_,_) _ -> pure $ TuImaginary}
    "{"                     { \(_,_,_,_) _ -> pure $ LBrace }
    "}"                     { \(_,_,_,_) _ -> pure $ RBrace }
    "("                     { \(_,_,_,_) _ -> pure $ LParen }
    ")"                     { \(_,_,_,_) _ -> pure $ RParen }
    "["                     { \(_,_,_,_) _ -> pure $ LBrack }
    "]"                     { \(_,_,_,_) _ -> pure $ RBrack }
    "->"                    { \(_,_,_,_) _ -> pure $ Arrow  }
    "&"                     { \(_,_,_,_) _ -> pure $ BitAnd }
    "|"                     { \(_,_,_,_) _ -> pure $ BitOr   }
    "*"                     { \(_,_,_,_) _ -> pure $ Times }
    "+"                     { \(_,_,_,_) _ -> pure $ Plus }
    "-"                     { \(_,_,_,_) _ -> pure $ Minus }
    "~"                     { \(_,_,_,_) _ -> pure $ Compliment }
    "!"                     { \(_,_,_,_) _ -> pure $ Not }
    "/"                     { \(_,_,_,_) _ -> pure $ Divide }
    "%"                     { \(_,_,_,_) _ -> pure $ Modulo }
    "<<"                    { \(_,_,_,_) _ -> pure $ LShift } 
    ">>"                    { \(_,_,_,_) _ -> pure $ RShift } 
    "<"                     { \(_,_,_,_) _ -> pure $ Lt     }
    "<="                    { \(_,_,_,_) _ -> pure $ Le     }
    ">"                     { \(_,_,_,_) _ -> pure $ Gt     }
    ">="                    { \(_,_,_,_) _ -> pure $ Ge     }
    "=="                    { \(_,_,_,_) _ -> pure $ Eq     }
    "!="                    { \(_,_,_,_) _ -> pure $ Neq    }
    "^"                     { \(_,_,_,_) _ -> pure $ BitXor }
    "&&"                    { \(_,_,_,_) _ -> pure $ LAnd   }
    "||"                    { \(_,_,_,_) _ -> pure $ LOr    }
    ";"                     { \(_,_,_,_) _ -> pure $ Semi   }
    "="                     { \(_,_,_,_) _ -> pure $ Assign }
    ","                     { \(_,_,_,_) _ -> pure $ Comma  }
    "."                     { \(_,_,_,_) _ -> pure $ Dot    }
    ":"                     { \(_,_,_,_) _ -> pure $ Colon  }
    $nondigit $identcont*   { \(_,_,_,t) i -> alexGetUserState <&> (\(AlexUserState (symtbl:_)) -> if isJust (M.lookup t symtbl) then TTypeName (T.take i t) else Ident (T.take i t)) }
    $nzdigit $digit*        { \(_,_,_,t) i -> pure $ Lit (LNum (T.take i t))}
    \" @schar \"            { \(_,_,_,t) i -> pure $ Lit (LString (T.take i t))}
    \' $printable \'        { \(_,_,_,t) i -> pure $ Lit (LChar (T.take i t))}
    $white+;
{
alexEOF :: Alex Token
alexEOF = pure EOF

data AlexUserState = AlexUserState [M.Map T.Text ()]

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [M.empty]
}
