{
{-# LANGUAGE NoMonomorphismRestriction #-}
module Compiler.Parser.Grammar(clike, injectTypeNameTokens, ParserScope, newParserScope) where

import Compiler.Parser.Lexer (AlexState)
import Compiler.Parser.Preprocessor (preprocess, PreprocessorState)

import Compiler.Parser.ParseTree
import Compiler.Parser.Tokens
import Compiler.Parser (Identifier)
import Compiler.Parser.SrcLoc
--import Compiler.Parser.GrammarHelpers

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import Conduit
import Data.Conduit

import Data.Map qualified as M
import Data.Set qualified as S
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Control.Monad
}

%name clike TranslationUnit

%monad {(IOE :> es, State ParserScope :> es, Error String :> es, State AlexState :> es, State PreprocessorState :> es )}  {ConduitT (Located Token) Void (Eff es) } {>>=} {return}
%lexer {(await >>=)} {Nothing}

%errorhandlertype explist
%error {parseError}
%tokentype { Maybe (Located Token) }

%token
    -- ident   { Just (L _ (Ident $$)) }
    -- typeName { Just (L _ (TTypeName $$)) }
    -- stringlit { Just (L _ (StringLiteral $$)) }
    -- constant  { Just (L _ (Constant $$)) }

    ident   { Just (L _ (Ident _)) }
    typeName { Just (L _ (TTypeName _)) }
    -- stringlit { Just (L _ (StringLiteral _)) }
    stringlit  { Just (L _ (Constant (StringLiteral _))) }
    constant  { Just (L _ (Constant _)) }

    auto    { Just (L $$ (Keyword Auto))  }
    break   { Just (L $$ (Keyword Break)) }
    case    { Just (L $$ (Keyword Case)) }
    const   { Just (L $$ (Keyword Const)) }
    continue{ Just (L $$ (Keyword Continue)) }
    default { Just (L $$ (Keyword Default)) }
    do      { Just (L $$ (Keyword Do)) }
    else    { Just (L $$ (Keyword Else)) }
    extern  { Just (L $$ (Keyword Extern)) }
    enum    { Just (L $$ (Keyword Enum)) }
    for     { Just (L $$ (Keyword For)) }
    goto    { Just (L $$ (Keyword Goto)) }
    if      { Just (L $$ (Keyword If)) }
    inline  { Just (L $$ (Keyword Inline)) }
    register{ Just (L $$ (Keyword Register)) }
    restrict{ Just (L $$ (Keyword Restrict)) }
    return  { Just (L $$ (Keyword Return)) }
    sizeof  { Just (L $$ (Keyword Sizeof)) }
    static  { Just (L $$ (Keyword TStatic)) }
    struct  { Just (L $$ (Keyword Struct)) }
    switch  { Just (L $$ (Keyword Switch)) }
    typedef { Just (L $$ (Keyword TypeDef)) }
    union   { Just (L $$ (Keyword Union)) }
    volatile{ Just (L $$ (Keyword Volatile)) }
    while   { Just (L $$ (Keyword While)) }
    void    { Just (L $$ (Keyword Void)) }
    char    { Just (L $$ (Keyword TChar)) }
    short   { Just (L $$ (Keyword TShort)) }
    int     { Just (L $$ (Keyword TInt)) }
    long    { Just (L $$ (Keyword TLong)) }
    float   { Just (L $$ (Keyword TFloat)) }
    double  { Just (L $$ (Keyword TDouble)) }
    signed  { Just (L $$ (Keyword TSigned)) }
    unsigned{ Just (L $$ (Keyword TUnsigned)) }
    uBool   { Just (L $$ (Keyword TuBool)) }
    uComplex { Just (L $$ (Keyword TuComplex)) }
    uImaginary { Just (L $$ (Keyword TuImaginary)) }
    '{'     { Just (L $$ (Punctuator LBrace)) }
    '}'     { Just (L $$ (Punctuator RBrace)) }
    '('     { Just (L $$ (Punctuator LParen)) }
    ')'     { Just (L $$ (Punctuator RParen)) }
    '['     { Just (L $$ (Punctuator LBrack)) }
    ']'     { Just (L $$ (Punctuator RBrack)) }
    '->'    { Just (L $$ (Punctuator Arrow))  }
    '&'     { Just (L $$ (Punctuator BitAnd)) }
    '|'     { Just (L $$ (Punctuator BitOr))   }
    '*'     { Just (L $$ (Punctuator Times)) }
    '+'     { Just (L $$ (Punctuator Plus)) }
    '-'     { Just (L $$ (Punctuator Minus)) }
    '~'     { Just (L $$ (Punctuator Compliment)) }
    '!'     { Just (L $$ (Punctuator Not)) }
    '/'     { Just (L $$ (Punctuator Divide)) }
    '%'     { Just (L $$ (Punctuator Modulo)) }
    '<<'    { Just (L $$ (Punctuator LShift)) } 
    '>>'    { Just (L $$ (Punctuator RShift)) } 
    '<'     { Just (L $$ (Punctuator Lt))     }
    '<='    { Just (L $$ (Punctuator Le))     }
    '>'     { Just (L $$ (Punctuator Gt))     }
    '>='    { Just (L $$ (Punctuator Ge))     }
    '=='    { Just (L $$ (Punctuator Eq))     }
    '!='    { Just (L $$ (Punctuator Neq))    }
    '^'     { Just (L $$ (Punctuator BitXor)) }
    '&&'    { Just (L $$ (Punctuator LAnd))   }
    '||'    { Just (L $$ (Punctuator LOr))    }
    ';'     { Just (L $$ (Punctuator Semi))   }
    '='     { Just (L $$ (Punctuator Assign)) }
    ','     { Just (L $$ (Punctuator Comma))  }
    '.'     { Just (L $$ (Punctuator Dot)) }
    ':'     { Just (L $$ (Punctuator Colon))}

    '++'    { Just (L $$ (Punctuator PlusPlus)) }
    '--'    { Just (L $$ (Punctuator MinusMinus)) }
    '?'     { Just (L $$ (Punctuator Question))  }
    '...'   { Just (L $$ (Punctuator Variadic))  }
    '*='    { Just (L $$ (Punctuator TimesAssign)) }
    '/='    { Just (L $$ (Punctuator DivAssign)) }
    '%='    { Just (L $$ (Punctuator ModAssign)) }
    '+='    { Just (L $$ (Punctuator PlusAssign)) }
    '-='    { Just (L $$ (Punctuator MinusAssign)) }
    '<<='   { Just (L $$ (Punctuator LShiftAssign)) }
    '>>='   { Just (L $$ (Punctuator RShiftAssign)) }
    '&='    { Just (L $$ (Punctuator AndAssign)) }
    '^='    { Just (L $$ (Punctuator XorAssign)) }
    '|='    { Just (L $$ (Punctuator OrAssign)) }
    -- '#'     { Just (L $$ (Punctuator Stringize)) }
    -- '##'    { Just (L $$ (Punctuator TokenPaste)) }

{-
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '>' '<=' '>='
%right '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
-}

%%
PrimaryExpr :: { Expr Identifier }
    : ident                                 { EIdent (extractSpan $1) (extractIdentifier $1)     }
    | constant                              { EConstant (extractSpan $1) (extractConstant $1)    }
    | stringlit                             { EConstant (extractSpan $1) (extractConstant $1)      }
    | '(' Expr ')'                          { $2                }

    -- | stringlit                             { EStringLiteral $1 }

PostfixExpr  :: { Expr Identifier }
    : PrimaryExpr                           { $1                }
    | PostfixExpr '[' Expr ']'              { (Bracketed $1 $3) }

    -- prob a better way to do this 
    | PostfixExpr '(' ')'                   { Called $1 []              }
    | PostfixExpr '(' ArgExprList ')'       { Called $1 (reverse $3)    }

    | PostfixExpr '.' ident                 { (DotE $1 (extractIdentifier $3))          }
    | PostfixExpr '->' ident                { (ArrowE $1 (extractIdentifier $3))        }
    | PostfixExpr '++'                      { (UnaryE UPostIncr $1) }
    | PostfixExpr '--'                      { (UnaryE UPostDecr $1) }
    | '(' TypeName ')' '{' InitializerList '}'     { InitE $2 (reverse $5) }
    | '(' TypeName ')' '{' InitializerList ',' '}' { InitE $2 (reverse $5) }

ArgExprList :: { [Expr Identifier] }
    : AssignmentExpr                        { [ $1 ]    }
    | ArgExprList ',' AssignmentExpr        { $3 : $1   }

UnaryOp  :: { UnaryOp }
    : '&'                                   { URef        }
    | '*'                                   { UDeref      }
    | '~'                                   { UCompliment }
    | '!'                                   { UNot        }
    | '+'                                   { UPlus       }
    | '-'                                   { UMinus      }


UnaryExpr :: { Expr Identifier }
    : PostfixExpr                           { $1                    }
    | '++' UnaryExpr                        { UnaryE UPreIncr $2    }
    | '--' UnaryExpr                        { UnaryE UPreDecr $2    }
    | UnaryOp CastExpr                      { UnaryE $1 $2          }
    | sizeof UnaryExpr                      { UnaryE USizeof $2     }
    | sizeof '(' TypeName ')'               { SizeofTypeE $3        }

CastExpr :: { Expr Identifier }
    : UnaryExpr                             { $1            }
    | '(' TypeName ')' CastExpr             { CastE $2 $4   }

MultiplicativeExpr   :: { Expr Identifier }
    : CastExpr                              { $1                    } 
    | MultiplicativeExpr '*' CastExpr       { BinaryOp $1 BMul $3   }
    | MultiplicativeExpr '/' CastExpr       { BinaryOp $1 BDiv $3   }
    | MultiplicativeExpr '%' CastExpr       { BinaryOp $1 BMod $3   }

AdditiveExpr  :: { Expr Identifier }   
    : MultiplicativeExpr                    { $1                    }
    | AdditiveExpr '+' MultiplicativeExpr   { BinaryOp $1 BAdd $3   }
    | AdditiveExpr '-' MultiplicativeExpr   { BinaryOp $1 BSub $3   }

ShiftExpr    :: { Expr Identifier }
    : AdditiveExpr                          { $1                        }
    | ShiftExpr '<<' AdditiveExpr           { BinaryOp $1 BShiftL $3    }
    | ShiftExpr '>>' AdditiveExpr           { BinaryOp $1 BShiftR $3    }

RelationalExpr   :: { Expr Identifier }
    : ShiftExpr                             { $1                    }
    | RelationalExpr '<' ShiftExpr          { BinaryOp $1 Blt $3    }
    | RelationalExpr '>' ShiftExpr          { BinaryOp $1 Bgt $3    }
    | RelationalExpr '<=' ShiftExpr         { BinaryOp $1 Ble $3    }
    | RelationalExpr '>=' ShiftExpr         { BinaryOp $1 Bge $3    }

EqualityExpr :: { Expr Identifier }    
    : RelationalExpr                        { $1                    }
    | EqualityExpr '==' RelationalExpr      { BinaryOp $1 Beq $3    }
    | EqualityExpr '!=' RelationalExpr      { BinaryOp $1 Bneq $3   }

AndExpr  :: { Expr Identifier }
    : EqualityExpr                          { $1                    }
    | AndExpr '&' EqualityExpr              {BinaryOp $1 BBitAnd $3 }

XorExpr  :: { Expr Identifier }
    : AndExpr                               { $1                    }
    | XorExpr '^' AndExpr                   {BinaryOp $1 BBitXor $3 }

OrExpr   :: { Expr Identifier }
    : XorExpr                               { $1                    }
    | OrExpr '|' XorExpr                    {BinaryOp $1 BBitOr $3  }

-- Expr
LAndExpr     :: { Expr Identifier }
    : OrExpr                                { $1                            }
    | LAndExpr  '&&' OrExpr                 { BinaryOp $1 BLogicalAnd $3    }

-- Expr
LOrExpr      :: { Expr Identifier }
    : LAndExpr                              { $1                        }
    | LOrExpr '||' LAndExpr                 { BinaryOp $1 BLogicalOr $3 }

-- could add ternary operator here
ConditionalExpr :: { Expr Identifier }
    : LOrExpr                               { $1                        }
    | LOrExpr '?' Expr ':' ConditionalExpr  { ConditionalExpr $1 $3 $5  }


AssignmentOperator :: { AssignmentOp }
    : '*='  { ATimesAssign  }
    | '/='  { ADivAssign    }
    | '%='  { AModAssign    }
    | '+='  { APlusAssign   }
    | '-='  { AMinusAssign  }
    | '<<=' { ALShiftAssign }
    | '>>=' { ARShiftAssign }
    | '&='  { AAndAssign    }
    | '^='  { AXorAssign    }
    | '|='  { AOrAssign     }

-- Expr
AssignmentExpr :: { Expr Identifier }
    : ConditionalExpr                                   { $1                        }
    | AssignmentExpr '=' ConditionalExpr                { SimpleAssignE $1 $3       }
    | AssignmentExpr AssignmentOperator ConditionalExpr { CompoundAssignE $1 $2 $3  }

Expr :: { Expr Identifier }
    : AssignmentExpr            { $1            }
    | Expr ',' AssignmentExpr   { CommaE $1 $3  }

-- should be evaluated at translation time
-- no increment, decrement, function calls, comma ops, unless they are in an unevaluated subexpression
ConstExpr :: { Expr Identifier }
    : Expr   { $1 }

Declaration :: { Declaration Identifier }
    : DeclarationSpecifiers InitDeclarationList ';' { Declaration $1 (reverse $2) }
    | DeclarationSpecifiers ';'                     { Declaration $1 []  }

-- page 97

-- Declarations 

DeclarationSpecifiers :: { [DeclarationSpecifier Identifier] }
    : DeclarationSpecifiers DeclarationSpecifier    { $2 : $1 }
    | DeclarationSpecifier                          { [$1] }

DeclarationSpecifier :: { DeclarationSpecifier Identifier }
    : StorageClassSpecifier                         { DSStorageSpec (getSpan $1) (getInner $1) }
    | TypeSpecifier                                 { DSTypeSpec $1    }
    | TypeQualifier                                 { DSTypeQual (getSpan $1) (getInner $1 )    }
    -- function specifiers
    | inline                                        { DSFuncSpec $1 FSInline }
    -- | FunctionSpecifier                             { DSFuncSpec $1    }


-- page 112
--FunctionSpecifier :: { Located FunctionSpecifier }
--    : inline    { L $1 FSInline }

InitDeclarationList :: { [InitDeclaration Identifier] }
    : InitDeclarator                            { [ $1 ] } 
    | InitDeclarationList ',' InitDeclarator    { $3 : $1 }

InitDeclarator :: { InitDeclaration Identifier }
    : Declarator                    { InitDeclaration $1 Nothing }
    | Declarator '=' Initializer    { InitDeclaration $1 (Just $3) }


-- page 98
StorageClassSpecifier :: { Located StorageClassSpecifier  }
    : typedef   { L $1 SCTypedef     }
    | extern    { L $1 SCExtern      }
    | static    { L $1 SCStatic      }
    | auto      { L $1 SCAuto        }
    | register  { L $1 SCRegister    }

    -- this needs to have the standard types and stuff
-- page 99
TypeSpecifier :: { TypeSpecifier Identifier }
    : void                      { PrimType (L $1 PVoid)        }
    | char                      { PrimType (L $1 PChar)        }
    | short                     { PrimType (L $1 PShort)       }
    | int                       { PrimType (L $1 PInt)         }
    | long                      { PrimType (L $1 PLong)        }
    | float                     { PrimType (L $1 PFloat)       }
    | double                    { PrimType (L $1 PDouble)      }
    | signed                    { PrimType (L $1 PSigned)      }
    | unsigned                  { PrimType (L $1 PUnsigned)    }
    | uBool                     { PrimType (L $1 PuBool)       }
    | uComplex                  { PrimType (L $1 PuComplex)    }
    | uImaginary                { PrimType (L $1 PuImaginary)  }
    | StructOrUnionSpecifier    { StructType $1         }
    | EnumSpecifier             { EnumType $1           }
    | TypedefName               { IdentType (L (getSpan $1) (getInner $1))          }

-- page 108
TypeQualifier :: { Located TypeQualifier }
    : const         { L $1 TQConst       }
    | restrict      { L $1 TQRestrict    }
    | volatile      { L $1 TQVolatile    }


-- page 101
-- might merge these two rules into 1 with 6 rules
StructOrUnionSpecifier :: { DataLayoutSpec Identifier }
    : StructOrUnion ident '{' StructDeclarationList '}'
        { case $1 of 
            L s SUStruct -> StructDef s (Just (L (extractSpan $2) (extractIdentifier $2)) ) $4
            L s SUUnion -> UnionDef s (Just (L (extractSpan $2) (extractIdentifier $2))) $4 }
    | StructOrUnion  '{' StructDeclarationList '}' 
        { case $1 of 
            L s SUStruct -> StructDef s Nothing $3
            L s SUUnion -> UnionDef s Nothing $3 }
    | StructOrUnion ident
        { case $1 of
            L s SUStruct -> StructRef s (L (extractSpan $2) (extractIdentifier $2))
            L s SUUnion -> UnionRef s (L (extractSpan $2) (extractIdentifier $2)) }

StructOrUnion :: { Located StructOrUnion }
    : struct    { L $1 SUStruct }
    | union     { L $1 SUUnion  }


StructDeclarationList :: { [StructDeclaration Identifier] }
    : StructDeclaration                         { [ $1 ] }
    | StructDeclarationList StructDeclaration   { ($2 : $1) }

--StructDeclaration       : SpecifierQualifierList StructDeclarator { StructDeclaration $1 ([$2] }

StructDeclaration :: { StructDeclaration Identifier }
    : SpecifierQualifierList StructDeclaratorList ';' { StructDeclaration (reverse $1) (reverse $2) }

StructDeclaratorList :: { [StructDeclarator Identifier] }
    : StructDeclarator                              { [ $1 ] }
    | StructDeclaratorList ',' StructDeclarator   { ($3 : $1) }

--SpecifierQualifierList  : SpecifierQualifierListI { (reverse $1) }

SpecifierQualifierList :: { [SpecifierQualifier Identifier] }
    : TypeSpecifier SpecifierQualifierList  { (Right $1) : $2    }
    | TypeQualifier SpecifierQualifierList  { (Left $1) : $2   }
    | TypeSpecifier                         { [ Right $1 ]       }
    | TypeQualifier                         { [ Left  $1 ]      }

-- unfinished
StructDeclarator :: { StructDeclarator Identifier }
    : Declarator            { (StructDeclarator $1 Nothing) }
    | Declarator ':' Expr   { (StructDeclarator $1 (Just $3)) }
                        -- | ':' Expr   { StructDeclarator $1 (Just $3) }
                        -- I could support bitfields but I dont really want to :/

-- page 104
EnumSpecifier :: { EnumSpecifier Identifier }
    : enum ident '{' EnumeratorList '}'     { EnumSpecifier (Just (L (extractSpan $2) (extractIdentifier $2))) (reverse $4)  }
    | enum  '{' EnumeratorList '}'          { EnumSpecifier Nothing   (reverse $3)  }
    | enum ident '{' EnumeratorList ',' '}' { EnumSpecifier (Just (L (extractSpan $2 ) (extractIdentifier $2))) (reverse $4)  }
    | enum '{' EnumeratorList ',' '}'       { EnumSpecifier Nothing   (reverse $3)  }
    | enum ident                            { EnumRef (L (extractSpan $2) (extractIdentifier $2))                     }

                                    -- this needs to be a "constant" expression
-- little hack for the list building
--EnumeratorList  :   EnumeratorListI { reverse $1                }

EnumeratorList :: { [(Located Identifier, Maybe (Expr Identifier))] }
    : Enumerator                        { [ $1 ]    }
    | EnumeratorList ',' Enumerator    { $3 : $1   }

Enumerator :: { (Located Identifier, Maybe (Expr Identifier)) }
    : EnumerationConstant             { ($1, Nothing) }
    | EnumerationConstant '=' Expr    { ($1, Just $3) }
                                    -- this needs to be a "constant" expression

EnumerationConstant :: { Located Identifier }
    : ident { (L (extractSpan $1) (extractIdentifier $1)) :: Located Identifier }

-- Page 114

Declarator :: { Declarator Identifier }
    : Pointer DirectDeclarator      { ($1 DDPointer id) $2 }
    | DirectDeclarator              { $1 }


DirectDeclarator :: { Declarator Identifier }
    : ident                                                             { DDIdent (extractIdentifier $1)}
    | '(' Declarator ')'                                                { $2              }
    -- array declarations
    | DirectDeclarator '[' TypeQualifierList AssignmentExpr ']'         { DDArr $1 False $3 (Just $4) False }
    | DirectDeclarator '[' TypeQualifierList '*' ']'                    { DDArr $1 False $3 Nothing   True  }       
    | DirectDeclarator '[' TypeQualifierList ']'                        { DDArr $1 False $3 Nothing   False }
    | DirectDeclarator '[' static TypeQualifierList AssignmentExpr ']'  { DDArr $1 True  $4 (Just $5) False }
    | DirectDeclarator '[' static AssignmentExpr ']'                    { DDArr $1 True  [] (Just $4) False }
    | DirectDeclarator '[' TypeQualifierList static AssignmentExpr ']'  { DDArr $1 True  $3 (Just $5) False }
    | DirectDeclarator '[' AssignmentExpr ']'                           { DDArr $1 False [] (Just $3) False }
    | DirectDeclarator '[' '*' ']'                                      { DDArr $1 False [] Nothing   True  }     
    | DirectDeclarator '[' ']'                                          { DDArr $1 False [] Nothing   False }
    -- function declarations
    | DirectDeclarator '(' ParameterTypeList ')'                        { DDFuncPList   $1 $3   }     
    | DirectDeclarator '(' IdentifierList ')'                           { DDFuncIList $1 (reverse $3)     }    
    | DirectDeclarator '(' ')'                                          { DDFuncIList $1 [] }   

-- ([TypeQualifier] -> (Maybe (AbstractDeclarator i)) -> AbstractDeclarator i) -> Maybe (AbstractDeclarator i) -> AbstractDeclarator i

-- declarators
Pointer :: { forall a b. ([Located TypeQualifier] -> a -> b) -> (b -> a) -> a -> b }
    : '*'                           { \constr _ -> constr [] }
    | '*' TypeQualifierList         { \constr _ -> constr (reverse $2)    }
    -- | '*' TypeQualifierList Pointer { \constr (c :: a) d -> constr (reverse $2) (($3 constr) c)  }
    | '*' Pointer                   { \constr d c  -> constr [] (d ($2 constr d c)) }

TypeQualifierList :: {[Located TypeQualifier ]}
    : TypeQualifier                     { [ $1 ]  }
    | TypeQualifierList TypeQualifier  { $2 : $1   }

ParameterTypeList  :: { [ParameterDeclaration Identifier] }
    : ParameterList                 { reverse $1}
    | ParameterList ',' '...'       { reverse (VariadicDeclaration : $1) }

ParameterList :: { [ParameterDeclaration Identifier] }
    : ParameterDeclaration                      { [ $1 ]   }
    | ParameterList ',' ParameterDeclaration    { $3 : $1  }

ParameterDeclaration :: { ParameterDeclaration Identifier }
    : DeclarationSpecifiers Declarator          { ParameterDeclaration $1 $2 }  
    | DeclarationSpecifiers AbstractDeclarator  { AbsParameterDeclaration $1 (Just $2) }
    | DeclarationSpecifiers                     { AbsParameterDeclaration $1 Nothing }

IdentifierList :: { [Identifier] }
    : ident                     { [ extractIdentifier $1 ]  }
    | IdentifierList ',' ident { (extractIdentifier $3) : $1 }

-- page 122
TypeName :: { TypeName Identifier }
    : SpecifierQualifierList AbstractDeclarator { TypeName (reverse $1) (Just $2) }
    | SpecifierQualifierList                    { TypeName (reverse $1) Nothing   }

AbstractDeclarator  :: { AbstractDeclarator Identifier }
    : Pointer                           { ($1 ADPtr (Just)) Nothing         }
    | Pointer DirectAbstractDeclarator  { ($1 ADPtr (Just)) (Just $2) }
    | DirectAbstractDeclarator          { $1       }

DirectAbstractDeclarator :: { AbstractDeclarator Identifier }
    : '(' AbstractDeclarator ')'                        { $2           }
    | DirectAbstractDeclarator '['  AssignmentExpr ']'  { Array (Just $1) (Just $3) }
    | DirectAbstractDeclarator '['  ']'                 { Array (Just $1) Nothing   }
    | '['  AssignmentExpr ']'                           { Array Nothing (Just $2)   }
    | '['  ']'                                          { Array Nothing Nothing     }
    | DirectAbstractDeclarator '[' '*' ']'              { VarArray (Just $1)        }
    | '[' '*' ']'                                       { VarArray Nothing          }
    | DirectAbstractDeclarator '(' ParameterTypeList ')'{ Parens (Just $1) $3       }
    | DirectAbstractDeclarator '(' ')'                  { Parens (Just $1) []       }
    | '(' ParameterTypeList ')'                         { Parens Nothing $2         }
    | '(' ')'                                           { Parens Nothing []         }

-- page 123
TypedefName :: { Located Identifier }
    : typeName { L (extractSpan $1) (extractTypename $1) :: Located Identifier }

-- page 125
Initializer :: { Initializer Identifier }
    : AssignmentExpr                { InitExpr $1 }
    | '{' InitializerList '}'       { InitList (reverse $2) }
    | '{' InitializerList ',' '}'   { InitList (reverse $2) }

InitializerList :: { [(Maybe [Designator Identifier], Initializer Identifier)] }
    : Designation Initializer                       { [ (Just $1, $2) ]     }
    | Initializer                                   { [ (Nothing, $1 ) ]    }
    | InitializerList ',' Designation Initializer   { (Just $3, $4) : $1    }
    | InitializerList ',' Initializer               { (Nothing, $3) : $1    }

Designation :: { [Designator Identifier] }
    : DesignatorList '='    { reverse $1 }

DesignatorList :: {[Designator Identifier] }
    : Designator                    { [ $1 ] }
    | DesignatorList Designator     { $2 : $1 }

Designator  :: { Designator Identifier }
    : '[' ConstExpr ']' { DesignatorExpr $2 }
    | '.' ident         { DesignatorDot (extractIdentifier $2)  }


Statement :: { Statement Identifier }
    : ident ':' Statement                       { LabeledStmt (extractIdentifier $1) $3 }
    | case Expr ':' Statement                   { CaseStmt $2 $4 }
    | default ':' Statement                     { DefaultStmt $3 }
    | CompoundStatement                         { CompoundStmt $1 }
    | if '(' Expr ')' Statement                 { IfStmt $3 $5 Nothing      }
    | if '(' Expr ')' Statement else Statement  { IfStmt $3 $5 (Just $7)    }
    | switch '(' Expr ')' Statement             { SwitchStmt $3 $5          }

    | while '(' Expr ')' Statement              { WhileStmt $3 $5   }
    | do Statement while '(' Expr ')' ';'       { DoStmt $2 $5      }
    -- lots of opts
    | goto ident ';'                            { GotoStmt (extractIdentifier $2)           }
    | continue ';'                              { ContinueStmt          }
    | break ';'                                 { BreakStmt             }
    | return ';'                                { ReturnStmt Nothing    }
    | return Expr ';'                           { ReturnStmt (Just $2)  }

    | Expr  ';'                                 { ExpressionStmt (Just $1) }
    | ';'                                       { ExpressionStmt Nothing   }
        --ExpressionStatement                   { (ExpressionStmt $1) }

LBrace :: { () }
    : '{'           {% enterScope }

RBrace :: { () }
    : '}'           {% exitScope }

CompoundStatement :: { CompoundStatement Identifier }
    : LBrace BlockItemList RBrace { CompoundStatement (reverse $2) }



BlockItemList :: { [BlockItem Identifier] }
        -- In some cases the lookahead token may be an (Ident i) where i is the identifier being
        -- declared in the preceeding declaration.
        -- In this case the lookahead needs to be replaced with a (TypeName i) token.
    : BlockItemList Declaration {%% 
        (\(lookahead :: Maybe (Located Token)) -> do 
            decl <- fmap ((:$1) . BDecl) (handleDeclaration $2)
            case lookahead of
                Just i@(L _ (Ident _)) ->  do
                    tok <- lift (checkToken i)
                    leftover tok
                Just x -> leftover x
                Nothing -> pure ()
            pure decl ) }

    | BlockItemList Statement   {  (BStmt $2 : $1) }
    |                           { [] }

-- page 140

DeclarationList :: { [Declaration Identifier] }
    : DeclarationList  Declaration  { $2 : $1 }
    |                               { [] }

FunctionDefinition :: { FunctionDefinition Identifier }
    : DeclarationSpecifiers Declarator DeclarationList CompoundStatement    {FunctionDefinition $1 $2 (Just $ reverse $3) $4 }

ExternalDeclaration :: { ExternDecl Identifier }
    : FunctionDefinition    { EFunctionDef $1 }
    | Declaration           {%%
        (\lookahead -> do 
            decl <- fmap EDecl (handleDeclaration ($1))
            case lookahead of
                Just i@(L _ (Ident _)) ->  do
                    (tok :: Located Token) <- lift (checkToken i)
                    leftover tok
                Just x -> leftover x
                Nothing -> pure ()
            pure decl ) }

TranslationUnit :: { [ExternDecl Identifier] }
    : TranslationUnitI   { reverse $1 }

TranslationUnitI :: { [ExternDecl Identifier] }
    : ExternalDeclaration                    { [ $1 ] }
    | TranslationUnitI ExternalDeclaration   { $2 : $1 }

-- page 145
-- PREPROCESSING

{

type ParserScope = N.NonEmpty (S.Set Identifier)

newParserScope = S.empty:|[]

enterScope :: (State ParserScope :> es) => ConduitT i o (Eff es) ()
enterScope = lift $ modify (\(h:|t) -> h:|(h:t))

exitScope :: (State ParserScope :> es) => ConduitT i o (Eff es) ()
exitScope = lift $ modify (\case
    _:|(h:t) -> h:|t
    h:|[] -> h:|[])


getDeclaredIdent :: Declarator i -> i
getDeclaredIdent (DDIdent i) = i
getDeclaredIdent (DDPointer _ inner) = getDeclaredIdent inner
getDeclaredIdent (DDArr inner _ _ _ _) = getDeclaredIdent inner
getDeclaredIdent (DDFuncPList inner _) = getDeclaredIdent inner
getDeclaredIdent (DDFuncIList inner _) = getDeclaredIdent inner

containsTypeDef :: [DeclarationSpecifier Identifier] -> Bool
containsTypeDef = any $ \case 
    DSStorageSpec _ (SCTypedef) -> True
    _ -> False

handleDeclaration :: (IOE :> es, State ParserScope :> es) => Declaration Identifier -> ConduitT i o (Eff es) (Declaration Identifier)
handleDeclaration d@(Declaration specs initDeclarators) = do
    lift 
        $ when (containsTypeDef specs) 
        $ modify
            (\(h:|t) -> (foldl' (flip S.insert) h $ map (\(InitDeclaration i _) -> getDeclaredIdent i) initDeclarators):|t)
    lift $ (get >>= \x -> liftIO (print x))
    pure d


parseError :: (Error String :> es, State ParserScope :> es) => (Maybe (Located Token), [String]) -> ConduitT (Located Token) Void (Eff es) a
parseError (Just (L span t), tokens) = lift $ error $ "Parsing failed on token: \"" ++ show t ++ "\" at " ++ show span ++ ". possible tokens: " ++ show tokens  
parseError (Nothing, tokens) = lift $ error $ "Parsing failed at EOF"


checkToken :: (State ParserScope :> es) => Located Token -> Eff es (Located Token)
checkToken (L s (Ident i)) = do
        h:|_ <- get
        pure $ L s $ case S.member i h of
            True -> TTypeName i
            False -> Ident i
checkToken x = pure x 

injectTypeNameTokens :: (State ParserScope :> es) => ConduitT (Located Token) (Located Token) (Eff es) ()
injectTypeNameTokens = mapMC $ checkToken



extractIdentifier :: Maybe (Located Token) -> Identifier
extractIdentifier (Just (L _ (Ident i))) = i
extractIdentifier _ = error "extractIdentifier is a hack and I hate it but happy cant have multiple semantic values for a token (span + actual value) :("

extractTypename :: Maybe (Located Token) -> Identifier
extractTypename (Just (L _ (TTypeName i))) = i
extractTypename _ = error "extractTypename is a hack and I hate it but happy cant have multiple semantic values for a token (span + actual value) :("

extractConstant :: Maybe (Located Token) -> Constant
extractConstant (Just (L _ (Constant i))) = i
extractConstant _ = error "extractConstant is a hack and I hate it but happy cant have multiple semantic values for a token (span + actual value) :("

extractSpan :: Maybe (Located a) -> SrcSpan
extractSpan (Just (L s _)) = s
extractspan _ = error "extractSpan is a hack and I hate it but happy cant have multiple semantic values for a token (span + actual value) :("


}

-- --parseError :: Error String :> es => (Token, [String]) -> Eff es a
-- 
-- parseError :: (Error String :> es, State AlexState :> es, State ParserState :> es) => (Maybe Token, [String]) -> ConduitT Token Void (Eff es) a
-- parseError (t, tokens) = lift $ error $ "something failed :(, failed on token: \"" ++  show t ++ "\"possible tokens: " ++ show tokens  
-- 
-- injectTypeNameTokens :: (IOE :> es, State ParserState :> es) => ConduitT Token Token (Eff es) ()
-- injectTypeNameTokens = mapMC $ \case
--     Ident i -> isType i >>= \case
--         True -> (liftIO (print "type name")) >> (pure $ TTypeName i )
--         False -> pure $ Ident i 
--     x -> pure x 

-- data PCtx = PCtx
-- data ParserContext :: Effect
-- type instance DispatchOf ParserContext = Static NoSideEffects
-- newtype instance StaticRep ParserContext = ParserContext PCtx
--
--
-- lexer :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) =>  (Token -> Eff es a) -> Eff es a
-- lexer = (preprocess >>=)

--parseError :: Error String :> es => Token -> Eff es a
--parseError t = error "failure :("


