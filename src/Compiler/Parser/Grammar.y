{
{-# LANGUAGE NoMonomorphismRestriction #-}
module Compiler.Parser.Grammar(clike, injectTypeNameTokens) where

import Compiler.Parser.Lexer (AlexState)
import Compiler.Parser.Preprocessor (preprocess, PreprocessorState)

import Compiler.Parser.ParseTree
import Compiler.Parser.Tokens
import Compiler.SymbolTable (SymbolTable, isType)
import Compiler.Parser.GrammarHelpers
import Data.Map qualified as M
--data ParseError = ParseError

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import Conduit
}

%name clike TranslationUnit
%name expr Expr

%monad {(IOE :> es, Error String :> es, State AlexState :> es, State SymbolTable :> es, State PreprocessorState :> es )}  {ConduitT Token Void (Eff es) } {>>=} {return}
%lexer {(await >>=)} {Nothing}

%errorhandlertype explist
%error {parseError}
%tokentype { Maybe Token }

%token
    ident   { Just (Ident $$) }
    typeName{ Just (TTypeName $$) }
    stringlit { Just (StringLiteral $$) }
    constant  { Just (Constant $$) }

    auto    { Just (Keyword Auto)  }
    break   { Just (Keyword Break) }
    case    { Just (Keyword Case) }
    const   { Just (Keyword Const) }
    continue{ Just (Keyword Continue) }
    default { Just (Keyword Default) }
    do      { Just (Keyword Do) }
    else    { Just (Keyword Else) }
    extern  { Just (Keyword Extern) }
    enum    { Just (Keyword Enum) }
    for     { Just (Keyword For) }
    goto    { Just (Keyword Goto) }
    if      { Just (Keyword If) }
    inline  { Just (Keyword Inline) }
    register{ Just (Keyword Register) }
    restrict{ Just (Keyword Restrict) }
    return  { Just (Keyword Return) }
    sizeof  { Just (Keyword Sizeof) }
    static  { Just (Keyword TStatic) }
    struct  { Just (Keyword Struct) }
    switch  { Just (Keyword Switch) }
    typedef { Just (Keyword TypeDef) }
    union   { Just (Keyword Union) }
    volatile{ Just (Keyword Volatile) }
    while   { Just (Keyword While) }
    void    { Just (Keyword Void) }
    char    { Just (Keyword TChar) }
    short   { Just (Keyword TShort) }
    int     { Just (Keyword TInt) }
    long    { Just (Keyword TLong) }
    float   { Just (Keyword TFloat) }
    double  { Just (Keyword TDouble) }
    signed  { Just (Keyword TSigned) }
    unsigned{ Just (Keyword TUnsigned) }
    uBool   { Just (Keyword TuBool) }
    uComplex { Just (Keyword TuComplex) }
    uImaginary { Just (Keyword TuImaginary) }
    '{'     { Just (Punctuator LBrace) }
    '}'     { Just (Punctuator RBrace) }
    '('     { Just (Punctuator LParen) }
    ')'     { Just (Punctuator RParen) }
    '['     { Just (Punctuator LBrack) }
    ']'     { Just (Punctuator RBrack) }
    '->'    { Just (Punctuator Arrow)  }
    '&'     { Just (Punctuator BitAnd) }
    '|'     { Just (Punctuator BitOr)   }
    '*'     { Just (Punctuator Times) }
    '+'     { Just (Punctuator Plus) }
    '-'     { Just (Punctuator Minus) }
    '~'     { Just (Punctuator Compliment) }
    '!'     { Just (Punctuator Not) }
    '/'     { Just (Punctuator Divide) }
    '%'     { Just (Punctuator Modulo) }
    '<<'    { Just (Punctuator LShift) } 
    '>>'    { Just (Punctuator RShift) } 
    '<'     { Just (Punctuator Lt)     }
    '<='    { Just (Punctuator Le)     }
    '>'     { Just (Punctuator Gt)     }
    '>='    { Just (Punctuator Ge)     }
    '=='    { Just (Punctuator Eq)     }
    '!='    { Just (Punctuator Neq)    }
    '^'     { Just (Punctuator BitXor) }
    '&&'    { Just (Punctuator LAnd)   }
    '||'    { Just (Punctuator LOr)    }
    ';'     { Just (Punctuator Semi)   }
    '='     { Just (Punctuator Assign) }
    ','     { Just (Punctuator Comma)  }
    '.'     { Just (Punctuator Dot) }
    ':'     { Just (Punctuator Colon)}

    '++'    { Just (Punctuator PlusPlus) }
    '--'    { Just (Punctuator MinusMinus) }
    '?'     { Just (Punctuator Question)  }
    '...'   { Just (Punctuator Variadic)  }
    '*='    { Just (Punctuator TimesAssign) }
    '/='    { Just (Punctuator DivAssign) }
    '%='    { Just (Punctuator ModAssign) }
    '+='    { Just (Punctuator PlusAssign) }
    '-='    { Just (Punctuator MinusAssign) }
    '<<='   { Just (Punctuator LShiftAssign) }
    '>>='   { Just (Punctuator RShiftAssign) }
    '&='    { Just (Punctuator AndAssign) }
    '^='    { Just (Punctuator XorAssign) }
    '|='    { Just (Punctuator OrAssign) }
    -- '#'     { Just (Punctuator Stringize) }
    -- '##'    { Just (Punctuator TokenPaste) }

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
    : ident                                 { EIdent $1         }
    | constant                              { EConstant $1      }
    | stringlit                             { EStringLiteral $1 }
    | '(' Expr ')'                          { $2                }


PostfixExpr  :: { Expr Identifier }
    : PrimaryExpr                           { $1                }
    | PostfixExpr '[' Expr ']'              { (Bracketed $1 $3) }

    -- prob a better way to do this 
    | PostfixExpr '(' ')'                   { Called $1 []              }
    | PostfixExpr '(' ArgExprList ')'       { Called $1 (reverse $3)    }

    | PostfixExpr '.' ident                 { (DotE $1 $3)          }
    | PostfixExpr '->' ident                { (ArrowE $1 $3)        }
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

DeclarationSpecifiers :: { [DeclarationSpecifiers Identifier] }
    : DeclarationSpecifiers DeclarationSpecifier    { $2 : $1 }
    | DeclarationSpecifier                          { [$1] }

DeclarationSpecifier :: { DeclarationSpecifiers Identifier }
    : StorageClassSpecifier                         { DSStorageSpec $1 }
    | TypeSpecifier                                 { DSTypeSpec $1    }
    | TypeQualifier                                 { DSTypeQual $1    }
    | FunctionSpecifier                             { DSFuncSpec $1    }

InitDeclarationList :: { [InitDeclaration Identifier] }
    : InitDeclarator                            { [ $1 ] } 
    | InitDeclarationList ',' InitDeclarator    { $3 : $1 }

InitDeclarator :: { InitDeclaration Identifier }
    : Declarator                    { InitDeclaration $1 Nothing }
    | Declarator '=' Initializer    { InitDeclaration $1 (Just $3) }


-- page 98
StorageClassSpecifier :: { StorageClassSpecifier  }
    : typedef   { SCTypedef     }
    | extern    { SCExtern      }
    | static    { SCStatic      }
    | auto      { SCAuto        }
    | register  { SCRegister    }

    -- this needs to have the standard types and stuff
-- page 99
TypeSpecifier :: { TypeSpecifier Identifier }
    : void                      { PrimType PVoid        }
    | char                      { PrimType PChar        }
    | short                     { PrimType PShort       }
    | int                       { PrimType PInt         }
    | long                      { PrimType PLong        }
    | float                     { PrimType PFloat       }
    | double                    { PrimType PDouble      }
    | signed                    { PrimType PSigned      }
    | unsigned                  { PrimType PUnsigned    }
    | uBool                     { PrimType PuBool       }
    | uComplex                  { PrimType PuComplex    }
    | uImaginary                { PrimType PuImaginary  }
    | StructOrUnionSpecifier    { StructType $1         }
    | EnumSpecifier             { EnumType $1           }
    | TypedefName               { IdentType $1          }

-- page 108
TypeQualifier :: { TypeQualifier }
    : const         { TQConst       }
    | restrict      { TQRestrict    }
    | volatile      { TQVolatile    }

-- page 112
FunctionSpecifier :: { FunctionSpecifier }
    : inline    { FSInline }
-- page 101
-- might merge these two rules into 1 with 6 rules
StructOrUnionSpecifier :: { DataLayoutSpec Identifier }
    : StructOrUnion ident '{' StructDeclarationList '}'
        { case $1 of 
            SUStruct -> StructDef (Just $2) $4
            SUUnion -> UnionDef (Just $2) $4 }
    | StructOrUnion  '{' StructDeclarationList '}' 
        { case $1 of 
            SUStruct -> StructDef Nothing $3
            SUUnion -> UnionDef Nothing $3 }
    | StructOrUnion ident
        { case $1 of
            SUStruct -> StructRef $2
            SUUnion -> UnionRef $2 }

StructOrUnion :: { StructOrUnion }
    : struct    { SUStruct }
    | union     { SUUnion  }


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
    : TypeSpecifier SpecifierQualifierList  { (Left $1) : $2    }
    | TypeQualifier SpecifierQualifierList  { (Right $1) : $2   }
    | TypeSpecifier                         { [ Left $1 ]       }
    | TypeQualifier                         { [ Right $1 ]      }

-- unfinished
StructDeclarator :: { StructDeclarator Identifier }
    : Declarator            { (StructDeclarator $1 Nothing) }
    | Declarator ':' Expr   { (StructDeclarator $1 (Just $3)) }
                        -- | ':' Expr   { StructDeclarator $1 (Just $3) }
                        -- I could support bitfields but I dont really want to :/

-- page 104
EnumSpecifier :: { EnumSpecifier Identifier }
    : enum ident '{' EnumeratorList '}'     { EnumSpecifier (Just $2) (reverse $4)  }
    | enum  '{' EnumeratorList '}'          { EnumSpecifier Nothing   (reverse $3)  }
    | enum ident '{' EnumeratorList ',' '}' { EnumSpecifier (Just $2) (reverse $4)  }
    | enum '{' EnumeratorList ',' '}'       { EnumSpecifier Nothing   (reverse $3)  }
    | enum ident                            { EnumRef $2                     }

                                    -- this needs to be a "constant" expression
-- little hack for the list building
--EnumeratorList  :   EnumeratorListI { reverse $1                }

EnumeratorList :: { [(Identifier, Maybe (Expr Identifier))] }
    : Enumerator                        { [ $1 ]    }
    | EnumeratorList ',' Enumerator    { $3 : $1   }

Enumerator :: { (Identifier, Maybe (Expr Identifier)) }
    : EnumerationConstant             { ($1, Nothing) }
    | EnumerationConstant '=' Expr    { ($1, Just $3) }
                                    -- this needs to be a "constant" expression

EnumerationConstant :: { Identifier }
    : ident { $1 :: Identifier }

-- Page 114

Declarator :: { Declarator Identifier }
    : Pointer DirectDeclarator      { ($1 DDPointer id) $2 }
    | DirectDeclarator              { $1 }


DirectDeclarator :: { Declarator Identifier }
    : ident                                                             { DDIdent $1}
    | '(' Declarator ')'                                               { $2              }
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
Pointer :: { forall a b. ([TypeQualifier] -> a -> b) -> (b -> a) -> a -> b }
    : '*'                           { \constr _ -> constr [] }
    | '*' TypeQualifierList         { \constr _ -> constr (reverse $2)    }
    -- | '*' TypeQualifierList Pointer { \constr (c :: a) d -> constr (reverse $2) (($3 constr) c)  }
    | '*' Pointer                   { \constr d c  -> constr [] (d ($2 constr d c)) }

TypeQualifierList :: {[TypeQualifier ]}
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
    : ident                     { [ $1 ]  }
    | IdentifierList ',' ident { $3 : $1 }

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
TypedefName :: { Identifier }
    : typeName { $1 }

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
    : DesignatorList '='    { reverse $1}

DesignatorList :: {[Designator Identifier] }
    : Designator                    { [ $1 ]        }
    | DesignatorList Designator    { $2 : $1       }

Designator  :: { Designator Identifier }
    : '[' ConstExpr ']' { DesignatorExpr $2 }
    | '.' ident         { DesignatorDot $2  }


Statement :: { Statement Identifier }
    : ident ':' Statement           { LabeledStmt $1 $3 }
    | case Expr ':' Statement       { CaseStmt $2 $4 }
    | default ':' Statement         { DefaultStmt $3 }
    | CompoundStatement     { CompoundStmt $1 }
    | if '(' Expr ')' Statement                 { IfStmt $3 $5 Nothing      }
    | if '(' Expr ')' Statement else Statement  { IfStmt $3 $5 (Just $7)    }
    | switch '(' Expr ')' Statement             { SwitchStmt $3 $5          }

    | while '(' Expr ')' Statement          { WhileStmt $3 $5   }
    | do Statement while '(' Expr ')' ';'   { DoStmt $2 $5      }
    -- | for '()'
    -- lots of opts
    | goto ident ';'        { GotoStmt $2           }
    | continue ';'          { ContinueStmt          }
    | break ';'             { BreakStmt             }
    | return ';'            { ReturnStmt Nothing    }
    | return Expr ';'       { ReturnStmt (Just $2)  }

    | Expr  ';'     { ExpressionStmt (Just $1) }
    | ';'           { ExpressionStmt Nothing   }
        --ExpressionStatement   { (ExpressionStmt $1) }


CompoundStatement :: { CompoundStatement Identifier }
    : '{' BlockItemList '}' { CompoundStatement (reverse $2) }

BlockItemList :: { [BlockItem Identifier] }
    : BlockItemList Declaration {  (BDecl $2 : $1) }
    | BlockItemList Statement   {  (BStmt $2 : $1) }
    |                           { [] }

-- page 140

DeclarationList :: { [Declaration Identifier] }
    : DeclarationList  Declaration  { $2 : $1 }
    |                               { [] }

FunctionDefinition :: { FunctionDefinition Identifier }
    : DeclarationSpecifiers Declarator DeclarationList CompoundStatement    {FunctionDefinition $1 $2 (Just $ reverse $3) $4 }

ExternalDeclaration :: { ExternDecl Identifier }
    : FunctionDefinition    {FunctionDef $1   }
    | Declaration           {EDecl $1         }

TranslationUnit :: { [ExternDecl Identifier] }
    : TranslationUnitI   { reverse $1 }

TranslationUnitI :: { [ExternDecl Identifier] }
    : ExternalDeclaration                    { [ $1 ] }
    | TranslationUnitI ExternalDeclaration   { $2 : $1 }

-- page 145
-- PREPROCESSING

--


{


--parseError :: Error String :> es => (Token, [String]) -> Eff es a
parseError :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) => (Maybe Token, [String]) -> ConduitT Token Void (Eff es) a
parseError (t, tokens) = lift $ error $ "something failed :(, failed on token: \"" ++  show t ++ "\"possible tokens: " ++ show tokens  

injectTypeNameTokens :: (State SymbolTable :> es) => ConduitT Token Token (Eff es) ()
injectTypeNameTokens = await >>= \case
    Just (Ident i) -> do
        tbl <- lift get
        yield $ if isType i tbl
            then TTypeName i
            else Ident i
        injectTypeNameTokens
    Just x -> yield x >> injectTypeNameTokens
    Nothing -> pure ()


}

-- data PCtx = PCtx
--
-- data ParserContext :: Effect
--
-- type instance DispatchOf ParserContext = Static NoSideEffects
--
-- newtype instance StaticRep ParserContext = ParserContext PCtx
--
--
-- lexer :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) =>  (Token -> Eff es a) -> Eff es a
-- lexer = (preprocess >>=)

--parseError :: Error String :> es => Token -> Eff es a
--parseError t = error "failure :("


