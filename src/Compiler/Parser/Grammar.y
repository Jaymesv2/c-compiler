{
{-# LANGUAGE NoMonomorphismRestriction #-}
module Compiler.Parser.Grammar where

import Compiler.Parser.Lexer (AlexState)
import Compiler.Parser.Preprocessor (preprocess, PreprocessorState)

import Compiler.Parser.ParseTree
import Compiler.Parser.Tokens
import Compiler.Parser (SymbolTable)

--data ParseError = ParseError

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

}

%name clike TranslationUnit
%name expr Expr

%monad {(IOE :> es, Error String :> es, State AlexState :> es, State SymbolTable :> es, State PreprocessorState :> es )}  {Eff es} {>>=} {return}
--%lexer {lexer} {EOF}
%lexer {(preprocess >>=)} {EOF}

%errorhandlertype explist
%error {parseError}
%tokentype { Token }

%token
    ident   { Ident $$ }
    typeName{ TTypeName $$ }
    stringlit { StringLiteral $$ }
    constant  { Constant $$ }

    auto    { Keyword Auto  }
    break   { Keyword Break }
    case    { Keyword Case }
    const   { Keyword Const }
    continue{ Keyword Continue }
    default { Keyword Default }
    do      { Keyword Do }
    else    { Keyword Else }
    extern  { Keyword Extern }
    enum    { Keyword Enum }
    for     { Keyword For }
    goto    { Keyword Goto}
    if      { Keyword If }
    inline  { Keyword Inline }
    register{ Keyword Register }
    restrict{ Keyword Restrict }
    return  { Keyword Return }
    sizeof  { Keyword Sizeof}
    static  { Keyword TStatic }
    struct  { Keyword Struct}
    switch  { Keyword Switch }
    typedef { Keyword TypeDef }
    union   { Keyword Union }
    volatile{ Keyword Volatile}
    while   { Keyword While}

    void    { Keyword Void }
    char    { Keyword TChar}
    short   { Keyword TShort}
    int     { Keyword TInt}
    long    { Keyword TLong}
    float   { Keyword TFloat}
    double  { Keyword TDouble}
    signed  { Keyword TSigned}
    unsigned{ Keyword TUnsigned}
    uBool   { Keyword TuBool}
    uComplex { Keyword TuComplex}
    uImaginary { Keyword TuImaginary}
    '{'     { Punctuator LBrace }
    '}'     { Punctuator RBrace }
    '('     { Punctuator LParen }
    ')'     { Punctuator RParen }
    '['     { Punctuator LBrack }
    ']'     { Punctuator RBrack }
    '->'    { Punctuator Arrow  }
    '&'     { Punctuator BitAnd }
    '|'     { Punctuator BitOr   }
    '*'     { Punctuator Times }
    '+'     { Punctuator Plus }
    '-'     { Punctuator Minus }
    '~'     { Punctuator Compliment }
    '!'     { Punctuator Not }
    '/'     { Punctuator Divide }
    '%'     { Punctuator Modulo }
    '<<'    { Punctuator LShift } 
    '>>'    { Punctuator RShift } 
    '<'     { Punctuator Lt     }
    '<='    { Punctuator Le     }
    '>'     { Punctuator Gt     }
    '>='    { Punctuator Ge     }
    '=='    { Punctuator Eq     }
    '!='    { Punctuator Neq    }
    '^'     { Punctuator BitXor }
    '&&'    { Punctuator LAnd   }
    '||'    { Punctuator LOr    }
    ';'     { Punctuator Semi   }
    '='     { Punctuator Assign }
    ','     { Punctuator Comma  }
    '.'     { Punctuator Dot }
    ':'     { Punctuator Colon}

    '++'    { Punctuator PlusPlus }
    '--'    { Punctuator MinusMinus }
    '?'     { Punctuator Question  }
    '...'   { Punctuator Variadic  }
    '*='    { Punctuator TimesAssign }
    '/='    { Punctuator DivAssign}
    '%='    { Punctuator ModAssign}
    '+='    { Punctuator PlusAssign}
    '-='    { Punctuator MinusAssign}
    '<<='   { Punctuator LShiftAssign}
    '>>='   { Punctuator RShiftAssign}
    '&='    { Punctuator AndAssign}
    '^='    { Punctuator XorAssign}
    '|='    { Punctuator OrAssign}
    -- '#'     { Punctuator Stringize }
    -- '##'    { Punctuator TokenPaste}


-- %name declaration Declaration
-- 
-- %name declarator Declarator
-- %name directDeclarator DirectDeclarator
-- %name directAbstractDeclarator DirectAbstractDeclarator
-- %name abstractDeclarator AbstractDeclarator
-- 
-- %name typeSpecifier TypeSpecifier
-- %name typeQualifier TypeQualifier
-- %name structOrUnionSpecifier StructOrUnionSpecifier
-- 
-- %name externalDeclaration ExternalDeclaration
-- %name functionDefinition FunctionDefinition
-- %name translationUnit TranslationUnit
-- 
-- %name parameterDeclaration ParameterDeclaration
-- %name parameterList ParameterList
-- %name identifierList IdentifierList
-- 
-- 
-- %name statement Statement
-- %name compoundStatement CompoundStatement
-- %name blockItem BlockItem
-- %name selectionStatement SelectionStatement
-- %name iterationStatement IterationStatement
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
PrimaryExpr :: { Expr }
    : ident                                 { EIdent $1         :: Expr }
    | constant                              { EConstant $1      :: Expr}
    | stringlit                             { EStringLiteral $1 :: Expr }
    | '(' Expr ')'                          { $2                :: Expr }


PostfixExpr  :: { Expr }
    : PrimaryExpr                           { $1                :: Expr }
    | PostfixExpr '[' Expr ']'              { (Bracketed $1 $3) :: Expr }

    -- prob a better way to do this 
    | PostfixExpr '(' ')'                   { Called $1 []              :: Expr }
    | PostfixExpr '(' ArgExprList ')'       { Called $1 (reverse $3)    :: Expr }

    | PostfixExpr '.' ident                 { (DotE $1 $3)          :: Expr }
    | PostfixExpr '->' ident                { (ArrowE $1 $3)        :: Expr }
    | PostfixExpr '++'                      { (UnaryE UPostIncr $1) :: Expr }
    | PostfixExpr '--'                      { (UnaryE UPostDecr $1) :: Expr }
    | '(' TypeName ')' '{' InitializerList '}'     { InitE $2 (reverse $5) }
    | '(' TypeName ')' '{' InitializerList ',' '}' { InitE $2 (reverse $5) }

ArgExprList :: { [Expr] }
    : AssignmentExpr                        { [ $1 ]    :: [Expr] }
    | ArgExprList ',' AssignmentExpr        { $3 : $1   :: [Expr] }

UnaryOp  :: { UnaryOp }
    : '&'                                   { URef        :: UnaryOp }
    | '*'                                   { UDeref      :: UnaryOp }
    | '~'                                   { UCompliment :: UnaryOp }
    | '!'                                   { UNot        :: UnaryOp }
    | '+'                                   { UPlus       :: UnaryOp }
    | '-'                                   { UMinus      :: UnaryOp }


UnaryExpr :: { Expr }
    : PostfixExpr                           { $1                    :: Expr }
    | '++' UnaryExpr                        { UnaryE UPreIncr $2    :: Expr }
    | '--' UnaryExpr                        { UnaryE UPreDecr $2    :: Expr }

    | UnaryOp CastExpr                      { UnaryE $1 $2          :: Expr }
    {-| '&' CastExpr { UnaryE URef $2        :: Expr }
    | '*' CastExpr { UnaryE UDeref $2      :: Expr }
    | '~' CastExpr { UnaryE UCompliment $2 :: Expr }
    | '!' CastExpr { UnaryE UNot $2        :: Expr }
    | '+' CastExpr { UnaryE UPlus $2       :: Expr }
    | '-' CastExpr { UnaryE UMinus $2      :: Expr }
    -}
    | sizeof UnaryExpr                      { UnaryE USizeof $2     :: Expr }
    | sizeof '(' TypeName ')'               { SizeofTypeE $3        :: Expr }

CastExpr :: { Expr }
    : UnaryExpr                             { $1            :: Expr }
    | '(' TypeName ')' CastExpr             { CastE $2 $4   :: Expr}

MultiplicativeExpr   :: { Expr }
    : CastExpr                              { $1                    :: Expr } 
    | MultiplicativeExpr '*' CastExpr       { BinaryOp $1 BMul $3   :: Expr }
    | MultiplicativeExpr '/' CastExpr       { BinaryOp $1 BDiv $3   :: Expr }
    | MultiplicativeExpr '%' CastExpr       { BinaryOp $1 BMod $3   :: Expr }

AdditiveExpr  :: { Expr }   
    : MultiplicativeExpr                    { $1                    :: Expr }
    | AdditiveExpr '+' MultiplicativeExpr   { BinaryOp $1 BAdd $3   :: Expr }
    | AdditiveExpr '-' MultiplicativeExpr   { BinaryOp $1 BSub $3   :: Expr }

ShiftExpr    :: { Expr }
    : AdditiveExpr                          { $1                        :: Expr }
    | ShiftExpr '<<' AdditiveExpr           { BinaryOp $1 BShiftL $3    :: Expr }
    | ShiftExpr '>>' AdditiveExpr           { BinaryOp $1 BShiftR $3    :: Expr }

RelationalExpr   :: { Expr }
    : ShiftExpr                             { $1                    :: Expr }
    | RelationalExpr '<' ShiftExpr          { BinaryOp $1 Blt $3    :: Expr }
    | RelationalExpr '>' ShiftExpr          { BinaryOp $1 Bgt $3    :: Expr }
    | RelationalExpr '<=' ShiftExpr         { BinaryOp $1 Ble $3    :: Expr }
    | RelationalExpr '>=' ShiftExpr         { BinaryOp $1 Bge $3    :: Expr }

EqualityExpr :: { Expr }    
    : RelationalExpr                        { $1                    :: Expr }
    | EqualityExpr '==' RelationalExpr      { BinaryOp $1 Beq $3    :: Expr }
    | EqualityExpr '!=' RelationalExpr      { BinaryOp $1 Bneq $3   :: Expr }

AndExpr  :: { Expr }
    : EqualityExpr                          { $1                    :: Expr }
    | AndExpr '&' EqualityExpr              {BinaryOp $1 BBitAnd $3 :: Expr }

XorExpr  :: { Expr }
    : AndExpr                               { $1                    :: Expr }
    | XorExpr '^' AndExpr                   {BinaryOp $1 BBitXor $3 :: Expr }

OrExpr   :: { Expr }
    : XorExpr                               { $1                    :: Expr }
    | OrExpr '|' XorExpr                    {BinaryOp $1 BBitOr $3  :: Expr }

-- Expr
LAndExpr     :: { Expr }
    : OrExpr                                { $1                            :: Expr }
    | LAndExpr  '&&' OrExpr                 { BinaryOp $1 BLogicalAnd $3    :: Expr }

-- Expr
LOrExpr      :: { Expr }
    : LAndExpr                              { $1                        :: Expr }
    | LOrExpr '||' LAndExpr                 { BinaryOp $1 BLogicalOr $3 :: Expr }

-- could add ternary operator here
ConditionalExpr :: { Expr }
    : LOrExpr                               { $1                        :: Expr }
    | LOrExpr '?' Expr ':' ConditionalExpr  { ConditionalExpr $1 $3 $5  :: Expr}


AssignmentOperator :: { AssignmentOp}
    : '*='  { ATimesAssign  :: AssignmentOp }
    | '/='  { ADivAssign    :: AssignmentOp }
    | '%='  { AModAssign    :: AssignmentOp }
    | '+='  { APlusAssign   :: AssignmentOp }
    | '-='  { AMinusAssign  :: AssignmentOp }
    | '<<=' { ALShiftAssign :: AssignmentOp }
    | '>>=' { ARShiftAssign :: AssignmentOp }
    | '&='  { AAndAssign    :: AssignmentOp }
    | '^='  { AXorAssign    :: AssignmentOp }
    | '|='  { AOrAssign     :: AssignmentOp }

-- Expr
AssignmentExpr :: { Expr }
    : ConditionalExpr                                   { $1                        :: Expr }
    | AssignmentExpr '=' ConditionalExpr                { SimpleAssignE $1 $3       :: Expr }
    | AssignmentExpr AssignmentOperator ConditionalExpr { CompoundAssignE $1 $2 $3  :: Expr }

Expr :: { Expr }
    : AssignmentExpr            { $1            :: Expr }
    | Expr ',' AssignmentExpr   { CommaE $1 $3  :: Expr }


-- should be evaluated at translation time
-- no increment, decrement, function calls, comma ops, unless they are in an unevaluated subexpression
ConstExpr :: {Expr}
    : Expr   { $1 :: Expr }

Declaration :: { Declaration }
    : DeclarationSpecifiers InitDeclarationList ';' { Declaration $1 (Just ((reverse $2) :: [InitDeclaration])) :: Declaration }
    | DeclarationSpecifiers ';'                     { Declaration $1  Nothing :: Declaration }

-- page 97

-- Declarations 

DeclarationSpecifiers :: { [DeclarationSpecifiers] }
    : DeclarationSpecifiers DeclarationSpecifier    { $2:$1 }
    | DeclarationSpecifier                          { [$1] }

DeclarationSpecifier :: { DeclarationSpecifiers }
    : StorageClassSpecifier                         { DSStorageSpec $1 }
    | TypeSpecifier                                 { DSTypeSpec $1    }
    | TypeQualifier                                 { DSTypeQual $1    }
    | FunctionSpecifier                             { DSFuncSpec $1    }

InitDeclarationList :: { [InitDeclaration] }
    : InitDeclarator                            { [ $1 ] } 
    | InitDeclarationList ',' InitDeclarator    { $3 : $1 }

InitDeclarator :: { InitDeclaration }
    : Declarator                    { UninitDeclaration $1 }
    | Declarator '=' Initializer    { InitDeclaration $1 $3 }


-- page 98
StorageClassSpecifier :: { StorageClassSpecifier }
    : typedef   { SCTypedef     :: StorageClassSpecifier }
    | extern    { SCExtern      :: StorageClassSpecifier }
    | static    { SCStatic      :: StorageClassSpecifier }
    | auto      { SCAuto        :: StorageClassSpecifier }
    | register  { SCRegister    :: StorageClassSpecifier }

    -- this needs to have the standard types and stuf
-- page 99
TypeSpecifier :: { TypeSpecifier }
    : void                      { PrimType PVoid        :: TypeSpecifier }
    | char                      { PrimType PChar        :: TypeSpecifier }
    | short                     { PrimType PShort       :: TypeSpecifier }
    | int                       { PrimType PInt         :: TypeSpecifier }
    | long                      { PrimType PLong        :: TypeSpecifier }
    | float                     { PrimType PFloat       :: TypeSpecifier }
    | double                    { PrimType PDouble      :: TypeSpecifier }
    | signed                    { PrimType PSigned      :: TypeSpecifier }
    | unsigned                  { PrimType PUnsigned    :: TypeSpecifier }
    | uBool                     { PrimType PuBool       :: TypeSpecifier }
    | uComplex                  { PrimType PuComplex    :: TypeSpecifier }
    | uImaginary                { PrimType PuImaginary  :: TypeSpecifier }
    | StructOrUnionSpecifier    { StructType $1         :: TypeSpecifier }
    | EnumSpecifier             { EnumType $1           :: TypeSpecifier }
    | TypedefName               { IdentType $1          :: TypeSpecifier }

-- page 108
TypeQualifier :: { TypeQualifier }
    : const         { TQConst       :: TypeQualifier }
    | restrict      { TQRestrict    :: TypeQualifier }
    | volatile      { TQVolatile    :: TypeQualifier }

-- page 112
FunctionSpecifier :: { FunctionSpecifier }
    : inline    { FSInline :: FunctionSpecifier }
-- page 101
-- might merge these two rules into 1 with 6 rules
StructOrUnionSpecifier :: {DataLayoutSpec}
    : StructOrUnion ident '{' StructDeclarationList '}' { DataLayoutSpec $1 (Just $2) (Just ((reverse $4) :: [StructDeclaration])) :: DataLayoutSpec}
    | StructOrUnion  '{' StructDeclarationList '}'      { DataLayoutSpec $1 Nothing (Just (reverse $3)) :: DataLayoutSpec }
    | StructOrUnion ident                               { DataLayoutSpec $1 (Just $2) Nothing :: DataLayoutSpec }

StructOrUnion :: { StructOrUnion }
    : struct    { SUStruct :: StructOrUnion }
    | union     { SUUnion  :: StructOrUnion }


StructDeclarationList :: { [StructDeclaration] }
    : StructDeclaration                         { [ $1 ] :: [StructDeclaration]  }
    | StructDeclarationList StructDeclaration   { ($2 : $1) :: [StructDeclaration] }

--StructDeclaration       : SpecifierQualifierList StructDeclarator { StructDeclaration $1 ([$2] :: [StructDeclarator]) }

StructDeclaration :: { StructDeclaration }
    : SpecifierQualifierList StructDeclaratorList ';' { StructDeclaration (reverse $1) (reverse $2 :: [StructDeclarator]) :: StructDeclaration }

StructDeclaratorList :: { [StructDeclarator] }
    : StructDeclarator                              { [ $1 ] :: [StructDeclarator] }
    | StructDeclaratorList ',' StructDeclarator   { ($3 : $1) :: [StructDeclarator] }

--SpecifierQualifierList  : SpecifierQualifierListI { (reverse $1) :: [SpecifierQualifier] }

SpecifierQualifierList :: { [SpecifierQualifier] }
    : TypeSpecifier SpecifierQualifierList  { (Left $1) : $2    :: [SpecifierQualifier] }
    | TypeQualifier SpecifierQualifierList  { (Right $1) : $2   :: [SpecifierQualifier] }
    | TypeSpecifier                         { [ Left $1 ]       :: [SpecifierQualifier] }
    | TypeQualifier                         { [ Right $1 ]      :: [SpecifierQualifier] }

-- unfinished
StructDeclarator :: { StructDeclarator }
    : Declarator            { (StructDeclarator ($1 :: Declarator) Nothing) :: StructDeclarator }
    | Declarator ':' Expr   { (StructDeclarator ($1 :: Declarator) (Just ($3 :: Expr))) :: StructDeclarator }
                        --| ':' Expr   { (StructDeclarator ($1 :: Declarator) (Just ($3 :: Expr))) :: StructDeclarator }
                        -- I could support bitfields but I dont really want to :/

-- page 104
EnumSpecifier :: { EnumSpecifier }
    : enum ident '{' EnumeratorList '}'     { EnumSpecifier (Just $2) (reverse $4)  :: EnumSpecifier }
    | enum  '{' EnumeratorList '}'          { EnumSpecifier Nothing   (reverse $3)  :: EnumSpecifier }
    | enum ident '{' EnumeratorList ',' '}' { EnumSpecifier (Just $2) (reverse $4)  :: EnumSpecifier }
    | enum '{' EnumeratorList ',' '}'       { EnumSpecifier Nothing   (reverse $3)  :: EnumSpecifier }
    | enum ident                            { EnumForwardRef $2                     :: EnumSpecifier }

                                    -- this needs to be a "constant" expression
-- little hack for the list building
--EnumeratorList  :   EnumeratorListI { reverse $1                :: [(Identifier, Maybe Expr)] }

EnumeratorList :: { [(Identifier, Maybe Expr)] }
    : Enumerator                        { [ $1 ]    :: [(Identifier, Maybe Expr)] }
    | EnumeratorList ',' Enumerator    { $3 : $1   :: [(Identifier, Maybe Expr)] }

Enumerator :: { (Identifier, Maybe Expr) }
    : EnumerationConstant             { ($1, Nothing) :: (Identifier, Maybe Expr) }
    | EnumerationConstant '=' Expr    { ($1, Just $3) :: (Identifier, Maybe Expr) }
                                    -- this needs to be a "constant" expression

EnumerationConstant :: { Identifier }
    : ident { $1 :: Identifier }

-- Page 114
Declarator :: {Declarator}
    : Pointer DirectDeclarator      { Declarator (Just $1) $2 :: Declarator }
    | DirectDeclarator              { Declarator Nothing $1   :: Declarator }

DirectDeclarator :: { DirectDeclarator }
    : ident                                                             { DDIdent $1 :: DirectDeclarator }
    | '(' Declarator ')'                                                { DDRec $2                  :: DirectDeclarator }
    -- array declarations
    | DirectDeclarator '[' TypeQualifierList AssignmentExpr ']'         { ( DDArr $1 False $3 (Just $4) False) :: DirectDeclarator }
    | DirectDeclarator '[' TypeQualifierList '*' ']'                    { ( DDArr $1 False $3 Nothing   True ) :: DirectDeclarator }       
    | DirectDeclarator '[' TypeQualifierList ']'                        { ( DDArr $1 False $3 Nothing   False) :: DirectDeclarator }
    | DirectDeclarator '[' static TypeQualifierList AssignmentExpr ']'  { ( DDArr $1 True  $4 (Just $5) False) :: DirectDeclarator }
    | DirectDeclarator '[' static AssignmentExpr ']'                    { ( DDArr $1 True  [] (Just $4) False) :: DirectDeclarator }
    | DirectDeclarator '[' TypeQualifierList static AssignmentExpr ']'  { ( DDArr $1 True  $3 (Just $5) False) :: DirectDeclarator }
    | DirectDeclarator '[' AssignmentExpr ']'                           { ( DDArr $1 False [] (Just $3) False) :: DirectDeclarator }
    | DirectDeclarator '[' '*' ']'                                      { ( DDArr $1 False [] Nothing   True ) :: DirectDeclarator }     
    | DirectDeclarator '[' ']'                                          { ( DDArr $1 False [] Nothing   False) :: DirectDeclarator }
    -- function declarations
    | DirectDeclarator '(' ParameterTypeList ')'                        { ( DDFuncPList   $1 $3  )  :: DirectDeclarator }     
    | DirectDeclarator '(' IdentifierList ')'                           { ( DDFuncIList $1 (reverse $3))      :: DirectDeclarator }    
    | DirectDeclarator '(' ')'                                          { ( DDFuncIList $1 [])      :: DirectDeclarator }   
                                                                            
-- declarators
Pointer :: { Pointer }
    : '*'                           { (Pointer [] Nothing)   :: Pointer }
    | '*' TypeQualifierList         { (Pointer (reverse $2) Nothing)   :: Pointer }
    | '*' TypeQualifierList Pointer { (Pointer (reverse $2) (Just $3)) :: Pointer }
    | '*' Pointer                   { (Pointer [] (Just $2)) :: Pointer}

--TypeQualifierList       : TypeQualifierListI                { reverse $1 :: [TypeQualifier] }
TypeQualifierList :: {[TypeQualifier]}
    : TypeQualifier                     { [ $1 ]     :: [TypeQualifier] }
    | TypeQualifierList TypeQualifier  { $2 : $1    :: [TypeQualifier] }

ParameterTypeList  :: {[ParameterDeclaration]}
    : ParameterList                 { reverse $1 :: [ParameterDeclaration] }
    | ParameterList ',' '...'       { reverse (VariadicDeclaration : $1) }

ParameterList :: { [ParameterDeclaration] }
    : ParameterDeclaration                      { [ $1 ]  :: [ParameterDeclaration] }
    | ParameterList ',' ParameterDeclaration    { $3 : $1 :: [ParameterDeclaration] }

ParameterDeclaration :: { ParameterDeclaration }
    : DeclarationSpecifiers Declarator          { (ParameterDeclaration $1 $2 ) :: ParameterDeclaration}
    | DeclarationSpecifiers AbstractDeclarator  { (AbsParameterDeclaration $1 (Just $2) ) :: ParameterDeclaration}
    | DeclarationSpecifiers                     { (AbsParameterDeclaration $1 Nothing ) :: ParameterDeclaration}

IdentifierList :: { [Identifier] }
    : ident                     { [ $1 ]     :: [Identifier]}
    | IdentifierList ',' ident { $3 : $1    :: [Identifier]}


-- page 122
TypeName :: {TypeName }
    : SpecifierQualifierList AbstractDeclarator { TypeName (reverse $1) (Just $2) :: TypeName }
    | SpecifierQualifierList                    { TypeName (reverse $1) Nothing   :: TypeName }

AbstractDeclarator  :: {AbstractDeclarator}
    : Pointer                           { ADPtr $1          :: AbstractDeclarator }
    | Pointer DirectAbstractDeclarator  { ADPtrDirect $1 $2 :: AbstractDeclarator }
    | DirectAbstractDeclarator          { ADDirect $1       :: AbstractDeclarator }

DirectAbstractDeclarator :: {DirectAbstractDeclarator}
    : '(' AbstractDeclarator ')'                        { DADeclarator $2           :: DirectAbstractDeclarator }
    | DirectAbstractDeclarator '['  AssignmentExpr ']'  { Array (Just $1) (Just $3) :: DirectAbstractDeclarator }
    | DirectAbstractDeclarator '['  ']'                 { Array (Just $1) Nothing   :: DirectAbstractDeclarator }
    | '['  AssignmentExpr ']'                           { Array Nothing (Just $2)   :: DirectAbstractDeclarator }
    | '['  ']'                                          { Array Nothing Nothing     :: DirectAbstractDeclarator }
    | DirectAbstractDeclarator '[' '*' ']'              { VarArray (Just $1)        :: DirectAbstractDeclarator }
    | '[' '*' ']'                                       { VarArray Nothing          :: DirectAbstractDeclarator }
    | DirectAbstractDeclarator '(' ParameterTypeList ')'{ Parens (Just $1) $3       :: DirectAbstractDeclarator }
    | DirectAbstractDeclarator '(' ')'                  { Parens (Just $1) []       :: DirectAbstractDeclarator }
    | '(' ParameterTypeList ')'                         { Parens Nothing $2         :: DirectAbstractDeclarator }
    | '(' ')'                                           { Parens Nothing []         :: DirectAbstractDeclarator }
                            
-- page 123
TypedefName :: { Identifier }
    : typeName { $1 :: Identifier }

-- page 125
Initializer :: { Initializer }
    : AssignmentExpr                { InitExpr $1 :: Initializer }
    | '{' InitializerList '}'       { InitList (reverse $2) :: Initializer }
    | '{' InitializerList ',' '}'   { InitList (reverse $2) :: Initializer }

--InitializerList     : InitializerListI                              { reverse $1            :: [(Maybe [Designator], Initializer)] }
InitializerList :: { [(Maybe [Designator], Initializer)] }
    : Designation Initializer                       { [ (Just $1, $2) ]     :: [(Maybe [Designator], Initializer)] }
    | Initializer                                   { [ (Nothing, $1 ) ]    :: [(Maybe [Designator], Initializer)] }
    | InitializerList ',' Designation Initializer   { (Just $3, $4) : $1    :: [(Maybe [Designator], Initializer)] }
    | InitializerList ',' Initializer               { (Nothing, $3) : $1    :: [(Maybe [Designator], Initializer)] }

Designation :: { [Designator] }
    : DesignatorList '='    { reverse $1 :: [Designator]}

--DesignatorList  : DesignatorListI               { reverse $1    :: [Designator]}
DesignatorList :: {[Designator] }
    : Designator                    { [ $1 ]        :: [Designator]}
    | DesignatorList Designator    { $2 : $1       :: [Designator]}

Designator  :: { Designator }
    : '[' ConstExpr ']' { DesignatorExpr $2 :: Designator}
    | '.' ident         { DesignatorDot $2  :: Designator}

{-
-- 131
Statement :: { Statement }
    : LabeledStatement      { $1 :: Statement }
    | CompoundStatement     { (CompoundStmt $1) :: Statement }
    | ExpressionStatement   { (ExpressionStmt $1) :: Statement }
    | SelectionStatement    { $1 :: Statement }
    | IterationStatement    { $1 :: Statement }
    | JumpStatement         { $1 :: Statement }
            
LabeledStatement :: { Statement }
    : ident ':' Statement           { (LabeledStmt $1 $3) :: Statement }
    | case Expr ':' Statement       { (CaseStmt $2 $4) :: Statement }
    | default ':' Statement         { (DefaultStmt $3) :: Statement}

CompoundStatement :: { [BlockItem ] }
    : '{' BlockItemList '}' { (reverse $2) :: [BlockItem] }
    | '{' '}'               { [] }

BlockItemList :: { [BlockItem] }
    : BlockItem                 { [ $1 ] :: [BlockItem] }
    | BlockItemList BlockItem   {  ($2 : $1) :: [BlockItem] }

BlockItem :: { BlockItem }
    : Declaration       { (BDecl $1) :: BlockItem }
    | Statement         { (BStmt $1) :: BlockItem }

ExpressionStatement :: { Maybe Expr }
    : Expr  ';'     { (Just $1) :: Maybe Expr }
    | ';'           { Nothing   :: Maybe Expr }

SelectionStatement  :: { Statement }
    : if '(' Expr ')' Statement                 { IfStmt $3 $5 Nothing      :: Statement }
    | if '(' Expr ')' Statement else Statement  { IfStmt $3 $5 (Just $7)    :: Statement }
    | switch '(' Expr ')' Statement             { SwitchStmt $3 $5          :: Statement }

IterationStatement  :: { Statement }
    : while '(' Expr ')' Statement          { WhileStmt $3 $5   :: Statement}
    | do Statement while '(' Expr ')' ';'   { DoStmt $2 $5      :: Statement}
                    -- lots of opts
                    --| for '()'

JumpStatement :: {Statement }
    : goto ident ';'        { GotoStmt $2           :: Statement}
    | continue ';'          { ContinueStmt          :: Statement}
    | break ';'             { BreakStmt             :: Statement}
    | return ';'            { ReturnStmt Nothing    :: Statement}
    | return Expr ';'       { ReturnStmt (Just $2)  :: Statement}
-}

Statement :: { Statement }
    : ident ':' Statement           { (LabeledStmt $1 $3) :: Statement }
    | case Expr ':' Statement       { (CaseStmt $2 $4) :: Statement }
    | default ':' Statement         { (DefaultStmt $3) :: Statement}
    | CompoundStatement     { (CompoundStmt $1) :: Statement }
    | if '(' Expr ')' Statement                 { IfStmt $3 $5 Nothing      :: Statement }
    | if '(' Expr ')' Statement else Statement  { IfStmt $3 $5 (Just $7)    :: Statement }
    | switch '(' Expr ')' Statement             { SwitchStmt $3 $5          :: Statement }

    | while '(' Expr ')' Statement          { WhileStmt $3 $5   :: Statement}
    | do Statement while '(' Expr ')' ';'   { DoStmt $2 $5      :: Statement}
    -- | for '()'
    -- lots of opts
    | goto ident ';'        { GotoStmt $2           :: Statement}
    | continue ';'          { ContinueStmt          :: Statement}
    | break ';'             { BreakStmt             :: Statement}
    | return ';'            { ReturnStmt Nothing    :: Statement}
    | return Expr ';'       { ReturnStmt (Just $2)  :: Statement}

    | Expr  ';'     { ExpressionStmt (Just $1) :: Statement }
    | ';'           { ExpressionStmt Nothing   :: Statement }
        --ExpressionStatement   { (ExpressionStmt $1) :: Statement }


CompoundStatement :: { [BlockItem] }
    : '{' BlockItemList '}' { (reverse $2) :: [BlockItem] }

BlockItemList :: { [BlockItem] }
    : BlockItemList Declaration {  (BDecl $2 : $1) :: [BlockItem] }
    | BlockItemList Statement   {  (BStmt $2 : $1) :: [BlockItem] }
    |                           { [] :: [BlockItem ]}

-- page 140
{-
DeclarationList :: { [Declaration] }
    : Declaration                   { [ $1 ] :: [Declaration]}
    | DeclarationList  Declaration { $2 : $1 :: [Declaration]}

ExternalDeclaration :: { ExternDecl }
    : FunctionDefinition    {(FunctionDef $1)   :: ExternDecl }
    | Declaration           {(EDecl $1)         :: ExternDecl   }

FunctionDefinition :: { FunctionDefinition }
    : DeclarationSpecifiers Declarator DeclarationList CompoundStatement    {FunctionDefinition $1 $2 (Just (reverse $3)) $4 :: FunctionDefinition }
    | DeclarationSpecifiers Declarator CompoundStatement                    {FunctionDefinition $1 $2 Nothing $3             :: FunctionDefinition }
-}

DeclarationList :: { [Declaration] }
    : DeclarationList  Declaration  { $2 : $1 :: [Declaration]}
    |                               { [] :: [Declaration]}

FunctionDefinition :: { FunctionDefinition }
    : DeclarationSpecifiers Declarator DeclarationList CompoundStatement    {FunctionDefinition $1 $2 (Just (reverse $3)) $4 :: FunctionDefinition }
    --| DeclarationSpecifiers Declarator CompoundStatement                    {FunctionDefinition $1 $2 Nothing $3             :: FunctionDefinition }

ExternalDeclaration :: { ExternDecl }
    : FunctionDefinition    {(FunctionDef $1)   :: ExternDecl }
    | Declaration           {(EDecl $1)         :: ExternDecl   }

TranslationUnit :: { TranslationUnit }
    : TranslationUnitI   {(reverse $1) :: TranslationUnit}

TranslationUnitI :: { TranslationUnit }
    : ExternalDeclaration                    { [ $1 ] :: TranslationUnit }
    | TranslationUnitI ExternalDeclaration   { ($2 : $1) :: TranslationUnit }

-- page 145
-- PREPROCESSING
{-

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       Ok a     -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a     -> Ok a
      Failed e -> k e
-}


{
--parseError :: Error String :> es => (Token, [String]) -> Eff es a
parseError :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) => (Token, [String]) -> Eff es a
parseError (t, tokens) = error $ "something failed :(, failed on token: \"" ++  show t ++ "\"possible tokens: " ++ show tokens  -- throwError "failure :("





{-
lexer :: (Error String :> es, State AlexState :> es, State SymbolTable :> es) =>  (Token -> Eff es a) -> Eff es a
lexer = (preprocess >>=)
-}

--parseError :: Error String :> es => Token -> Eff es a
--parseError t = error "failure :("
}