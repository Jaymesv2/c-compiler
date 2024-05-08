{
module Compiler.Parser.Grammar where

import Compiler.Parser.Lexer

import Compiler.Parser.ParseTree
import Compiler.Parser.Tokens

--data ParseError = ParseError
}

%name clike TranslationUnit
%error {parseError}
%monad {Alex} {>>=} {return }
%lexer {(alexMonadScan >>=)} {EOF}
%tokentype { Token }

%token
    ident   { Ident $$ }
    typeName{ TTypeName $$ }
    stringlit { StringLiteral $$ }
    constant  { Constant $$ }

    auto    { Auto  }
    break   { Break }
    case    { Case }
    const   { Const }
    continue{ Continue }
    default { Default }
    do      { Do }
    else    { Else }
    extern  { Extern }
    enum    { Enum }
    for     { For }
    goto    { Goto}
    if      { If }
    inline  { Inline }
    register{ Register }
    restrict{ Restrict }
    return  { Return }
    sizeof  { Sizeof}
    static  { Static }
    struct  { Struct}
    switch  { Switch }
    typedef { TypeDef }
    union   { Union }
    volatile{ Volatile}
    while   { While}

    void    { Void }
    char    {  TChar}
    short   {  TShort}
    int     {  TInt}
    long    {  TLong}
    float   {  TFloat}
    double  {  TDouble}
    signed  {  TSigned}
    unsigned{  TUnsigned}
    uBool   {  TuBool}
    uComplex {  TuComplex}
    uImaginary {  TuImaginary}
    '{'     { LBrace }
    '}'     { RBrace }
    '('     { LParen }
    ')'     { RParen }
    '['     { LBrack }
    ']'     { RBrack }
    '->'    { Arrow  }
    '&'     { BitAnd }
    '|'     { BitOr   }
    '*'     { Times }
    '+'     { Plus }
    '-'     { Minus }
    '~'     { Compliment }
    '!'     { Not }
    '/'     { Divide }
    '%'     { Modulo }
    '<<'    { LShift } 
    '>>'    { RShift } 
    '<'     { Lt     }
    '<='    { Le     }
    '>'     { Gt     }
    '>='    { Ge     }
    '=='    { Eq     }
    '!='    { Neq    }
    '^'     { BitXor }
    '&&'    { LAnd   }
    '||'    { LOr    }
    ';'     { Semi   }
    '='     { Assign }
    ','     { Comma  }
    '.'     { Dot }
    ':'     { Colon}


%name expr Expr
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


%%

TranslationUnit     : TranslationUnitI   {(reverse $1) :: TranslationUnit}
TranslationUnitI    : ExternalDeclaration                    { [ $1 ] :: TranslationUnit }
                    | TranslationUnitI ExternalDeclaration   { ($2 : $1) :: TranslationUnit }

PrimaryExpr : ident         { (EIdent $1) :: Expr }
            | constant      { (EConstant $1) :: Expr}
            | stringlit       { (EStringLiteral $1) :: Expr }
            | '(' Expr ')'  { $2 :: Expr }


PostfixExpr : PrimaryExpr               { $1 :: Expr }
            | PostfixExpr '[' Expr ']'  { (Bracketed $1 $3) :: Expr }

            -- prob a better way to do this 
            | PostfixExpr '(' ')'               { (Called $1 []) :: Expr }
            | PostfixExpr '(' ArgExprList ')'   { (Called $1 ( reverse $3 )) :: Expr }

            | PostfixExpr '.' ident             { (DotE $1 $3) :: Expr }
            | PostfixExpr '->' ident            { (ArrowE $1 $3) :: Expr }
            -- evil type casts :/
            --| '(' ident ')' '{' InitializerList '}'
            --| '(' ident ')' '{' InitializerList ',' '}'

UnaryOp     : '&' { URef }
            | '*' { UDeref }
            | '~' { UCompliment }
            | '!' { UNot }

UnaryExpr   : PostfixExpr               { $1 :: Expr }
            | UnaryOp CastExpr         { UnaryE $1 $2 }
            | sizeof UnaryExpr          { UnaryE USizeof $2 }
            | sizeof '(' TypeName ')'      { SizeofTypeE $3 }

CastExpr    : UnaryExpr                 { $1 :: Expr }
            | '(' TypeName ')' CastExpr    { CastE $2 $4 }

MultiplicativeExpr  : CastExpr                          { $1 :: Expr } 
                    | MultiplicativeExpr '*' CastExpr   { BinaryOp $1 BMul $3 }
                    | MultiplicativeExpr '/' CastExpr   { BinaryOp $1 BDiv $3 }
                    | MultiplicativeExpr '%' CastExpr   { BinaryOp $1 BMod $3 }

AdditiveExpr    : MultiplicativeExpr                    { $1 :: Expr }
                | AdditiveExpr '+' MultiplicativeExpr   { BinaryOp $1 BAdd $3 }
                | AdditiveExpr '-' MultiplicativeExpr   { BinaryOp $1 BSub $3 }

ShiftExpr   : AdditiveExpr                              { $1 :: Expr }
            | ShiftExpr '<<' AdditiveExpr               { BinaryOp $1 BShiftL $3}
            | ShiftExpr '>>' AdditiveExpr               { BinaryOp $1 BShiftR $3}

RelationalExpr  : ShiftExpr                             { $1 :: Expr}
                | RelationalExpr '<' ShiftExpr          { BinaryOp $1 Blt $3 }
                | RelationalExpr '>' ShiftExpr          { BinaryOp $1 Bgt $3 }
                | RelationalExpr '<=' ShiftExpr         { BinaryOp $1 Ble $3 }
                | RelationalExpr '>=' ShiftExpr         { BinaryOp $1 Bge $3 }

EqualityExpr    : RelationalExpr                        { $1 ::Expr }
                | EqualityExpr '==' RelationalExpr      { BinaryOp $1 Beq $3 }
                | EqualityExpr '!=' RelationalExpr      { BinaryOp $1 Bneq $3 }

AndExpr : EqualityExpr                                  { $1 :: Expr }
        | AndExpr '&' EqualityExpr                      {BinaryOp $1 BBitAnd $3 }

XorExpr : AndExpr                                       { $1 :: Expr }
        | XorExpr '^' AndExpr                           {BinaryOp $1 BBitXor $3 }

OrExpr  : XorExpr                                       { $1 :: Expr}
        | OrExpr '|' XorExpr                            {BinaryOp $1 BBitOr $3 }

-- Expr
LAndExpr    : OrExpr                { $1 :: Expr }
            | LAndExpr  '&&' OrExpr { (BinaryOp $1 BLogicalAnd $3) :: Expr }

-- Expr
LOrExpr     : LAndExpr                  { $1 :: Expr }
            | LOrExpr '||' LAndExpr     { (BinaryOp $1 BLogicalOr $3) :: Expr }

-- could add ternary operator here
ConditionalExpr : LOrExpr           { $1 :: Expr }

-- Expr
AssignmentExpr  : ConditionalExpr                       { ($1 :: Expr) }
                | AssignmentExpr '=' ConditionalExpr    { (AssignE $1 $3) :: Expr }

-- Gives [ Expr ]   WILL BE REVERSED
ArgExprList : AssignmentExpr                        { [ $1 ] }
            | ArgExprList ',' AssignmentExpr        { $3 : $1 }
            --| {- empty -}       { [] }

Expr    : AssignmentExpr { $1 }
        | Expr ',' AssignmentExpr { CommaE $1 $3 }


-- should be evaluated at translation time
-- no increment, decrement, function calls, comma ops, unless they are in an unevaluated subexpression
ConstExpr   : ConditionalExpr   { $1 }

Declaration : DeclarationSpecifiers InitDeclarationList ';' { (Declaration ($1 :: [DeclarationSpecifiers]) (Just ((reverse $2) :: [InitDeclaration]))) :: Declaration }
            | DeclarationSpecifiers ';'                     { (Declaration ($1 :: [DeclarationSpecifiers]) Nothing ) :: Declaration }

-- page 97

-- Declarations 


DeclarationSpecifiers   : StorageClassSpecifier DeclarationSpecifiers   { ((DSStorageSpec $1):$2) :: [DeclarationSpecifiers] }
                        | StorageClassSpecifier                         { ([DSStorageSpec $1]   ) :: [DeclarationSpecifiers] }
                        | TypeSpecifier DeclarationSpecifiers           { ((DSTypeSpec $1):$2   ) :: [DeclarationSpecifiers] }
                        | TypeSpecifier                                 { ([DSTypeSpec $1]      ) :: [DeclarationSpecifiers] }
                        | TypeQualifier DeclarationSpecifiers           { ((DSTypeQual $1):$2   ) :: [DeclarationSpecifiers] }
                        | TypeQualifier                                 { ([DSTypeQual $1]      ) :: [DeclarationSpecifiers] }
                        | FunctionSpecifier DeclarationSpecifiers       { ((DSFuncSpec $1):$2   ) :: [DeclarationSpecifiers] }
                        | FunctionSpecifier                             { ([DSFuncSpec $1]      ) :: [DeclarationSpecifiers] }


InitDeclarationList : InitDeclarator { [ $1 ] } 
                    | InitDeclarationList ',' InitDeclarator { $3 : $1 }

InitDeclarator  : Declarator                    { UninitDeclaration $1 }
                | Declarator '=' Initializer    { InitDeclaration $1 $3 }


-- page 98
StorageClassSpecifier   : typedef   { SCTypedef }
                        | extern    { SCExtern }
                        | static    { SCStatic }
                        | auto      { SCAuto }
                        | register  { SCRegister}

    -- this needs to have the standard types and stuf
-- page 99
TypeSpecifier  : void           { PrimType PVoid }
                | char          { PrimType PChar }
                | short         { PrimType PShort }
                | int           { PrimType PInt }
                | long          { PrimType PLong }
                | float         { PrimType PFloat }
                | double        { PrimType PDouble }
                | signed        { PrimType PSigned }
                | unsigned      { PrimType PUnsigned }
                | uBool         { PrimType PuBool }
                | uComplex      { PrimType PuComplex }
                | uImaginary    { PrimType PuImaginary }
                | StructOrUnionSpecifier    {StructType $1 }
                | EnumSpecifier              {EnumType $1 }
                | TypedefName       { IdentType $1 }
-- page 108
TypeQualifier   : const         { TQConst }
                | restrict      { TQRestrict }
                | volatile      { TQVolatile }

-- page 112
FunctionSpecifier   : inline    { FSInline }
-- page 101
-- might merge these two rules into 1 with 6 rules
StructOrUnionSpecifier  : StructOrUnion ident '{' StructDeclarationList '}' { DataLayoutSpec ($1 :: StructOrUnion) (Just ($2 :: Identifier)) (Just ((reverse $4) :: [StructDeclaration])) }
                        | StructOrUnion  '{' StructDeclarationList '}'      { DataLayoutSpec ($1 :: StructOrUnion) Nothing (Just (reverse $3)) }
                        | StructOrUnion ident                               { DataLayoutSpec ($1 :: StructOrUnion) (Just ($2 :: Identifier)) Nothing }

StructOrUnion           : struct    { SUStruct }
                        | union     { SUUnion }


StructDeclarationList   : StructDeclaration                         { [ $1 ] :: [StructDeclaration]  }
                        | StructDeclarationList StructDeclaration   { ($2 : $1) :: [StructDeclaration] }

--StructDeclaration       : SpecifierQualifierList StructDeclarator { StructDeclaration $1 ([$2] :: [StructDeclarator]) }


StructDeclaration       : SpecifierQualifierList StructDeclaratorList ';' { StructDeclaration $1 (reverse $2 :: [StructDeclarator]) }

StructDeclaratorList    : StructDeclarator                              { [ $1 ] :: [StructDeclarator] }
                        | StructDeclaratorList ',' StructDeclarator   { ($3 : $1) :: [StructDeclarator] }


SpecifierQualifierList  : SpecifierQualifierListI { (reverse $1) :: [SpecifierQualifier] }

SpecifierQualifierListI : TypeSpecifier SpecifierQualifierListI { (Left $1) : $2 }
                        | TypeQualifier SpecifierQualifierListI { (Right $1) : $2 }
                        | TypeSpecifier { [ Left $1 ] }
                        | TypeQualifier { [ Right $1 ] }

-- unfinished
StructDeclarator        : Declarator            { (StructDeclarator ($1 :: Declarator) Nothing) :: StructDeclarator }
                        | Declarator ':' Expr   { (StructDeclarator ($1 :: Declarator) (Just ($3 :: Expr))) :: StructDeclarator }
                        --| ':' Expr   { (StructDeclarator ($1 :: Declarator) (Just ($3 :: Expr))) :: StructDeclarator }
                        -- I could support bitfields but I dont really want to :/

-- page 104
EnumSpecifier   : enum ident '{' EnumeratorList '}'     { EnumSpecifier (Just $2) $4 }
                | enum  '{' EnumeratorList '}'          { EnumSpecifier Nothing $3 }
                | enum ident '{' EnumeratorList ',' '}' { EnumSpecifier (Just $2) $4 }
                | enum '{' EnumeratorList ',' '}'       { EnumSpecifier Nothing $3 }
                | enum ident                            { EnumForwardRef $2  }

-- little hack for the list building
EnumeratorList  :   EnumeratorListI { reverse $1 }

EnumeratorListI : Enumerator                        { [ $1 ]}
                | EnumeratorListI ',' Enumerator    { $3 : $1 }

Enumerator      : EnumerationConstant             { ($1, Nothing) }
                | EnumerationConstant '=' Expr    { ($1, Just $3) }
                                    -- this needs to be a "constant" expression

EnumerationConstant : ident { $1 }

-- Page 114
Declarator  : Pointer DirectDeclarator      { Declarator (Just $1) $2 }
            | DirectDeclarator              { Declarator Nothing $1 }

DirectDeclarator        : ident                                                             { DDIdent $1 }
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
                        | '(' Declarator ')'                                                { DDRec $2 }
                        | DirectDeclarator '(' ParameterTypeList ')'                        { ( DDFuncPList   $1 $3  ) :: DirectDeclarator }     
                        | DirectDeclarator '(' IdentifierList ')'                           { ( DDFuncIList $1 $3) :: DirectDeclarator }    
                        | DirectDeclarator '(' ')'                                          { ( DDFuncIList $1 []) :: DirectDeclarator }   
                                                                            
                                                                            
-- declarators
Pointer                 : '*' TypeQualifierList Pointer { (Pointer $2 (Just $3)) :: Pointer }
                        | '*' TypeQualifierList         { (Pointer $2 Nothing) :: Pointer }
                        | '*' Pointer                   { (Pointer [] (Just $2)) :: Pointer}
                        | '*'                           { (Pointer [] Nothing) :: Pointer }

TypeQualifierList       : TypeQualifierListI { reverse $1 }
TypeQualifierListI      : TypeQualifier { [ $1 ] }
                        | TypeQualifierListI TypeQualifier { $2 : $1 }

ParameterTypeList       : ParameterList                 { reverse $1 }
--                      | ParameterList ',' '...'       {  }

ParameterList           : ParameterDeclaration                      { [ $1 ] }
                        | ParameterList ',' ParameterDeclaration    { $3 : $1 }

ParameterDeclaration    : DeclarationSpecifiers Declarator          { (ParameterDeclaration $1 $2 ) :: ParameterDeclaration}
                        | DeclarationSpecifiers AbstractDeclarator  { (AbsParameterDeclaration $1 $2 ) :: ParameterDeclaration}

IdentifierList          : IdentifierListI           { reverse $1 }
IdentifierListI         : ident                     { [ $1 ] }
                        | IdentifierListI ',' ident { $3 : $1 }




-- page 122
TypeName    : SpecifierQualifierList AbstractDeclarator {TypeName $1 (Just $2)}
            | SpecifierQualifierList                    {TypeName $1 Nothing}

AbstractDeclarator  : Pointer                           { ADPtr $1 }
                    | Pointer DirectAbstractDeclarator  { ADPtrDirect $1 $2 }
                    | DirectAbstractDeclarator          { ADDirect $1 }

DirectAbstractDeclarator    : '(' AbstractDeclarator ')'                        {DADeclarator $2 }
                            | DirectAbstractDeclarator '['  AssignmentExpr ']'  {Array (Just $1) (Just $3)}
                            | DirectAbstractDeclarator '['  ']'                 {Array (Just $1) Nothing}
                            | '['  AssignmentExpr ']'                           {Array Nothing (Just $2)}
                            | '['  ']'                                          {Array Nothing Nothing}
                            | DirectAbstractDeclarator '[' '*' ']'              {VarArray (Just $1)}
                            | '[' '*' ']'                                       {VarArray Nothing}
                            | DirectAbstractDeclarator '(' ParameterTypeList ')'{Parens (Just $1) $3}
                            | DirectAbstractDeclarator '(' ')'                  {Parens (Just $1) []}
                            | '(' ParameterTypeList ')'                         {Parens Nothing $2}
                            | '(' ')'                                           {Parens Nothing []}
                            
-- page 123
TypedefName : typeName { $1 }

-- page 125
Initializer : AssignmentExpr                { InitExpr $1 }
            | '{' InitializerList '}'       { InitList $2 }
            | '{' InitializerList ',' '}'   { InitList $2 }

InitializerList     : InitializerListI                              { reverse $1 }
InitializerListI    : Designation Initializer                       { [ (Just $1, $2) ] }
                    | Initializer                                   { [ (Nothing, $1 ) ] }
                    | InitializerListI ',' Designation Initializer  { (Just $3, $4) : $1 }
                    | InitializerListI ',' Initializer              { (Nothing, $3) : $1 }

Designation     : DesignatorList '='    { $1 }

DesignatorList  : DesignatorListI               { reverse $1 }
DesignatorListI : Designator                    { [ $1 ] }
                | DesignatorListI Designator    { $2 : $1 }

Designator  : '[' ConstExpr ']'  { DesignatorExpr $2 }
            | '.' ident     { DesignatorDot $2 }


-- 131
Statement   : LabeledStatement      { $1 :: Statement }
            | CompoundStatement     { (CompoundStmt $1) :: Statement }
            | ExpressionStatement   { (ExpressionStmt $1) :: Statement }
            | SelectionStatement    { $1 :: Statement }
            | IterationStatement    { $1 :: Statement }
            | JumpStatement         { $1 :: Statement }
            
LabeledStatement    : ident ':' Statement           { (LabeledStmt $1 $3) :: Statement }
                    | case Expr ':' Statement       { (CaseStmt $2 $4) :: Statement }
                    | default ':' Statement         { (DefaultStmt $3) :: Statement}

CompoundStatement   : '{' BlockItemList '}' { (reverse $2) :: [BlockItem] }
                    | '{' '}'               { [] }

BlockItemList       : BlockItem                 { [ $1 ] :: [BlockItem] }
                    | BlockItemList BlockItem   {  ($2 : $1) :: [BlockItem] }

BlockItem           : Declaration       { (BDecl $1) :: BlockItem }
                    | Statement         { (BStmt $1) :: BlockItem }

ExpressionStatement : Expr  ';'     { (Just $1) :: Maybe Expr }
                    | ';'           { Nothing }

SelectionStatement  : if '(' Expr ')' Statement                 { IfStmt $3 $5 Nothing }
                    | if '(' Expr ')' Statement else Statement  { IfStmt $3 $5 (Just $7) }
                    | switch '(' Expr ')' Statement             { SwitchStmt $3 $5 }

IterationStatement  : while '(' Expr ')' Statement          { WhileStmt $3 $5 }
                    | do Statement while '(' Expr ')' ';'   { DoStmt $2 $5 }
                    -- lots of opts
                    --| for '()'

JumpStatement       : goto ident ';'        { GotoStmt $2 }
                    | continue ';'          { ContinueStmt }
                    | break ';'             { BreakStmt}
                    | return ';'            { ReturnStmt Nothing }
                    | return Expr ';'       { ReturnStmt (Just $2) }

-- page 140




DeclarationList     : Declaration                   { [ $1 ] }
                    | DeclarationList  Declaration { $2 : $1 }

ExternalDeclaration : FunctionDefinition    {(FunctionDef $1) :: ExternDecl }
                    | Declaration           {(EDecl $1) :: ExternDecl   }

FunctionDefinition  : DeclarationSpecifiers Declarator DeclarationList CompoundStatement {FunctionDefinition $1 $2 (Just (reverse $3)) $4}
                    | DeclarationSpecifiers Declarator CompoundStatement    {FunctionDefinition $1 $2 Nothing $3}

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
parseError :: Token -> Alex a
parseError t = error "failure :("
}