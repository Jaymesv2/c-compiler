{-# LANGUAGE TemplateHaskell #-}

module Compiler.Parser.ParseTree where
import Compiler.Parser.SrcLoc (Located, SrcSpan)
import Compiler.Parser.Tokens
import Compiler.Parser


import Data.Functor.Foldable.TH

-- import Compiler.Types
import Data.Text qualified as T




{-

    The parse tree is meant to match the syntax of the provided program.

-}

-- page
data UnaryOp = URef | UDeref | UCompliment | UNot | USizeof | UPreIncr | UPostIncr | UPreDecr | UPostDecr | UPlus | UMinus
    deriving stock (Eq, Show) 

data BinOp
    = BMul
    | BDiv
    | BMod
    | BAdd
    | BSub
    | BShiftL
    | BShiftR
    | BBitAnd
    | BBitXor
    | BBitOr
    | Blt
    | Bgt
    | Ble
    | Bge
    | Beq
    | Bneq
    | BLogicalAnd
    | BLogicalOr
    deriving stock (Eq, Show)


data AssignmentOp = ATimesAssign | ADivAssign | AModAssign | APlusAssign | AMinusAssign | ALShiftAssign | ARShiftAssign | AAndAssign | AXorAssign | AOrAssign
    deriving stock (Eq, Show)


data Expr i
    = EIdent SrcSpan i
    | EConstant SrcSpan Constant
    | Bracketed (Expr i) (Expr i)
    | Called (Expr i) [Expr i]
    | DotE (Expr i) Identifier
    | ArrowE (Expr i) Identifier
    | UnaryE UnaryOp (Expr i)
    | InitE (TypeName i) [(Maybe [Designator i], Initializer i)]
    | SizeofE (Expr i)
    | SizeofTypeE (TypeName i)
    | CastE (TypeName i) (Expr i)
    | BinaryOp (Expr i) BinOp (Expr i)
    | ConditionalExpr (Expr i) (Expr i) (Expr i)
    | SimpleAssignE (Expr i) (Expr i)
    | CompoundAssignE (Expr i) AssignmentOp (Expr i)
    | CommaE (Expr i) (Expr i)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 


-- must have at least one type specifier,
-- page 97

data Declaration i = Declaration [DeclarationSpecifier i] [InitDeclaration i]
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

-- This can probably be changed to an enum that just combines each of the specifiers and qualifiers and a nonempty list can be used in the above declaration
data DeclarationSpecifier i
    = DSStorageSpec SrcSpan StorageClassSpecifier
    | DSTypeSpec (TypeSpecifier i)
    | DSTypeQual SrcSpan TypeQualifier
    | DSFuncSpec SrcSpan FunctionSpecifier
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data InitDeclaration   i = InitDeclaration (Declarator   i) (Maybe (Initializer   i)) 
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

-- page 114

data Declarator i
    = DDIdent i
    | DDPointer [Located TypeQualifier] (Declarator i)
    | DDArr (Declarator i) Bool [Located TypeQualifier] (Maybe (Expr i)) Bool
    | DDFuncPList (Declarator i) [ParameterDeclaration i]
    | DDFuncIList (Declarator i) [i]
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 


-- needs to be updat ffed
-- page 101
-- data DataLayoutSpec = DataLayoutSpec StructOrUnion (Maybe Identifier) (Maybe [StructDeclaration]) 
--     deriving stock (Eq, Show)

data DataLayoutSpec i
    = StructDef SrcSpan (Maybe (Located i)) [StructDeclaration i]
    | StructRef SrcSpan (Located i)
    | UnionDef SrcSpan (Maybe (Located i)) [StructDeclaration i]
    | UnionRef SrcSpan (Located i)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data StructOrUnion = SUStruct | SUUnion
    deriving stock (Eq, Show)

data StructDeclaration i = StructDeclaration [SpecifierQualifier i] [StructDeclarator i]
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

--type SpecifierQualifierList t i = [SpecifierQualifier i]

type SpecifierQualifier i = Either (Located TypeQualifier) (TypeSpecifier i)

data StructDeclarator i = StructDeclarator (Declarator i) (Maybe (Expr i)) -- the element and the size
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data EnumSpecifier i
    = EnumSpecifier (Maybe (Located i)) [(Located i, Maybe (Expr i))]
    | EnumRef (Located Identifier)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

-- data Pointer = Pointer [TypeQualifier] (Maybe Pointer)
--     deriving stock (Eq, Show)

data ParameterDeclaration i
    = ParameterDeclaration [DeclarationSpecifier i] (Declarator i)
    | AbsParameterDeclaration [DeclarationSpecifier i] (Maybe (AbstractDeclarator i))
    | VariadicDeclaration
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 



data PrimitiveTypes
    = PVoid
    | PChar
    | PShort
    | PInt
    | PLong
    | PFloat
    | PDouble
    | PSigned
    | PUnsigned -- signed / unsigned
    | PuBool
    | PuComplex
    | PuImaginary -- u for underscore
    deriving stock (Eq, Show)


-- page 99
data TypeSpecifier i
    = PrimType (Located PrimitiveTypes)
    | StructType (DataLayoutSpec i)
    | EnumType (EnumSpecifier i)
    | IdentType (Located Identifier)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

-- page 108
data TypeQualifier = TQConst | TQRestrict | TQVolatile
    deriving stock (Eq, Show)

-- page 112
data FunctionSpecifier = FSInline
    deriving stock (Eq, Show)

data StorageClassSpecifier = SCTypedef | SCExtern | SCStatic | SCAuto | SCRegister
    deriving stock (Eq, Show)

data TypeName i = TypeName [SpecifierQualifier i] (Maybe (AbstractDeclarator i))
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data AbstractDeclarator i
    = ADPtr [Located TypeQualifier] (Maybe (AbstractDeclarator i))
    | Array (Maybe (AbstractDeclarator i)) (Maybe (Expr i))
    | VarArray (Maybe (AbstractDeclarator i))
    | Parens (Maybe (AbstractDeclarator i)) [ParameterType i]
    -- | ADPtrDirect Pointer (AbstractDeclarator i)
    -- | ADDirect (AbstractDeclarator i)
    -- | DADeclarator (AbstractDeclarator i)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 


type ParameterType i = ParameterDeclaration i

data Initializer i
    = InitExpr (Expr i)
    | InitList [(Maybe [Designator i], Initializer i)]
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data Designator i
    = DesignatorExpr (Expr i)
    | DesignatorDot i
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data Statement i
    = LabeledStmt i (Statement i)
    | CaseStmt (Expr i) (Statement i)
    | DefaultStmt (Statement i)
    | CompoundStmt (CompoundStatement i)
    | ExpressionStmt (Maybe (Expr i))
    | -- | SelectionStmt
      IfStmt (Expr i) (Statement i) (Maybe (Statement i))
    | SwitchStmt (Expr i) (Statement i)
    | -- | IterationStmt
      WhileStmt (Expr i) (Statement i)
    | DoStmt (Statement i) (Expr i)
    | ForStmt (Maybe (Expr i)) (Maybe (Expr i)) (Maybe (Expr i)) (Statement i)
    | ForDeclStmt (Declaration i) (Maybe (Expr i)) (Maybe (Expr i)) (Statement i)
    | -- | JumpStmt
      GotoStmt i
    | ContinueStmt
    | BreakStmt
    | ReturnStmt (Maybe (Expr   i))
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

newtype CompoundStatement   i = CompoundStatement [BlockItem   i]
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

data BlockItem   i
    = BDecl (Declaration i)
    | BStmt (Statement   i)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

type TranslationUnit   i = [ExternDecl   i]

--type ExternDecl i = Either (FunctionDefinition i) (Declaration i)

data FunctionDefinition   i = FunctionDefinition [DeclarationSpecifier i] (Declarator   i) (Maybe (DeclarationList   i)) (CompoundStatement   i )
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 

type DeclarationList   i = [Declaration   i]


data ExternDecl   i
    = EFunctionDef (FunctionDefinition   i)
    | EDecl (Declaration i)
    deriving stock (Eq, Show, Functor, Foldable, Traversable) 






makeBaseFunctor ''AbstractDeclarator
makeBaseFunctor ''Declarator
makeBaseFunctor ''Expr
makeBaseFunctor ''Statement

-- makeBaseFunctor ''InitDeclaration
{- Expr i
Declaration i = Declaration [DeclarationSpecifiers i] [InitDeclaration i]
DeclarationSpecifiers i
    = DSStorageSpec StorageClassSpecifier
    | DSTypeSpec (TypeSpecifier i)
    | DSTypeQual TypeQualifier
    | DSFuncSpec FunctionSpecifier

InitDeclaration   i = InitDeclaration (Declarator   i) (Maybe (Initializer   i)) 
DataLayoutSpec i
    = StructDef (Maybe i) [StructDeclaration i]
    | StructRef i
    | UnionDef (Maybe i) [StructDeclaration i]
    | UnionRef i
StructDeclarator i = StructDeclarator (Declarator i) (Maybe (Expr i)) -- the element and the size
EnumSpecifier i
ParameterDeclaration i
    = ParameterDeclaration [DeclarationSpecifiers i] (Declarator i)
    | AbsParameterDeclaration [DeclarationSpecifiers i] (Maybe (AbstractDeclarator i))
    | VariadicDeclaration
TypeSpecifier i
Declarator i
TypeName i
AbstractDeclarator i
Initializer i
Designator i
Statement i -}
