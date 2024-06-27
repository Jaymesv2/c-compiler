{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Compiler.Parser.ParseTree where

import Compiler.Parser.Tokens
import Compiler.Types
import Data.Text qualified as T

import Data.Functor.Foldable.TH
import Data.Functor.Foldable

import GHC.Generics

import Data.Fix

{- import Data.Bifunctor

data Attr f g a b 
    = Attr 
        { ast :: f a
        , parseTree :: f b
        } deriving stock (Eq, Show, Functor)

instance (Functor f, Functor g) => Bifunctor (Attr f g) where
    bimap f g Attr{ast=x,parseTree=y} = Attr (fmap f x) (fmap g y) -}


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
    = EIdent i
    | EConstant Constant
    | EStringLiteral T.Text
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
    deriving stock (Eq, Show) 


{- data Expr f i
    = EIdent i
    | EConstant Constant
    | EStringLiteral T.Text
    | Bracketed f f
    | Called f f
    | DotE f Identifier
    | ArrowE f Identifier
    | UnaryE UnaryOp 
    | InitE (TypeName i) [(Maybe [Designator i], Initializer i)]
    | SizeofE f
    | SizeofTypeE (TypeName i)
    | CastE (TypeName i) f 
    | BinaryOp f BinOp f
    | ConditionalExpr f f f
    | SimpleAssignE f f 
    | CompoundAssignE f AssignmentOp f
    | CommaE f f
    deriving stock (Eq, Show)
-}

-- must have at least one type specifier,
-- page 97

data Declaration i = Declaration [DeclarationSpecifiers i] [InitDeclaration i]
    deriving stock (Eq, Show)

-- data Declaration = Declaration (Maybe StorageClassSpecifier) Identifier
--     deriving stock (Eq, Show)

-- This can probably be changed to an enum that just combines each of the specifiers and qualifiers and a nonempty list can be used in the above declaration
data DeclarationSpecifiers i
    = DSStorageSpec StorageClassSpecifier
    | DSTypeSpec (TypeSpecifier i)
    | DSTypeQual TypeQualifier
    | DSFuncSpec FunctionSpecifier
    deriving stock (Eq, Show)

data InitDeclaration   i = InitDeclaration (Declarator   i) (Maybe (Initializer   i)) deriving stock (Eq, Show)

-- page 114

data Declarator i
    = DDIdent i
    | DDPointer [TypeQualifier] (Declarator i)
    | DDArr (Declarator i) Bool [TypeQualifier] (Maybe (Expr i)) Bool
    | DDFuncPList (Declarator i) [ParameterDeclaration i]
    | DDFuncIList (Declarator i) [i]
    deriving stock (Eq, Show)

-- needs to be updated
-- page 101
-- data DataLayoutSpec = DataLayoutSpec StructOrUnion (Maybe Identifier) (Maybe [StructDeclaration])
--     deriving stock (Eq, Show)

data DataLayoutSpec i
    = StructDef (Maybe Identifier) [StructDeclaration i]
    | StructRef Identifier
    | UnionDef (Maybe Identifier) [StructDeclaration i]
    | UnionRef Identifier
    deriving stock (Eq, Show)

data StructOrUnion = SUStruct | SUUnion
    deriving stock (Eq, Show)

data StructDeclaration i = StructDeclaration [SpecifierQualifier i] [StructDeclarator i]
    deriving stock (Eq, Show)

type SpecifierQualifierList t i = [SpecifierQualifier i]

type SpecifierQualifier i = Either (TypeSpecifier i) TypeQualifier

data StructDeclarator i = StructDeclarator (Declarator i) (Maybe (Expr i)) -- the element and the size
    deriving stock (Eq, Show)

data EnumSpecifier i
    = EnumSpecifier (Maybe i) [(i, Maybe (Expr i))]
    | EnumRef Identifier
    deriving stock (Eq, Show)

-- data Pointer = Pointer [TypeQualifier] (Maybe Pointer)
--     deriving stock (Eq, Show)

data ParameterDeclaration i
    = ParameterDeclaration [DeclarationSpecifiers i] (Declarator i)
    | AbsParameterDeclaration [DeclarationSpecifiers i] (Maybe (AbstractDeclarator i))
    | VariadicDeclaration
    deriving stock (Eq, Show)

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
    = PrimType PrimitiveTypes
    | StructType (DataLayoutSpec i)
    | EnumType (EnumSpecifier i)
    | IdentType Identifier
    deriving stock (Eq, Show)

-- page 108
data TypeQualifier = TQConst | TQRestrict | TQVolatile
    deriving stock (Eq, Show)

-- page 112
data FunctionSpecifier = FSInline
    deriving stock (Eq, Show)

data StorageClassSpecifier = SCTypedef | SCExtern | SCStatic | SCAuto | SCRegister
    deriving stock (Eq, Show)

data TypeName i = TypeName [SpecifierQualifier i] (Maybe (AbstractDeclarator i))
    deriving stock (Eq, Show)

data AbstractDeclarator i
    = ADPtr [TypeQualifier] (Maybe (AbstractDeclarator i))
    | Array (Maybe (AbstractDeclarator i)) (Maybe (Expr i))
    | VarArray (Maybe (AbstractDeclarator i))
    | Parens (Maybe (AbstractDeclarator i)) [ParameterType i]
    -- | ADPtrDirect Pointer (AbstractDeclarator i)
    -- | ADDirect (AbstractDeclarator i)
    -- | DADeclarator (AbstractDeclarator i)
    deriving stock (Eq, Show)


-- data AbstractDeclarator i
--     = ADPtr Pointer
--     | ADPtrDirect Pointer (DirectAbstractDeclarator i)
--     | ADDirect (DirectAbstractDeclarator i)
--     deriving stock (Eq, Show)
--
-- data DirectAbstractDeclarator i
--     = DADeclarator (AbstractDeclarator i)
--     | Array (Maybe (DirectAbstractDeclarator i)) (Maybe (Expr i))
--     | VarArray (Maybe (DirectAbstractDeclarator i))
--     | Parens (Maybe (DirectAbstractDeclarator i)) [ParameterType i]
--     deriving stock (Eq, Show)

type ParameterType i = ParameterDeclaration i

data Initializer i
    = InitExpr (Expr i)
    | InitList [(Maybe [Designator i], Initializer i)]
    deriving stock (Eq, Show)

data Designator i
    = DesignatorExpr (Expr i)
    | DesignatorDot i
    deriving stock (Eq, Show)

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
    deriving stock (Eq, Show)

newtype CompoundStatement   i = CompoundStatement [BlockItem   i]
    deriving stock (Eq, Show)

data BlockItem   i
    = BDecl (Declaration i)
    | BStmt (Statement   i)
    deriving stock (Eq, Show)

type TranslationUnit   i = [ExternDecl   i]

data ExternDecl   i
    = FunctionDef (FunctionDefinition   i)
    | EDecl (Declaration i)
    deriving stock (Eq, Show)

data FunctionDefinition   i = FunctionDefinition [DeclarationSpecifiers   i] (Declarator   i) (Maybe (DeclarationList   i)) (CompoundStatement   i )
    deriving stock (Eq, Show)

type DeclarationList   i = [Declaration   i]
