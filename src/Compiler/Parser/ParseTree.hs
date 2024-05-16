module Compiler.Parser.ParseTree where

import Compiler.Parser.Tokens
import Data.Text qualified as T

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

data Expr
    = EIdent Identifier
    | EConstant Constant
    | EStringLiteral T.Text
    | Bracketed Expr Expr
    | Called Expr [Expr]
    | DotE Expr Identifier
    | ArrowE Expr Identifier
    | UnaryE UnaryOp Expr
    | InitE TypeName [(Maybe [Designator], Initializer)]
    | SizeofE Expr
    | SizeofTypeE TypeName
    | CastE TypeName Expr
    | BinaryOp Expr BinOp Expr
    | ConditionalExpr Expr Expr Expr
    | SimpleAssignE Expr Expr
    | CompoundAssignE Expr AssignmentOp Expr
    | CommaE Expr Expr
    deriving stock (Eq, Show)

-- page 97
data Declaration = Declaration [DeclarationSpecifiers] (Maybe [InitDeclaration])
    deriving stock (Eq, Show)

-- new type
{-
data DeclSpecifiers
    = DeclSpecifiers
        (Maybe StorageClassSpecifier)
        [TypeSpecifier]
        [TypeQualifier]
-}

-- This can probably be changed to an enum that just combines each of the specifiers and qualifiers and a nonempty list can be used in the above declaration
data DeclarationSpecifiers
    = DSStorageSpec StorageClassSpecifier
    | DSTypeSpec TypeSpecifier
    | DSTypeQual TypeQualifier
    | DSFuncSpec FunctionSpecifier
    deriving stock (Eq, Show)

{-
data DeclarationSpecifiers
    = DSStorageSpec StorageClassSpecifier DeclarationSpecifiers
    | DSTypeSpec TypeSpecifier DeclarationSpecifiers
    | DSTypeQual TypeQualifier DeclarationSpecifiers
    | DSFuncSpec FunctionSpecifier DeclarationSpecifiers
    | DSNil
    deriving stock (Eq, Show)
-}

data InitDeclaration
    = UninitDeclaration Declarator
    | InitDeclaration Declarator Initializer
    deriving stock (Eq, Show)

-- page 114
data Declarator = Declarator (Maybe Pointer) DirectDeclarator
    deriving stock (Eq, Show)

data DirectDeclarator
    = DDIdent Identifier
    | DDRec Declarator
    | -- static expr pointer
      DDArr DirectDeclarator Bool [TypeQualifier] (Maybe Expr) Bool
    | DDFuncPList DirectDeclarator [ParameterDeclaration]
    | DDFuncIList DirectDeclarator [Identifier]
    | DDPlaceholder
    deriving stock (Eq, Show)

-- page 101
data DataLayoutSpec = DataLayoutSpec StructOrUnion (Maybe Identifier) (Maybe [StructDeclaration])
    deriving stock (Eq, Show)

data StructOrUnion = SUStruct | SUUnion
    deriving stock (Eq, Show)

data StructDeclaration = StructDeclaration [SpecifierQualifier] [StructDeclarator]
    deriving stock (Eq, Show)

type SpecifierQualifierList = [SpecifierQualifier]

type SpecifierQualifier = Either TypeSpecifier TypeQualifier

data StructDeclarator = StructDeclarator Declarator (Maybe Expr)
    deriving stock (Eq, Show)

data EnumSpecifier
    = EnumSpecifier (Maybe Identifier) [(Identifier, Maybe Expr)]
    | EnumForwardRef Identifier
    deriving stock (Eq, Show)

data Pointer = Pointer [TypeQualifier] (Maybe Pointer)
    deriving stock (Eq, Show)

data ParameterDeclaration
    = ParameterDeclaration [DeclarationSpecifiers] Declarator
    | AbsParameterDeclaration [DeclarationSpecifiers] (Maybe AbstractDeclarator)
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
data TypeSpecifier
    = PrimType PrimitiveTypes
    | StructType DataLayoutSpec
    | EnumType EnumSpecifier
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

data TypeName = TypeName [SpecifierQualifier] (Maybe AbstractDeclarator)
    deriving stock (Eq, Show)

data AbstractDeclarator
    = ADPtr Pointer
    | ADPtrDirect Pointer DirectAbstractDeclarator
    | ADDirect DirectAbstractDeclarator
    deriving stock (Eq, Show)

data DirectAbstractDeclarator
    = DADeclarator AbstractDeclarator
    | Array (Maybe DirectAbstractDeclarator) (Maybe Expr)
    | VarArray (Maybe DirectAbstractDeclarator)
    | Parens (Maybe DirectAbstractDeclarator) [ParameterType]
    deriving stock (Eq, Show)

type ParameterType = ParameterDeclaration

data Initializer
    = InitExpr Expr
    | InitList [(Maybe [Designator], Initializer)]
    deriving stock (Eq, Show)

data Designator
    = DesignatorExpr Expr
    | DesignatorDot Identifier
    deriving stock (Eq, Show)

data Statement
    = LabeledStmt Identifier Statement
    | CaseStmt Expr Statement
    | DefaultStmt Statement
    | CompoundStmt CompoundStatement
    | ExpressionStmt (Maybe Expr)
    | -- | SelectionStmt
      IfStmt Expr Statement (Maybe Statement)
    | SwitchStmt Expr Statement
    | -- | IterationStmt
      WhileStmt Expr Statement
    | DoStmt Statement Expr
    | ForStmt (Maybe Expr) (Maybe Expr) (Maybe Expr) Statement
    | ForDeclStmt Declaration (Maybe Expr) (Maybe Expr) Statement
    | -- | JumpStmt
      GotoStmt Identifier
    | ContinueStmt
    | BreakStmt
    | ReturnStmt (Maybe Expr)
    deriving stock (Eq, Show)

type CompoundStatement = [BlockItem]

data BlockItem
    = BDecl Declaration
    | BStmt Statement
    deriving stock (Eq, Show)

type TranslationUnit = [ExternDecl]

data ExternDecl
    = FunctionDef FunctionDefinition
    | EDecl Declaration
    deriving stock (Eq, Show)

data FunctionDefinition = FunctionDefinition [DeclarationSpecifiers] Declarator (Maybe DeclarationList) CompoundStatement
    deriving stock (Eq, Show)

type DeclarationList = [Declaration]