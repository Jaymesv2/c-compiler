module Compiler.AST where

import qualified Data.Text as T

type Identifier = T.Text

data Literal =
    LString T.Text
  | LChar   T.Text
  | LNum    T.Text
  | LFloat  T.Text
  deriving (Eq, Show)

data Token = 
    -- keywords
      Ident Identifier --T.Text
    | TTypeName Identifier
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

-- page
data UnaryOp = URef | UDeref | UCompliment | UNot | USizeof
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data Expr
    = EIdent Identifier
    | ELiteral Literal
    | Bracketed Expr Expr
    | Called Expr [Expr]
    | DotE Expr Identifier
    | ArrowE Expr Identifier
    | UnaryE UnaryOp Expr
    | SizeofE Expr
    | SizeofTypeE TypeName
    | CastE TypeName Expr
    | BinaryOp Expr BinOp Expr
    | AssignE Expr Expr
    | CommaE Expr Expr
    deriving (Eq, Show)

-- page 97
data Declaration = Declaration DeclarationSpecifiers (Maybe [InitDeclaration])
    deriving (Eq, Show)

-- This can probably be changed to an enum that just combines each of the specifiers and qualifiers and a nonempty list can be used in the above declaration
data DeclarationSpecifiers
    = DSStorageSpec StorageClassSpecifier DeclarationSpecifiers
    | DSTypeSpec TypeSpecifier DeclarationSpecifiers
    | DSTypeQual TypeQualifier DeclarationSpecifiers
    | DSFuncSpec FunctionSpecifier DeclarationSpecifiers
    | DSNil
    deriving (Eq, Show)

data InitDeclaration
    = UninitDeclaration Declarator
    | InitDeclaration Declarator Initializer
    deriving (Eq, Show)

-- page 114
data Declarator = Declarator (Maybe Pointer) DirectDeclarator
    deriving (Eq, Show)

data DirectDeclarator
    = DDIdent Identifier
    | DDRec Declarator
    | DDPlaceholder
    deriving (Eq, Show)

-- page 101
data DataLayoutSpec = DataLayoutSpec StructOrUnion (Maybe Identifier) (Maybe [StructDeclaration])
    deriving (Eq, Show)

data StructOrUnion = SUStruct | SUUnion
    deriving (Eq, Show)

data StructDeclaration = StructDeclaration [SpecifierQualifier] [StructDeclarator]
    deriving (Eq, Show)

type SpecifierQualifierList = [SpecifierQualifier]

type SpecifierQualifier = Either TypeSpecifier TypeQualifier

data StructDeclarator = StructDeclarator Declarator (Maybe Expr)
    deriving (Eq, Show)

data EnumSpecifier
    = EnumSpecifier (Maybe Identifier) [(Identifier, Maybe Expr)]
    | EnumForwardRef Identifier
    deriving (Eq, Show)

data Pointer = Pointer [TypeQualifier] (Maybe Pointer)
    deriving (Eq, Show)

data ParameterDeclaration
    = ParameterDeclaration DeclarationSpecifiers Declarator
    | AbsParameterDeclaration DeclarationSpecifiers AbstractDeclarator
    deriving (Eq, Show)

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
    deriving (Eq, Show)

-- page 99
data TypeSpecifier
    = PrimType PrimitiveTypes
    | StructType DataLayoutSpec
    | EnumType EnumSpecifier
    | IdentType Identifier
    deriving (Eq, Show)

-- page 108
data TypeQualifier = TQConst
    deriving (Eq, Show)

-- page 112
data FunctionSpecifier = FSInline
    deriving (Eq, Show)

data StorageClassSpecifier = SCTypedef | SCExtern | SCStatic
    deriving (Eq, Show)

data TypeName = TypeName [SpecifierQualifier] (Maybe AbstractDeclarator)
    deriving (Eq, Show)

data AbstractDeclarator
    = ADPtr Pointer
    | ADPtrDirect Pointer DirectAbstractDeclarator
    | ADDirect DirectAbstractDeclarator
    deriving (Eq, Show)

data DirectAbstractDeclarator
    = DADeclarator AbstractDeclarator
    | Array (Maybe DirectAbstractDeclarator) (Maybe Expr)
    | VarArray (Maybe DirectAbstractDeclarator)
    | Parens (Maybe DirectAbstractDeclarator) [ParameterType]
    deriving (Eq, Show)

type ParameterType = ParameterDeclaration

data Initializer
    = InitExpr Expr
    | InitList [(Maybe [Designator], Initializer)]
    deriving (Eq, Show)

data Designator
    = DesignatorExpr Expr
    | DesignatorDot Identifier
    deriving (Eq, Show)

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
    deriving (Eq, Show)

type CompoundStatement = [BlockItem]

data BlockItem
    = BDecl Declaration
    | BStmt Statement
    deriving (Eq, Show)

type TranslationUnit = [ExternDecl]

data ExternDecl
    = FunctionDef FunctionDefinition
    | EDecl Declaration
    deriving (Eq, Show)

data FunctionDefinition = FunctionDefinition DeclarationSpecifiers Declarator (Maybe DeclarationList) CompoundStatement
    deriving (Eq, Show)

type DeclarationList = [Declaration]