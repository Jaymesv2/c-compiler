module Compiler.Types where

import Compiler.Parser
import Compiler.Parser.Tokens ()
import Data.Text.Internal.Fusion.Types (RS(RS0))

import Data.Map qualified as M
import Data.Set qualified as S

import GHC.Exts

{-
3 different kinds of types:
- object types: types which fully describe their objects
- function types: types that describe functions
- incomplete types: types that describe their object but lack info to determine their size

there are primitive types:
signed ints, bools, etc....

derivied types:
- Array type: characterized by the `element type` and the number of elements
- structure type:
- union type:
- function type:
- pointer type

(page 40/pdf 52)
COMPATABLE TYPES:
"two types have compatible type if their types are the same."

-}

{-
data StandardSignedInt
    = SChar
    | SShort
    | SInt
    | SLongInt
    | SLongLongInt
    deriving stock (Eq, Show)

data StandardUnsignedInt
    = Unsigned StandardSignedInt
    | UBool
    deriving stock (Eq, Show)

data StandardInt
    = SSI StandardInt
    | SUI StandardUnsignedInt
    deriving stock (Eq, Show)

data RealFloating
    = RFloat
    | RDouble
    | RLongDouble
    deriving stock (Eq, Show)

-- complex types are the same as a 2 element array (0: real part, 1: imaginary part)
newtype Complex = Complex RealFloating
    deriving stock (Eq, Show)

data FloatingType
    = Floating RealFloating
    | FComplex Complex
    deriving stock (Eq, Show)

data Basic
    = BChar
    | BSignedInt StandardInt
    | BUnsignedInt StandardUnsignedInt
    | BFloating FloatingType
    deriving stock (Eq, Show)

-- character types: char, signed char, unsigned char
-- enumerated types: named consts for ints
-- arithmetic types: {integer + floating types}
-- each arithmetic type is in a type domain.
--   the real domain comprises the real types
--   the complex comain comprises the complex types

data Void = Void
    deriving stock (Eq, Show)

-}

{-

declarations start with a storage class specifier, a type, and a list of declarators.

declarators specify an identifier or an identifier which is a pointer.

Direct declarators specify an identifier with a specific type or modifications.

Abstract declarators only specify a type name, no identifiers.

-}



newtype UniqueGen = MkUniqueGen Int deriving stock (Eq, Show, Ord)

newUniqueGen :: UniqueGen
newUniqueGen = MkUniqueGen 0


-- page 46
newtype Unique = MkUnique Int deriving stock (Eq, Show, Ord)

nextUnique :: UniqueGen -> (Unique, UniqueGen)
nextUnique (MkUniqueGen i) = (MkUnique i, MkUniqueGen (i+1))

--instance Show Unique where

--newtype LabelID = MkLabel Unique deriving stock (Eq, Show)


--newtype MemberID = MkIdent Unique deriving stock (Eq, Show)

newtype VariableID = MkVariable Unique deriving stock (Eq, Show, Ord)

nextVariableID :: UniqueGen -> (VariableID, UniqueGen)
nextVariableID (MkUniqueGen i) = (MkVariable . MkUnique $ i, MkUniqueGen (i+1))

newtype TypeID = MkType Unique deriving stock (Eq, Show, Ord)

nextTypeID :: UniqueGen -> (TypeID, UniqueGen)
nextTypeID (MkUniqueGen i) = (MkType . MkUnique $ i, MkUniqueGen (i+1))

newtype TagID = MkData Unique deriving stock (Eq, Show, Ord)





data TypeMap =
    TypeMap {
        types :: M.Map TypeID CType
    }


data TypeClassification 
    = Function
    | Object
    | Incomplete
    deriving stock (Eq, Show)



-- -- typedef isn't a valid storage class in the type system
-- data StorageClass
--     = Extern
--     | Static
--     | Auto
--     | Register
--     deriving stock (Eq, Show)

data TypeQualifiers = TypeQualifiers
    { constq :: Bool
    , restrict :: Bool
    , volatile :: Bool
    }
    deriving stock (Eq, Show)

-- mergeQualifiers :: TypeQualifiers -> TypeQualifiers -> Either 

instance Semigroup TypeQualifiers where
    (TypeQualifiers{constq = ca, restrict = ra, volatile = va})
        <> (TypeQualifiers{constq = cb, restrict = rb, volatile = vb}) =
            TypeQualifiers{constq = ca || cb, restrict = ra || rb, volatile = va || vb}

instance Monoid TypeQualifiers where
    mempty = emptyQualifier


emptyQualifier, constTypeQualifier, restrictTypeQualifier, volatileTypeQualifier :: TypeQualifiers
emptyQualifier = TypeQualifiers{constq = False, restrict = False, volatile = False}
constTypeQualifier = emptyQualifier{constq = True}
restrictTypeQualifier = emptyQualifier{restrict = True}
volatileTypeQualifier = emptyQualifier{volatile = True}






data CPrimitive 
    = CVoid
    | CBool
    | CChar         { sign :: Bool }
    | CShortInt     { sign :: Bool }
    | CInt          { sign :: Bool }
    | CLongInt      { sign :: Bool }
    | CLongLongInt  { sign :: Bool }
    | CFloat        { complex :: Bool }
    | CDouble       { complex :: Bool }
    | CLongDouble   { complex :: Bool }
    deriving stock (Eq, Show)


data CType
    = PrimTy CPrimitive
    | ArrayTy CType (Maybe Int)
    | StructTy CStruct
    | UnionTy CUnion
    | FuncTy CFunc
    | PointerTy TypeQualifiers CType
    deriving stock (Eq, Show)

data CFunc = CFunc CType [CType] Bool
    deriving stock (Eq, Show)

newtype CStruct = CStruct [(CType, Identifier, Maybe Int)]
    deriving stock (Eq, Show)

newtype CUnion = CUnion [(CType, Identifier, Maybe Int)]
    deriving stock (Eq, Show)

isCompatible :: CType -> CType -> Bool
isCompatible x y = x == y

isComplete :: CType -> Bool
isComplete ty = error ""

isIncomplete :: CType -> Bool
isIncomplete = not . isComplete





-- a non empty list of typedefs, struct/union/enum defs, identifier defs
-- data SymbolTable = SymbolTable (NonEmpty (M.Map T.Text (), M.Map T.Text (), M.Map T.Text ()))



