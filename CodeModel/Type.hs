module CodeModel.Type where

import Data.List

data SimpleType = Int | Double

data Type = Simple SimpleType | UserDefined String | FunctionType Parameter Type | ListType Type | TupleType [Type] | MapType Type Type | PointerType PtrKind Type | ConstType Type

data Parameter = Param Type | NamedParam Type String

data PtrKind = Shared | Unique | View

instance Show SimpleType where
    show Int = "Int"
    show Double = "Double"

instance Show Type where
    show (Simple t) = show t
    show (UserDefined s) = s
    show (FunctionType p r) = "(" ++ show p ++ " -> " ++ show r ++ ")"
    show (ListType t) = "[" ++ show t ++ "]"
    show (TupleType xs) = "(" ++ intercalate "," (map show xs) ++ ")"
    show (MapType l r) = "{" ++ show l ++ "," ++ show r ++ "}"
    show (PointerType p t) = show p ++ "<" ++ show t ++ ">"
    show (ConstType t) = "const<" ++ show t ++ ">"

instance Show Parameter where
    show (Param t) = show t
    show (NamedParam t n) = show t ++ " " ++ n

instance Show PtrKind where
    show Shared = "shared"
    show Unique = "unique"
    show View = "view"
