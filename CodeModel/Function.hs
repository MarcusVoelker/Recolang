module CodeModel.Function where

import CodeModel.Signature

data Function = Function Signature Block

type Block = [Statement]

data Statement = VarDefinition String Statement
               | Var String
               | IntLiteral Int
               | StringLiteral String
               | CharLiteral Char
               | FloatLiteral Double
               | ArrayLiteral [Statement]
               | MapLiteral [(Statement,Statement)]
               | TupleLiteral [Statement]
               | For Statement Statement Statement Block
               | RangeFor String Statement Block
               | While Statement Block
               | If Statement Block Block
               | DoWhile Statement Block

instance Show Function where
	show (Function sig body) = "    " ++ show sig ++ "\n" ++ unlines (map (\s -> "        "++ show s) body)

instance Show Statement where
	show (VarDefinition n s) = n ++ " := " ++ show s
	show (Var n) = n
