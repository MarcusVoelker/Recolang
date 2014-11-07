module CodeModel.Function where

import CodeModel.Signature

data Function = Function Signature [Statement]

data Statement = VarDefinition String Statement | Var String

instance Show Function where
	show (Function sig body) = "    " ++ show sig ++ "\n" ++ unlines (map (\s -> "        "++ show s) body)

instance Show Statement where
	show (VarDefinition n s) = n ++ " := " ++ show s
	show (Var n) = n