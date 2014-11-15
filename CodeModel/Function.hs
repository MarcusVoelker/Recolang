module CodeModel.Function where

import Data.List

import CodeModel.Signature

data Function = Function Signature Block

type Block = [Statement]

data Statement = Identifier String
               | TemplatedIdentifier String Statement
               | IntLiteral Integer
               | StringLiteral String
               | CharLiteral Char
               | FloatLiteral Double
               | ArrayLiteral [Statement]
               | TupleLiteral [Statement]
               | MapLiteral [(Statement,Statement)]
               | Operator String Statement Statement
               | For String Statement Block
               | While Statement Block
               | If Statement Block
               | IfE Statement Block Block
               | Return Statement
               | Call Statement Statement

instance Show Function where
	show (Function sig body) = "   " ++ show sig ++ "\n" ++ showBlock 2 body

showBlock :: Int -> Block -> String
showBlock i = unlines . map (showIndentedStatement i)

showIndentedStatement :: Int -> Statement -> String
showIndentedStatement i s = replicate (i*3) ' ' ++ showStatement i s

showStatement :: Int -> Statement -> String
showStatement _ (Identifier n) = n
showStatement _ (TemplatedIdentifier n t) = n ++ "(" ++ show t ++ ")"
showStatement _ (IntLiteral l) = show l
showStatement _ (StringLiteral s) = show s
showStatement _ (CharLiteral c) = show c
showStatement _ (FloatLiteral f) = show f
showStatement _ (ArrayLiteral a) = show a
showStatement _ (MapLiteral ms) = "{" ++ intercalate "," (map show ms) ++ "}"
showStatement _ (TupleLiteral ts) = "(" ++ intercalate "," (map show ts) ++ ")"
showStatement _ (Operator op l r) = "(" ++ show l ++ " " ++ op ++ " " ++ show r ++ ")"
showStatement i (For s r b) = "for " ++ s ++ " <- " ++ show r ++ "\n" ++ showBlock (i+1) b
showStatement i (While c b) = "while " ++ show c ++ "\n" ++ showBlock (i+1) b
showStatement i (If s b) = "if " ++ show s ++ "\n" ++ showBlock (i+1) b
showStatement i (IfE s b e) = "if " ++ show s ++ "\n" ++ showBlock (i+1) b ++ replicate (i*3) ' ' ++ "else\n" ++ showBlock (i+1) e
showStatement _ (Return s) = "return " ++ show s
showStatement _ (Call s1 s2) = show s1 ++ " " ++ show s2

instance Show Statement where
   show = showStatement 0
