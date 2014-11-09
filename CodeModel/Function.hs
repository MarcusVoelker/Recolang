module CodeModel.Function where

import Data.List

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
               | TupleLiteral [Statement]
               | MapLiteral [(Statement,Statement)]
               | Assignment Statement Statement
               | For String Statement Block
               | While Statement Block
               | If Statement Block
               | IfE Statement Block Block

instance Show Function where
	show (Function sig body) = "   " ++ show sig ++ "\n" ++ showBlock 2 body

showBlock :: Int -> Block -> String
showBlock i = unlines . map (showIndentedStatement i)

showIndentedStatement :: Int -> Statement -> String
showIndentedStatement i s = replicate (i*3) ' ' ++ showStatement i s

showStatement :: Int -> Statement -> String
showStatement i (VarDefinition n s) = n ++ " := " ++ show s
showStatement i (Var n) = n
showStatement i (IntLiteral l) = show l
showStatement i (StringLiteral s) = show s
showStatement i (CharLiteral c) = show c
showStatement i (FloatLiteral f) = show f
showStatement i (ArrayLiteral a) = show a
showStatement i (MapLiteral ms) = "{" ++ (intercalate "," $ map show ms) ++ "}"
showStatement i (TupleLiteral ts) = "(" ++ (intercalate "," $ map show ts) ++ ")"
showStatement i (Assignment l r) = show l ++ " = " ++ show r
showStatement i (For s r b) = "for " ++ s ++ " <- " ++ show r ++ "\n" ++ showBlock (i+1) b
showStatement i (While c b) = "while " ++ show c ++ "\n" ++ showBlock (i+1) b
showStatement i (If s b) = "if " ++ show b ++ "\n" ++ showBlock (i+1) b
showStatement i (IfE s b e) = "if " ++ show b ++ "\n" ++ showBlock (i+1) b ++ (replicate (i*3) ' ') ++ "else\n" ++ showBlock (i+1) e

instance Show Statement where
  show = showStatement 0
