module CodeModel.Interface where

import Data.List

import CodeModel.Signature

data Interface = Interface String [String] [Signature] 

data Trait = Trait String [String] [Signature] 

instance Show Interface where
	show (Interface name trts sigs) = "interface " ++ name ++ (if null trts then "" else " : " ++ intercalate ", " trts) ++ "\n" ++ unlines (map (\s -> "   "++ show s) sigs)

instance Show Trait where
	show (Trait name trts sigs) = "trait " ++ name ++ (if null trts then "" else " : " ++ intercalate ", " trts) ++ "\n" ++ unlines (map (\s -> "   "++ show s) sigs)
