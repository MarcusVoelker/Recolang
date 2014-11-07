module CodeModel.Interface where

import CodeModel.Signature

data Interface = Interface String [Signature]

instance Show Interface where
	show (Interface name sigs) = "interface " ++ name ++ "\n" ++ unlines (map (\s -> "    "++ show s) sigs)