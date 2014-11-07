module CodeModel.Signature where

import CodeModel.Type

data Signature = Signature String Type

instance Show Signature where
	show (Signature n t) = n ++ " :: " ++ show t