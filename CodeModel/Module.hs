module CodeModel.Module where

import CodeModel.Core
import CodeModel.Interface

data Import = Import String

data Module = Module String [Import] [Interface] [Core] [Trait]

instance Show Import where
    show (Import s) = "import " ++ s

instance Show Module where
    show (Module n ims ins cs ts) = "module " ++ n ++ "\n\n" ++ unlines (map show ims) ++ "\n"  ++ unlines (map show ts) ++ unlines (map show ins) ++ unlines (map show cs)
