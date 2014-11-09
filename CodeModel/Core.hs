module CodeModel.Core where

import CodeModel.Function

data Core = Core String [Function]

instance Show Core where
  show (Core name funs) = "core " ++ name ++ "\n" ++ unlines (map show funs)
