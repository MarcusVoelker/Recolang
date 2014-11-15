module CodeModel.Core where

import CodeModel.Function
import CodeModel.Signature

data Core = Core String [Function]

instance Show Core where
  show (Core name funs) = "core " ++ name ++ "\n" ++ unlines (map show funs)

getFunction :: Core -> String -> Maybe Function
getFunction (Core _ fs) s = (\filtered -> if null filtered then Nothing else Just $ head filtered) (filter (\(Function (Signature n _) _) -> n == s) fs)
