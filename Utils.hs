module Utils where

escape :: Char -> Char
escape '0' = '\0'
escape 'n' = '\n'
escape 't' = '\t'
escape '"' = '"'
escape '\'' = '\''
escape '\\' = '\\'
escape x = x

data TEither t1 t2 t3 = T1 t1 | T2 t2 | T3 t3

partitionTEithers :: [TEither t1 t2 t3] -> ([t1],[t2],[t3])
partitionTEithers []        = ([],[],[])
partitionTEithers (T1 x:xs) = (\(a,b,c) -> (x:a,b,c)) $ partitionTEithers xs
partitionTEithers (T2 x:xs) = (\(a,b,c) -> (a,x:b,c)) $ partitionTEithers xs
partitionTEithers (T3 x:xs) = (\(a,b,c) -> (a,b,x:c)) $ partitionTEithers xs
