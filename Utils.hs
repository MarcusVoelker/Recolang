module Utils where

escape :: Char -> Char
escape '0' = '\0'
escape 'n' = '\n'
escape 't' = '\t'
escape '"' = '"'
escape '\'' = '\''
escape '\\' = '\\'
