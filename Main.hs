module Main where

import System.Environment

import Parser.Parser

execute :: [String] -> IO ()
execute args = do
    let name = last args
    code <- readFile name
    let mdl = parseFile code
    putStr (show mdl)

main :: IO ()
main = do
    args <- getArgs
    execute args