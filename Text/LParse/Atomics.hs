module Text.LParse.Atomics where 

import Control.Continuations

import Control.Applicative
import Data.Char
import Text.LParse.Parser
import Text.LParse.Transformers

noop :: Parser r t ()
noop = return ()

full :: Parser r t [t]
full = many tokenReturn

discard :: Parser r t ()
discard = full >> noop

eoi :: Parser r t ()
eoi = cParse null noop ("Input not fully consumed")

tokenParse :: (t -> a) -> Parser r t a
tokenParse f = Parser (\s -> DCont (\btr etr -> if null s then etr "Unexpected EOI" else btr (f $ head s,tail s)))

tokenReturn :: Parser r a a
tokenReturn = tokenParse id

cPeek :: (Eq t, Show t) => ([t] -> Bool) -> Parser r t ()
cPeek cond = cParse cond noop "Expected match"

consume :: (Eq t, Show t) => [t] -> Parser r t ()
consume pre = cParse (\c -> all id (zipWith (==) pre c) && length pre <= length c) (pParse (drop (length pre)) noop) ("Expected " ++ show pre)

consumeSingle :: (Eq t, Show t) => t -> Parser r t ()
consumeSingle t = cParse (\s -> not (null s) && head s == t) (pParse tail noop) ("Expected " ++ show t)

word :: Parser r Char String
word = some (cParse (\s -> not (null s) && isLetter (head s)) tokenReturn "Expected letter")

integer :: Parser r Char Integer
integer = read <$> some (cParse (\s -> not (null s) && isDigit (head s)) tokenReturn "Expected digit")