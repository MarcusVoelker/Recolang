module Parser.LParse where

import Parser.Continuations

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List

data Parser r a = Parser {pFunc :: String -> DCont r String (a,String)}

instance Functor (Parser r) where
    fmap f p = Parser (fmap (first f) . pFunc p)

instance Applicative (Parser r) where
    pure = return
    f <*> a = f >>= (\ff -> fmap ff a)

instance Alternative (Parser r) where
    empty = pFail "Empty Fail"
    p1 <|> p2 = Parser (\s -> branch (pFunc p1 s) (pFunc p2 s))

instance Monad (Parser r) where
	return = constParse 
	a >>= f = Parser (pFunc a >=> (\ (r, s') -> pFunc (f r) s'))

(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= (\x -> b >> return x)

parse :: Parser r a -> String -> (a -> r) -> (String -> r) -> r
parse p s btr = run (pFunc p s) (btr . fst)

debugParse :: (Show a) => Parser (IO ()) a -> String -> IO ()
debugParse p s = run (pFunc p s) (putStr . (\x -> show (fst x) ++ "\n")) (\e -> putStr ("Error: "++ e ++ "\n"))

infixl 1 <.
infixr 0 .>

constParse :: a -> Parser r a
constParse a = Parser (\s -> return (a,s))

dot :: Parser r a -> (a -> b -> c) -> Parser r b  -> Parser r c
dot pa f pb = Parser (pFunc pa >=> (\(a,r) -> fmap (first (f a)) (pFunc pb r)))

(<.) :: a -> b -> (a,b)
(<.) = (,)

(.>) :: (Parser r a, a -> b -> c) -> Parser r b -> Parser r c
(.>) = uncurry dot

cParse :: (String -> Bool) -> Parser r a -> String -> Parser r a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

prefixParse :: String -> Parser r a -> Parser r a
prefixParse pre p = Parser (\s -> if pre `isPrefixOf` s then pFunc p s else throw ("Expected " ++ pre))

dPrefixParse :: String -> Parser r a -> Parser r a
dPrefixParse pre p = prefixParse pre (pParse (drop (length pre)) p)

pParse :: (String -> String) -> Parser r a -> Parser r a
pParse f p = Parser (pFunc p . f)

pFail :: String -> Parser r a
pFail err = Parser (const $ throw err)

noopParse :: Parser r ()
noopParse = Parser (\s -> DCont (\btr _ -> btr ((),s)))

charParse :: (Char -> a) -> Parser r a
charParse f = Parser (\s -> DCont (\btr _ -> btr (f $ head s,tail s)))

consume :: String -> Parser r ()
consume pre = cParse (pre `isPrefixOf`) (pParse (drop (length pre)) noopParse) ("Expected \"" ++ pre ++ "\"")

eatWhitespace :: Parser r ()
eatWhitespace = noopParse <.const.> many (consume " ")

wConsume :: String -> Parser r()
wConsume s = eatWhitespace <.const.> consume s <.const.> eatWhitespace

sepMany :: String -> Parser r a -> Parser r [a]
sepMany s p = (p <.(:).> many (wConsume s <.const id.> p)) <|> (fmap return p) <|> (return [])


