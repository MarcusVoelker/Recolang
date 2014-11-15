module Parser.LParse where

import Continuations

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import Data.List

import Utils

data Parser r t a = Parser {pFunc :: [t] -> DCont r String (a,[t])}

instance Functor (Parser r t) where
    fmap f p = Parser (fmap (first f) . pFunc p)

instance Applicative (Parser r t) where
    pure = return
    f <*> a = f >>= (<$> a)

instance Alternative (Parser r t) where
    empty = pFail "Empty Fail"
    p1 <|> p2 = Parser (\s -> branch (pFunc p1 s) (pFunc p2 s))

instance Monad (Parser r t) where
	return = constParse 
	a >>= f = Parser (pFunc a >=> (\ (r, s') -> pFunc (f r) s'))

(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= (\x -> b >> return x)

parse :: Parser r t a -> [t] -> (a -> r) -> (String -> r) -> r
parse p s btr = run (pFunc p s) (btr . fst)

debugParse :: (Show a) => Parser (IO ()) t a -> [t] -> IO ()
debugParse p s = run (pFunc p s) (putStr . (\x -> show (fst x) ++ "\n")) (\e -> putStr ("Error: "++ e ++ "\n"))

infixl 1 <.
infixr 0 .>

constParse :: a -> Parser r t a
constParse a = Parser (\s -> return (a,s))

dot :: Parser r t a -> (a -> b -> c) -> Parser r t b  -> Parser r t c
dot pa f pb = Parser (pFunc pa >=> (\(a,r) -> fmap (first (f a)) (pFunc pb r)))

(<.) :: a -> b -> (a,b)
(<.) = (,)

(.>) :: (Parser r t a, a -> b -> c) -> Parser r t b -> Parser r t c
(.>) = uncurry dot

cParse :: ([t] -> Bool) -> Parser r t a -> String -> Parser r t a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

prefixParse :: (Eq t, Show t) => [t] -> Parser r t a -> Parser r t a
prefixParse pre p = Parser (\s -> if pre `isPrefixOf` s then pFunc p s else throw ("Expected " ++ show pre))

dPrefixParse :: (Eq t, Show t) => [t] -> Parser r t a -> Parser r t a
dPrefixParse pre p = prefixParse pre (pParse (drop (length pre)) p)

pParse :: ([t] -> [t]) -> Parser r t a -> Parser r t a
pParse f p = Parser (pFunc p . f)

pFail :: String -> Parser r t a
pFail err = Parser (const $ throw err)

noopParse :: Parser r t ()
noopParse = Parser (\s -> DCont (\btr _ -> btr ((),s)))

tokenParse :: (t -> a) -> Parser r t a
tokenParse f = Parser (\s -> DCont (\btr _ -> btr (f $ head s,tail s)))

consume :: (Eq t, Show t) => [t] -> Parser r t ()
consume pre = cParse (pre `isPrefixOf`) (pParse (drop (length pre)) noopParse) ("Expected " ++ show pre)

consumeSingle :: (Eq t, Show t) => t -> Parser r t ()
consumeSingle t = cParse (\s -> not (null s) && head s == t) (pParse tail noopParse) ("Expected " ++ show t)

peek :: (Eq t, Show t) => [t] -> Parser r t ()
peek pre = cParse (pre `isPrefixOf`) noopParse ("Expected " ++ show pre)

cPeek :: (Eq t, Show t) => ([t] -> Bool) -> Parser r t ()
cPeek cond = cParse cond noopParse "Expected match"

sepMany :: Parser r t () -> Parser r t a -> Parser r t [a]
sepMany sep p = (p <.(:).> many (sep >> p)) <|> fmap return p <|> return []

parseEither :: Parser r t a -> Parser r t b -> Parser r t (Either a b)
parseEither pa pb = do
    a <- optional pa
    case a of
        Nothing -> Right <$> pb
        (Just a') -> return $ Left a'

parseEitherList :: Parser r t a -> Parser r t b -> Parser r t ([a],[b])
parseEitherList pa pb = partitionEithers <$> many (parseEither pa pb)

parseTEither :: Parser r t a -> Parser r t b -> Parser r t c -> Parser r t (TEither a b c)
parseTEither pa pb pc = do
    a <- optional pa
    case a of
        Nothing -> do 
            b <- optional pb
            case b of
            	Nothing -> T3 <$> pc
            	(Just b') -> return $ T2 b'
        (Just a') -> return $ T1 a'

parseTEitherList :: Parser r t a -> Parser r t b -> Parser r t c -> Parser r t ([a],[b],[c])
parseTEitherList pa pb pc = partitionTEithers <$> many (parseTEither pa pb pc)