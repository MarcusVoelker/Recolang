module Continuations where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Category

data DCont r e a = DCont {run :: (a -> r) -> (e -> r) -> r}

chain :: DCont r e a -> (a -> DCont r e b) -> DCont r e b
chain c f = DCont (\btr etr -> run c (\x -> run (f x) btr etr) etr)

weakChain :: DCont r e a -> (a -> DCont r e a) -> DCont r e a
weakChain c f = DCont (\btr etr -> run c (\x -> run (f x) btr (\_ -> btr x)) etr)

exceptChain :: DCont r e a -> (a -> DCont r e b) -> (a -> b) -> DCont r e b
exceptChain c f e = DCont (\btr etr -> run c (\x -> run (f x) btr (\_ -> btr $ e x)) etr)

branch :: DCont r e a -> DCont r e a -> DCont r e a
branch c1 c2 = DCont (\atr etr -> run c1  atr (\_ -> run c2 atr etr))

returnValue :: a -> DCont r e a
returnValue x = DCont (\f _ -> f x)

throw :: e -> DCont r e a
throw x = DCont (\_ g -> g x)

instance Monad (DCont r e) where
    return = returnValue
    (>>=) = chain

instance Functor (DCont r e) where
    fmap = liftM

instance Applicative (DCont r e) where
    pure = return
    f <*> a = f >>= (<$> a)

instance Alternative (DCont r e) where
    empty = DCont (\_ g -> g undefined)
    p1 <|> p2 = branch p1 p2


data DContFunc r e i a = DContFunc {execute :: i -> (a -> r) -> (e -> r) -> r}

instance Category (DContFunc r e) where
    id = DContFunc (\i atr _ -> atr i)
    bc . ab = DContFunc (\a ctr etr -> execute ab a (\b -> execute bc b ctr etr) etr)

instance Arrow (DContFunc r e) where
    arr f = DContFunc (\i atr _ -> atr $ f i)
    first ab = DContFunc (\(a,c) bctr etr -> execute ab a (\b -> bctr (b,c)) etr)

instance Monad (DContFunc r e i) where
    return a = DContFunc (\_ itr _ -> itr a)
    dcfa >>= f = DContFunc (\i btr etr -> execute dcfa i (\a -> execute (f a) i btr etr) etr) 

instance Functor (DContFunc r e i) where
    fmap = liftM

instance Applicative (DContFunc r e i) where
    pure = return
    f <*> a = f >>= (<$> a)

instance Alternative (DContFunc r e i) where
    empty = DContFunc (\_ _ g -> g undefined)
    p1 <|> p2 = DContFunc (\i ctr etr -> execute p1 i ctr (\_ -> execute p2 i ctr etr))