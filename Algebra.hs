{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Algebra
    (Monomial (Monomial)
    ,Expression (Expression)
    ,inZp
    ) where

import Data.List
import Data.Maybe

data Monomial = Monomial Integer String deriving Eq
newtype Expression = Expression [Monomial]
type UExp = [Monomial]
ubox :: Expression -> UExp
ubox (Expression ex) = ex

inZp :: Integer -> Expression -> Expression
inZp p = automap (\(Monomial c m) -> Monomial (c `mod` p) m)

instance Show Monomial where
    show (Monomial 0 _) = "0"
    show (Monomial 1 "") = "1"
    show (Monomial (-1) "") = "-1"
    show (Monomial 1 s) = s
    show (Monomial (-1) s) = "-" ++ s
    show (Monomial n s) = (show n) ++ s
instance Show Expression where
    show (Expression []) = ""
    show (Expression l)  =
        (\(m:ms) -> (show m) ++ (case ms of [] -> ""
                                            (Monomial c m'):ms' -> if c < 0 then "" else "+"
                                ) ++ (show $ Expression ms)) $ sumSort l

cons' :: Monomial -> Expression -> Expression
cons' m (Expression ex) = Expression $ m:ex

automap :: (Monomial -> Monomial) -> Expression -> Expression
automap f (Expression e) = Expression $ map f e

map' :: (Monomial -> b) -> Expression -> [b]
map' f (Expression ex) = map f ex

fold' :: (Monomial -> Expression -> Expression) -> Expression -> Expression -> Expression
fold' _ d (Expression []) = d
fold' f d (Expression (e:es)) = f e (fold' f d $ Expression es)

sum' :: Expression -> Expression
sum' = fold' (\x es -> (Expression [x]) + es) e0

filter' :: (Monomial->Bool) -> Expression -> Expression
filter' f (Expression l) = Expression $ filter f l

sumh :: Monomial -> Expression -> Expression
sumh m (Expression []) = Expression [m]
sumh (Monomial c m) (Expression ((Monomial c' m'):es)) = if m == m' then sumh (Monomial (c + c') m) (Expression es) else cons' (Monomial c' m') (sumh (Monomial c m) $ Expression es)

multh :: Monomial -> Expression -> Expression
multh (Monomial 0 _) ex = Expression [Monomial 0 ""]
multh (Monomial c m) ex = sum' $ automap (\(Monomial c' m') -> (Monomial (c*c') $ m++m')) ex

sumSort :: UExp -> UExp
sumSort l = sortBy (\(Monomial _ a) (Monomial _ b) -> compare (length a) (length b)) $ filter (\(Monomial c _) -> c /= 0) l

e0 = Expression [Monomial 0 ""]

instance Eq Expression where
    (Expression []) == (Expression []) = True
    (Expression [Monomial 0 _]) == (Expression []) = True
    (Expression []) == (Expression [Monomial 0 _]) = True
    (Expression [Monomial 0 _]) == (Expression [Monomial 0 _]) = True
    _ == (Expression []) = False
    _ == (Expression [Monomial 0 _]) = False
    a == b = (a - b) == e0
instance Num Expression where
    (Expression []) + (Expression l) = Expression l
    (Expression ((Monomial c m):es)) + ex2 = (+) (Expression es) $ sumh (Monomial c m) ex2
    fromInteger x = Expression [Monomial x ""]
    negate (Expression []) = Expression []
    negate (Expression ((Monomial c m):ms)) = cons' (Monomial (negate c) m) (negate $ Expression ms)
    abs ex = automap (\(Monomial c m) -> (Monomial (abs c) m)) ex
    signum ex = automap (\(Monomial c _) -> (Monomial (signum c) "")) ex
    (Expression a) * (Expression b)
        | ((Expression b) == e0) = e0
        | otherwise = case a of [] -> e0
                                [x] -> multh x $ Expression b
                                x:xs -> (multh x $ Expression b) + ((Expression $ xs) * (Expression b))
