{-# LANGUAGE TypeFamilies #-}
module Augmentation.Braid
    (AugBraid (AugBraid)
    ) where

import Algebra
import Braid

import Data.Either
import Data.Functor.Identity

footprinth :: Int -> [Either (Int,Char,Char) Int] -> [(Char,Int)]
footprinth _k [] = []
footprinth k ((Right i):xs) = (toEnum k,i):(footprinth (k+1) xs)
footprinth k ((Left (i,c,cinv)):xs) = [(c,i-1),(cinv,i+1)] ++ (footprinth (k+1) xs)

split :: String -> String -> [String]
split [] s = [s]
split key s
    | (length key) > (length s) = [s]
    | otherwise = (\(s',ss) -> s:(split key ss)) $ splith key s
splith _key [] = ([],[])
splith key s@(c:cs)
    | (length key) > (length s) = (s,[])
    | s' == key = ([], ss)
    | otherwise = (\(s'',ss) -> (c:s'',ss)) $ splith key cs
    where s' = take (length key) s
          ss = (iterate tail s) !! ((length key) -1)

--TODO: ADD REALATIONS TO MAKE s_i * s_i^-1 = s_i^-1 * s_i = 1
apprelh :: [Monomial] -> [(Char,Char)] -> Expression
apprelh ms [] = Expression ms
apprelh ms ((c,cinv):ccs) = Expression $ map (\(Monomial k s) -> Monomial k $ concat $ split [c,cinv] $ concat $ split [cinv,c] s) $ (\(Expression ms') -> ms') $ apprelh ms ccs
{-
apprelh :: AugBraid -> [Monomial] -> [(Char,Char)] -> [Char] -> Expression
apprelh _ [] _ _ = Expression []
apprelh b ((Monomial k s):ms) ccinv cs
    | k == 0 = (Expression []) + (apprelh b ms ccinv cs)
    | invs /= [] = (Expression [Monomial k $ foldl (\xs (c1,c2) -> remove c1 $ remove c2 xs) s invs]) + (apprelh b ms ccinv cs)
    | otherwise = (Expression [Monomial k s]) + (apprelh b ms ccinv cs)
    where invs = filter (\(c1,c2) -> (c1 `elem` s) && (c2 `elem` s)) ccinv
-}
iscrossh :: [Either (Int,Char,Char) Int] -> Int -> Bool
iscrossh [] _ = False
iscrossh ((Left _):_) 0 = False
iscrossh ((Left _):_) 1 = False
iscrossh ((Right _):_) 0 = True
iscrossh ((Right _):cs) x = iscrossh cs (x-1)
iscrossh ((Left _):cs) x = iscrossh cs (x-2)
 
data AugBraid = AugBraid Int [Either (Int,Char,Char) Int] -- Either element of H1(L), its inverse, and its position or an integer representing the corresponding braid group element
instance Braid AugBraid where
    type M AugBraid = Either (Int,Char,Char)
    get_word (AugBraid _ w) = w
    algebra_footprint (AugBraid _ w) = footprinth 65 w
    toStdBraid (AugBraid w ws) = StdBraid w (rights ws)
    fromStdBraid (StdBraid w ws) = AugBraid w (map Right ws)
    isCross (AugBraid _ ws) x = iscrossh ws x
    applyRelations b (Expression e) = apprelh e (map (\(_,c,c') -> (c,c')) $ lefts $ get_word b) --(map fst $ filter snd $ zipWith (\x c -> (c,isCross b x)) [0..] $ map fst $ algebra_footprint b)
    cross_art b row (Right s) = cross_art (toStdBraid b) row (Identity s)
    cross_art _b row (Left (s,s1,s2)) = if (row - (abs $ s-1)*3) `elem` [0..3]
                        then Just $ (case ((row-(s-1)*3) `mod` 4) of 0 -> "---" ++ [s1] ++ "----"
                                                                     1 -> "   " ++  " " ++ "    "
                                                                     2 -> "   " ++  " " ++ "    "
                                                                     3 -> "---" ++ [s2] ++ "----")
                        else Nothing
instance Eq AugBraid where
    (==) = equal
instance Show AugBraid where
    show = showBraid
