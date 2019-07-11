{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module Braid
    (Braid (get_width,get_word,algebra_footprint,toStdBraid,fromStdBraid,isCross)
    ,StdBraid (StdBraid)
    ,AugBraid (AugBraid)
    ) where

import Data.Either
import Data.Functor.Identity
import Data.Functor.Classes

class (Show a, Eq a, Monad (M a),Eq1 (M a)) => Braid a where
    type M a :: * -> *
    get_width :: a -> Int
    get_width = get_width . toStdBraid
    get_word :: a -> [M a Int]
    algebra_footprint :: a -> [(Char,Int)]
    toStdBraid :: a -> StdBraid
    fromStdBraid :: StdBraid -> a
    isCross :: a -> Int -> Bool
    cross_art :: a -> Int -> M a Int -> Maybe String
    showBraid :: a -> String
    showBraid b = foldr (\x xs -> (concat $ map (showh x) s) ++ "\n" ++ xs) "" [0..(3*(w-1))]
        where w = get_width b
              s = get_word b
              showh row s' = maybe (case (row `mod` 3) of 0 -> "--------"
                                                          1 -> "        "
                                                          2 -> "        "
                                                          3 -> "--------") id $ cross_art b row s'
    equal :: a -> a -> Bool
    equal b1 b2 = (get_width b1 == get_width b2) && (and $ zipWith (\b b' -> eq1 b b') (get_word b1) (get_word b2))

data StdBraid = StdBraid Int [Int]
instance Braid StdBraid where
    type M StdBraid = Identity
    get_width (StdBraid _ []) = 0
    get_width (StdBraid w _)  = w
    get_word (StdBraid _ w) = map Identity w
    algebra_footprint b = algebra_footprint $ ((fromStdBraid b)::AugBraid)
    toStdBraid = id
    fromStdBraid = id
    isCross (StdBraid _ w) x = x < length w
    cross_art _b row (Identity s) = if (row - (abs $ s-1)*3) `elem` [0..3]
                        then Just $ if s > 0 then (case ((row-(s-1)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                                   1 -> "   \\    "
                                                                                   2 -> "    \\   "
                                                                                   3 -> "--/  \\--")
                                             else (case ((row-(1-s)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                                   1 ->  "    /   "
                                                                                   2 ->  "   /    "
                                                                                   3 ->  "--/  \\--")
                        else Nothing
instance Eq StdBraid where
    (==) = equal
instance Show StdBraid where
    show = showBraid
-- --\  /-- |s-1|*4
--    \     |s-1|*4 +1
--     \    |s-1|*4 +2
-- --/  \-- |s-1|*4 +3
-- 8x4

footprinth :: Int -> [Either (Int,Char,Char) Int] -> [(Char,Int)]
footprinth _k [] = []
footprinth k ((Right i):xs) = (toEnum k,i):(footprinth (k+1) xs)
footprinth k ((Left (i,c,cinv)):xs) = [(c,i),(cinv,i+1)] ++ (footprinth (k+1) xs)

data AugBraid = AugBraid Int [Either (Int,Char,Char) Int] -- Either element of H1(L), its inverse, and its position or an integer representing the corresponding braid group element
instance Braid AugBraid where
    type M AugBraid = Either (Int,Char,Char)
    get_word (AugBraid _ w) = w
    algebra_footprint (AugBraid _ w) = footprinth 65 w
    toStdBraid (AugBraid w ws) = StdBraid w (rights ws)
    fromStdBraid (StdBraid w ws) = AugBraid w (map Right ws)
    isCross (AugBraid _ ws) x = (x < length ws) && (isRight $ ws !! x)
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
