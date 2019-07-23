{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module Braid.Class
    (Braid (..)
    ,StdBraid (StdBraid)
    ) where

import Algebra.Expression
import Libs.List
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
    applyRelations :: a -> Expression -> Expression
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

footprinth :: Int -> [Int] -> [(Char,Int)]
footprinth _k [] = []
footprinth k (i:xs) = (toEnum k,i):(footprinth (k+1) xs)

data StdBraid = StdBraid Int [Int]
instance Braid StdBraid where
    type M StdBraid = Identity
    get_width (StdBraid _ []) = 0
    get_width (StdBraid w _)  = w
    get_word (StdBraid _ w) = map Identity w
    algebra_footprint (StdBraid _ w) = footprinth 65 w
    toStdBraid = id
    fromStdBraid = id
    applyRelations _ = id
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
