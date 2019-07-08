{-# TypeFamilies #-}
module Braid
    (Braid (get_width,get_word,to_std_braid)
    ,Std_Braid (Std_Braid)
    ,Aug_Braid (Aug_Braid)
    ) where

import Data.Either

class (Monad (M a)) => Braid a where
    type M a :: * -> *
    get_width :: Int
    get_width = get_width . to_std_braid
    get_word :: [M a Int]
    to_std_braid :: Braid m -> Std_Braid
    cross_art :: Int -> M a Int -> Maybe String
instance Eq (Braid a) where
    b1 == b2 = (eqh get_width) && (eqh get_word)
instance Show (Braid a) where
    show b = foldr (\x xs -> (map (showh x) s) ++ "\n" ++ xs) "" [0..(3*(w-1))]
        where w = get_width b
              s = get_word b
              showh row s' = maybe (case (row `mod` 3) of 0 -> "--------"
                                                          1 -> "        "
                                                          2 -> "        "
                                                          3 -> "--------") id $ cross_art row s'
       where eqh = (\f-> (f b1) == (f b2))

data Std_Braid = Std_Braid Int [Int]
instance Braid Std_Braid where
    type M Std_Braid = Identity
    get_width (Std_Braid _ []) = 0
    get_width (Std_Braid w _)  = w
    get_word (Std_Braid _ w) = map Identity w
    to_std_braid = id
    cross_art row (Identity s) = if (row - (abs $ s-1)*3) `elem` [0..3]
                        then Just $ if s > 0 then (case ((row-(s-1)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                                   1 -> "   \\    "
                                                                                   2 -> "    \\   "
                                                                                   3 -> "--/  \\--")
                                             else (case ((row-(1-s)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                                   1 ->  "    /   "
                                                                                   2 ->  "   /    "
                                                                                   3 ->  "--/  \\--")
                        else Nothing

-- --\  /-- |s-1|*4
--    \     |s-1|*4 +1
--     \    |s-1|*4 +2
-- --/  \-- |s-1|*4 +3
-- 8x4

data Aug_Braid = Aug_Braid Int [Either (Char,Char,Int) Int] -- Either element of H1(L), its inverse, and it's position or an integer representing the corresponding braid group element
instance Braid Aug_Braid where
    type M Aug_Braid = Either (Char,Char)
    get_word (Aug_Braid _ w) = w
    to_std_braid (Aug_Braid w ws) = Std_Braid w (rights ws)
    cross_art row (Right s) = if (row - (abs $ s-1)*3) `elem` [0..3]
                        then Just $ if s > 0 then (case ((row-(s-1)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                                   1 -> "   \\    "
                                                                                   2 -> "    \\   "
                                                                                   3 -> "--/  \\--")
                                             else (case ((row-(1-s)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                                   1 ->  "    /   "
                                                                                   2 ->  "   /    "
                                                                                   3 ->  "--/  \\--")
                        else Nothing
    cross_art row (Left (s,s1,s2) = (if (row - (abs $ s-1)*3) `elem` [0..3]
                        then Just $ (case ((row-(s-1)*3) `mod` 4) of 0 -> "---" ++ show s1 ++  "---"
                                                                     1 -> "    "     ++       "    "
                                                                     2 -> "    "     ++       "    "
                                                                     3 -> "---" ++ show s2 ++  "---")
                        else Nothing
