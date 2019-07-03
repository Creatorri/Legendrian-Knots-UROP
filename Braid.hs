module Braid
    (Braid (Braid,width,word)
    ) where

data Braid = Braid {width :: Int,word :: [Int]} deriving Eq
instance Show Braid where
    show (Braid w s) = foldr (\x xs -> (showh x s) ++ "\n" ++ xs) "" [0..(3*(w-1))]

showh :: Int -> [Int] -> String
showh row [] = ""
showh row (s:ss) = (if (row - (abs $ s-1)*3) `elem` [0..3]
                        then if s > 0 then (case ((row-(s-1)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                            1 -> "   \\    "
                                                                            2 -> "    \\   "
                                                                            3 -> "--/  \\--")
                                      else (case ((row-(1-s)*3) `mod` 4) of 0 -> "--\\  /--"
                                                                            1 ->  "    /   "
                                                                            2 ->  "   /    "
                                                                            3 ->  "--/  \\--")
                        else (case (row `mod` 3) of 0 -> "--------"
                                                    1 -> "        "
                                                    2 -> "        "
                                                    3 -> "--------")) ++ showh row ss
-- --\  /-- |s-1|*4
--    \     |s-1|*4 +1
--     \    |s-1|*4 +2
-- --/  \-- |s-1|*4 +3
-- 8x4
--
--
--
