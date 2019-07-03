module Libs.List
    (remove
    --,permute
    ) where

import Data.List

remove :: Eq a => [a] -> a -> [a]
remove l x
    | l == [] = []
    | (head l) == x = tail l
    | otherwise = (head l):(remove (tail l) x)

--permute :: Enum b => [a] -> [b] -> [a]

