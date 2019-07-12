module Libs.List
    (remove
    --,permute
    ) where

import Data.List

remove :: Eq a => a -> [a] -> [a]
remove x l
    | l == [] = []
    | (head l) == x = tail l
    | otherwise = (head l):(remove x (tail l))

--permute :: Enum b => [a] -> [b] -> [a]

