module Conjecture
    (pathMatrix
    ,isAug
    ) where

import Braid
import Data.Matrix
import Algebra

type Algebra = Adjoin Z2 FreeGroup
type AMatrix = Matrix Algebra

pathMatrix :: StdBraid -> AMatrix
pathMatrix (StdBraid q w) = pathh q w 65

pathh :: Int -> [Int] -> Int -> AMatrix
pathh q [] _ = identity q
pathh q (i:is) k = (matrix q q $ \(n,m) -> if n == m && (n < i || n > q-i+1) then N 1 else if (m == n && n == i) then G $ E $ toEnum k else if (m == n + 1 && n == i) || (m + 1 == n && m == i) then N 1 else N 0
                   ) * (pathh q is $ k + 1)


isAug :: (Algebra -> Algebra) -> StdBraid -> Bool
isAug f b = and $ map ((==) 1 . detLaplace) $ map (\x -> submatrix 1 1 x x m) [1..nrows m]
    where m = pathMatrix b
