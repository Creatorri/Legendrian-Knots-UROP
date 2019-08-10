module Algebra
    (Adjoin (..)
    ,FreeGroup (..)
    ,Group (..)
    ,Z2
    ,plugIn
    ,Algebra
    ,represent
    ,cutoff
    ,module N
    ) where

import Algebra.Adjoin
import Algebra.Group
import Algebra.FreeGroup
import Algebra.Z2
import Numeric.LinearAlgebra as N

type Algebra = Adjoin Z2 FreeGroup

cutoff :: R
cutoff = 0.0000000001

plugIn :: (Eq f,Fractional f) => (Char -> Adjoin f FreeGroup) -> Adjoin f FreeGroup -> Adjoin f FreeGroup
plugIn f = adjPlugIn (groupPlugIn f)

represent :: [Char] -> Algebra -> Maybe [Vector Z]
represent [] _ = Nothing
represent elems expr = do
                        { chain <- pairPlusChain expr
                        ; vectors <- mapM (\(_,g) -> toPseudoVector g) chain
                        ; return $ map (\l -> (length elems) |> [toEnum $ maybe 0 id $ lookup (elems !! i) l | i <- [0..]]) vectors
                        }
