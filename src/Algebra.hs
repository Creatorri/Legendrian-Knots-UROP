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
{--type Algegra' = [(Z2,Vector Z,FreeGroup)]

plush :: (Z2,Vector Z,FreeGroup) -> Algebra' -> Algebra'
plush e [] = [e]
plush e1@(z1,v1,g1) (e2@(z2,v2,g2):ls) = if v1 == v2 && g1 == g2 then (if z1+z2 == 0 then (0,N.fromList $ map (\_ -> 0) $ N.tolist v1,mempty) else (z1+z2,v1,g1)):ls else e2:(plush e1 ls)

instance Num Algebra' where
    l1 + l2 = map (\e -> plush e l2) l1
    l1 * l2 = sum $ map (\(z1,v1,g1) -> map (\(z2,v2,g2) -> (z1*z2,v1+v2,g1<>g2)) l2) l1
    abs = map (\(z,v,g) -> (abs z,v,g))
    signum = map (\(z,v,g) -> (signum z,v,g))
--    fromInteger x = [(fromInteger x,
-}
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
