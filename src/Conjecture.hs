module Conjecture
    ( orderPinch
    , ordCon
    , ordTest
    ) where

import Braid
import Algebra
import Augmentation
import Data.Maybe
import Data.List
import Control.Monad.Random
import Debug.Trace
{-
import Data.Matrix
-- Augmentation checking
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
-}
-- Non-cobounding order checker
orderPinch :: StdBraid -> [Int] -> Maybe Augmentation
orderPinch b@(StdBraid _ ws) perm = (orderPinchh (fromStdBraid b) perm) >>= \m -> fromDGAMap b m (map sChar [1..(length ws)])

orderPinchh :: AugBraid -> [Int] -> Maybe DGA_Map
orderPinchh b@(AugBraid w _) [] = Just $ DGA_Map []
orderPinchh b (p:per) = do
                        { (m,b') <- pinchMap (p-1) b
                        ; m' <- orderPinchh b' $ map (\p' -> if p' > p then p'-1 else p') per
                        ; return $ compose_maps m m'
                        }

noncobound :: StdBraid -> [(Int,Int)]
noncobound b = let chars = catMaybes $ map (\g -> case g of G (E c) -> Just c; _ -> Nothing) $ map fst $ algebra_footprint b
                   f a = maybe (-1) id $ lookup a $ zip chars [1..]
                   disks = map fst $ filter (\((c1,c2),x) -> x == [] && c1 /= c2) $ concat $ map (\c1 -> map (\c2 -> ((c1,c2),augmentationDisks b c1 c2)) chars) chars
                in nubBy (\(c1,c2) (d1,d2) -> (==2) $ length $ nub [c1,c2,d1,d2]) $ map (\(c1,c2) -> (f c1, f c2)) disks

type Perm = [Int]

phi :: (Int,Int) -> Perm -> Perm
phi (a,b) = map (\x -> if x == a then b else if x == b then a else x) 

equiv :: StdBraid -> Perm -> Perm -> Bool
equiv b p1 p2 = let nocos = noncobound b
                    phis = map (\p -> phi p p2) nocos
                 in p1 `elem` phis

ordCon :: StdBraid -> [((Perm,Perm),Bool)]
ordCon b@(StdBraid _ ws) = let perms = permutations [1..(length ws)]
                               nocos = noncobound b
                               pairs = nubBy (\(h,i) (j,k) -> (==2) $ length $ nub [h,i,j,k]) $ concat $ map (\p -> map (\no -> (p,phi no p)) nocos) perms
                            in map (\(p1,p2) -> (\bool -> ((p1,p2), bool)) $ maybe (trace "Failed" False) id $ do 
                                                                { a1 <- orderPinch b p1
                                                                ; a2 <- orderPinch b p2
                                                                ; return $ a1 == a2
                                                                }) pairs

ordTest :: RandomGen g => Int -> Int -> Rand g (StdBraid,[(Perm,Perm)])
ordTest w l = do
            { b <- genRandPosBraid w l
            ; return (b,map fst $ filter (\(_,bool) -> not bool) $ ordCon b)
            }
