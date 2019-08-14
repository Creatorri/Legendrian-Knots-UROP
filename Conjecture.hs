module Conjecture
    (
    --,pathMatrix
    --,isAug
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
orderPinchh b@(AugBraid w _) [] = if w == 0 then Just $ DGA_Map [] else Nothing
orderPinchh b@(AugBraid 0 _) perm = if perm == [] then Just $ DGA_Map [] else Nothing
orderPinchh b (p:per) = do
                        { (m,b') <- pinchMap p b
                        ; m' <- orderPinchh b' $ map (\p' -> if p' > p then p'-1 else p') per
                        ; return $ compose_maps m m'
                        }

ordConj :: StdBraid -> Bool
ordConj b@(StdBraid _ ws) = let chars = catMaybes $ map (\g -> case g of G (E c) -> Just c; _ -> Nothing) $ map fst $ algebra_footprint b
                                f a = lookup a $ zip [1..] chars
                                disks = map fst $ filter (\((c1,c2),x) -> x == [] && c1 /= c2) $ concat $ map (\c1 -> map (\c2 -> ((c1,c2),augmentationDisks b c1 c2)) chars) chars
                                nocob = filter (\l -> let a' = maybe '\0' id $ f $ head l
                                                          b' = maybe '\0' id $ f $ head $ tail l
                                                       in or $ map (\(c1,c2) -> (c1 == a' && c2 == b') || (c1 == b' && c2 == a')) disks) $ permutations [1..(length chars)]
                             in (==1) $ (\b -> traceShow b b) $ maybe 0 length $ mapM (\perm -> orderPinch b perm) nocob

ordTest :: RandomGen g => Int -> Int -> Rand g Bool
ordTest w l = do
            { b <- genRandPosBraid w l
            ; return $ ordConj $ traceShow b b
            }

main = do
        { bools <- mapM (\w -> mapM (\l -> (evalRandIO $ ordTest w l) >>= (\b -> return ((w,l),b))) [2..5]) [2..5]
        ; print $ map fst $ filter (\(_,b) -> not b) $ concat bools
        }
