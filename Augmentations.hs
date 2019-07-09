module Augmentations
    (pinch
    ,pinchMap
    ,pinchTree
    ,numAugmentations
    ) where

import Algebra
import Algebra.DGA
import Augmentation.Disks
import Braid

import Data.List
import Data.Maybe
import Data.Either
import Data.Tree
import Control.Monad
import Libs.List

import Debug.Trace

remove_unused :: AugBraid -> AugBraid
remove_unused (AugBraid w ws) = case (unusedh w ws 0) of (w',ws') -> AugBraid w ws'

unusedh :: Int -> [Either (Int,Char,Char) Int] -> Int -> (Int, [Either (Int,Char,Char) Int])
unusedh w ws s
    | s == w = (w,ws)
    | (and $ map (\s' -> not $ elem s' $ rights ws) [s,s+1]) = (min wm (w-1), map (\x ->
        case x of (Left (i,c,c')) -> Left (if i == s then 0 else if i > s then i-1 else i,c,c')
                  (Right i) -> Right (if i > s then i-1 else i)) wms)
    | otherwise = unusedh w ws (s+1)
    where (wm,wms) = unusedh (w-1) ws s

pinch :: Int -> AugBraid -> AugBraid
pinch x b = remove_unused $ pinchh x (get_word b) (get_width b) 581

pinchh :: Int -> [Either (Int,Char,Char) Int] -> Int -> Int -> AugBraid
pinchh x [] w t = AugBraid w []
pinchh x ((Left (i,c,cinv)):cs) w t = AugBraid w $ (Left (i,c,cinv)):(get_word $ pinchh x cs w $ foldr max t [fromEnum c,fromEnum cinv])
pinchh 0 ((Right i):cs) w t = AugBraid w $ (Left (i,toEnum (t+1),toEnum (t+2))):cs
pinchh x ((Right i):cs) w t = AugBraid w $ (Right i):(get_word $ pinchh (x-1) cs w t)

ithcross :: Int -> AugBraid -> Char
ithcross i b = crossh i (get_word b) (algebra_footprint b)

--crossh _ [] _ = '\1'
crossh i w f = if (isRight (head w))
                then if (i == 0) then fst $ head f
                                 else crossh (i-1) (tail w) (tail f)
                else crossh i (tail w) (tail $ tail f)

newChars :: Int -> AugBraid -> (Char,Char)
newChars x b = charh x (get_word b) 581

charh x w t = if (isRight (head w))
                then if x == 0 then (toEnum (t+1), toEnum (t+2))
                               else charh (x-1) (tail w) t
                else case (fromLeft (0,toEnum t,toEnum t) (head w)) of (_z,c,cinv) -> charh x (tail w) (foldr max t [fromEnum c,fromEnum cinv])

pinchMap :: Int -> AugBraid -> (DGA_Map, AugBraid)
pinchMap _ (AugBraid 0 _) = (DGA_Map [], AugBraid 0 [])
pinchMap _ (AugBraid _ []) = pinchMap 0 (AugBraid 0 [])
pinchMap x b = (dmap, pinch x b)
    where chars = algebra_footprint b
          (c,cinv) = newChars x b
          change = ithcross x b
          cnegs = map (\(x,i) -> (x,if x == change
            then [[c]]
            else if i == 0 then [['\1']]
                           else map neg $ augmentationDisks b change x)) chars
          cexps = map (\(c,s) -> (c,sum $ map (\t -> if t == "\1"
                                            then Expression []
                                            else Expression [Monomial 1 t]) s)) cnegs
          dmap = DGA_Map cexps
            
pinchTree :: AugBraid -> Tree (DGA_Map, AugBraid)
pinchTree (AugBraid 0 _) = nullTree
pinchTree (AugBraid _ []) = pinchTree (AugBraid 0 [])
pinchTree b = Node (DGA_Map [], b) (map (\x -> let (m',b') = pinchMap x b
                                                   tree    = pinchTree b'
                                                in Node (m',b') (subForest $ fmap (\(m'',b'') -> (compose_maps m' m'',b')) tree)) $ filter (\x -> nullTree /= (pinchTree $ snd $ pinchMap x b)) $ [0..(length $ get_word $ toStdBraid b)])
nullTree = Node (DGA_Map [],AugBraid 0 []) []

numAugmentations :: AugBraid -> Integer
numAugmentations b = genericLength $ nub $ numh $ pinchTree b

numh :: Tree (DGA_Map,AugBraid) -> [DGA_Map]
numh (Node (m,b) []) = [m]
numh (Node _ t) = concat $ map numh t
