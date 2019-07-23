module Augmentation.Tree
    (pinchTree
    ,getUniques
    ,leaves
    ,numAugmentations
    ,equivPerms
    ) where

import Algebra
import Algebra.DGA
import Augmentation.Pinch
import Augmentation.Braid
import Augmentation.Disks
import Braid

import Data.List
import Data.Maybe
import Data.Either
import Data.Tree
import Control.Monad

import Debug.Trace
                
pinchTree :: AugBraid -> Tree (DGA_Map, AugBraid) --This is highly inefficient!!! It generates n! nodes! That's really bad! We can do better by identifying equivilant maps at each level of the tree!
pinchTree (AugBraid 0 _) = nullTree
pinchTree (AugBraid _ []) = nullTree
pinchTree b = Node (DGA_Map [], b)
        (foldl (\xs x -> (++) xs $ maybe [] (\l -> [l]) $ do
            { (m1, b1) <- pinchMap (x-1) b
            --; tree <- (\z -> if z == nullTree then Nothing else Just z) $ pinchTree b1
            ; let tree = pinchTree b1
            ; let forest = subForest $ fmap (\(m2,b2) -> (case (compose_maps m1 m2)
                                         of DGA_Map m -> DGA_Map $ id m --map (\(c,e) -> (c,inZp 2 e)) m
                                  ,b2)) tree
            ; return $ Node (m1,b1) forest
            }) [] [1..(length $ get_word $ toStdBraid b)])

nullTree = Node (DGA_Map [],AugBraid 0 []) []

perms :: Tree (a,Int) -> Tree (a,Int)
perms (Node (a,i) t) = Node (a,i) $ map (fmap (\(a',i') -> (a',if i == 0 then i' else if i == 1 then i'+1 else if i' >= i then i' + 1 else i')) . perms) t

labelNodes :: Forest a -> Forest (a,Int)
labelNodes t = zipWith (\x (Node a fs) -> Node (a,x) $ labelNodes fs) [1..(length t)] t

collect :: Tree a -> [[a]]
collect (Node a []) = [[a]]
collect (Node a as) = map (\as' -> a:as') $ concat $ map collect as

equivPerms :: Eq a => Tree a -> [[[Int]]]
equivPerms t0@(Node a t) = let t' = perms $ Node (a,0) $ labelNodes t
                               alist = nub $ leaves t0
                               ks = map tail $ collect t'
                               ks' = map (\k -> (fst $ last k, map snd k)) ks
                            in map (\a -> map snd $ filter (\(x,_p) -> a == x) ks') alist

leaves :: Tree a -> [a]
leaves (Node a []) = [a]
leaves (Node a as) = (concat $ map leaves as)

getUniques :: AugBraid -> [[Expression]]
getUniques b = nub $ map (\(m,b') -> (map (applyDGAMap m) $ map (\(c,_) -> Expression [Monomial 1 [c]]) $ algebra_footprint b)) $ leaves $ pinchTree b

numAugmentations :: AugBraid -> Integer
numAugmentations = genericLength . getUniques
