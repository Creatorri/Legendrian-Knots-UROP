module Augmentation.Graph
    (pinchGraph
    ,getUniques
    ,numAugmentations
    ,getAugs
    ,numAugs
    ) where

import Algebra
import Augmentation.DGA
import Augmentation.Disks
import Augmentation.Braid
import Augmentation.Pinch
import Braid
import Libs.Graph

import Braid.GenBraid

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad

import Debug.Trace

nullGraph = Leaf [(DGA_Map [],AugBraid 0 [])]

pinchgraphh :: [(DGA_Map,AugBraid)] -> Int -> LevelGraph (DGA_Map,AugBraid)
pinchgraphh last 1 = trace ("LeafSize: " ++ (show $ length last)) Leaf last
pinchgraphh last i = let thisnext = map (\(m1,b1) ->
                                                let preforest = catMaybes $ map (\y -> pinchMap (y-1) b1) [1..(length $ get_word $ toStdBraid b1)]
                                                    forest = map (\(m2,b2) -> (compose_maps m1 m2,b2)) preforest
                                                 in ((m1,b1),forest)) last
                         next = nub $ concat $ (map snd thisnext)
                         this = nub $ map (\(a,as) -> (a, catMaybes $ map (\s -> getnum next s) as)) thisnext
                      in trace ("LevelSize: " ++ show (length this)) $ Level this $ pinchgraphh next $ i-1

pinchGraph :: StdBraid -> LevelGraph (DGA_Map, AugBraid)
pinchGraph (StdBraid 0 _) = nullGraph
pinchGraph (StdBraid _ []) = nullGraph
pinchGraph b = let l = map (\x -> x-1) $ [1..(length $ get_word b)]
                   nodes = catMaybes $ map (\x -> pinchMap x $ fromStdBraid b) l
                in Level [((DGA_Map [],fromStdBraid b),l)] $ pinchgraphh nodes (length l)

getAugs :: StdBraid -> Maybe [Augmentation]
getAugs b@(StdBraid _ w) = do
                            { let dim = length w
                            ; let chars = map sChar [1..dim]
                            ; rel <- relations' b 
                            ; let alph = map fst $ algebra_footprint b
                            ; let lg = pinchGraph b
                            ; ls <- mapM (\(DGA_Map l,_) -> mapM (\(c,a) -> (represent chars a) >>= (\r -> Just (c,r))) l) $ leaves lg
                            ; let augs = map (\l -> Aug b $ map (\(c,vs) -> (c,map (\v -> rel #> v) vs)) l) ls
                            ; return $ nub augs
                            }
numAugs :: StdBraid -> Int
numAugs = maybe 0 length . getAugs

getnum :: Eq a => [a] -> a -> Maybe Int
getnum [] _ = Nothing
getnum (a:as) t = if a == t then Just 1 else (return . (+) 1) =<< (getnum as t)

getUniques :: StdBraid -> [[Algebra]]
getUniques b = {-head $ sortBy (\a b -> compare (length a) (length b)) $ map-} (\lg -> nub $ map (\(m,b') -> (map (applyDGAMap m) $ map fst $ algebra_footprint b)) $ leaves lg) $ pinchGraph b

numAugmentations :: StdBraid -> Integer
numAugmentations = (\n -> traceShow n n) . genericLength . getUniques
