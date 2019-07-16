module Aug3
    (pinch
    ,pinchMap
    ,pinchGraph
    ,getUniques
    ,leaves
    ,numAugmentations
    ) where

import Algebra
import Algebra.DGA
import Augmentation.Disks
import Braid
import Libs.Graph

import Braid.GenBraid

import Data.List
import Data.Maybe
import Data.Either
--import Data.Tree
import Control.Monad

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

pinch :: Int -> AugBraid -> Maybe AugBraid
pinch x b = (Just . remove_unused) =<< (pinchh x (get_word b) (get_width b) (newChars x b))

pinchh :: Int -> [Either (Int,Char,Char) Int] -> Int -> Maybe (Char,Char) -> Maybe AugBraid
pinchh _ [] _ _ = Nothing
pinchh x ((Left (i,c,cinv)):cs) w t = (\b -> Just $ AugBraid w $ (Left (i,c,cinv)):(get_word b)) =<< (pinchh x cs w t)
pinchh 0 ((Right i):cs) w t = (\(c,cinv) -> return $ AugBraid w $ (Left (i,c,cinv)):cs) =<< t
pinchh x ((Right i):cs) w t = (\b -> Just $ AugBraid w $ (Right i):(get_word b)) =<< (pinchh (x-1) cs w t)

ithcross :: Int -> AugBraid -> Maybe Char
ithcross i b = crossh i (get_word b) (algebra_footprint b)

crossh _ [] _ = Nothing
--crossh _ [] _ = '\1'
crossh i w f = if (isRight (head w))
                then if (i == 0) then Just $ fst $ head f
                                 else crossh (i-1) (tail w) (tail f)
                else crossh i (tail w) (tail $ tail f)

newChars :: Int -> AugBraid -> Maybe (Char,Char)
newChars x b = charh x (get_word b) 460 --4607

charh _ [] _ = Nothing
charh 0 ((Right _):_) t = Just (toEnum (t+1), toEnum (t+2))
charh x ((Right _):cs) t = charh (x-1) cs (t+2)
charh x ((Left _):cs) t = charh x cs (t+2)
--charh x ((Left (_z,c,cinv)):cs) t = charh x cs ((+) 1 $ foldr max (t+1) [fromEnum c,fromEnum cinv])

pinchMap :: Int -> AugBraid -> Maybe (DGA_Map, AugBraid)
pinchMap _ (AugBraid 0 _) = Just (DGA_Map [], AugBraid 0 [])
pinchMap _ (AugBraid _ []) = pinchMap 0 (AugBraid 0 [])
pinchMap x b = do
    { let chars = algebra_footprint b
    ; let foot' = zipWith (\c x -> (c,isCross b x)) chars [0..]
    ; change <- ithcross x b
    ; (c,cinv) <- newChars x b
    ; let disks = map (\((c0,i),k) -> (++) [[c0]] $ map (\d -> cinv:(neg d)) $ augmentationDisks b change c0) foot'
    ; let cnegs = catMaybes $ map (\((c0,i),k) -> if c0 == change
            then Just $ (c0,[[c]])
            else if i == 0
                then Nothing
                else if k
                    then Just $ (c0,(++) [[c0]] $ map (\d -> cinv:(neg d)) $ augmentationDisks b change c0)
                    else Nothing -- Just (c0,[[c0]])
            ) foot'
    ; let cexps = map (\(c,s) -> (c,applyRelations b $ sum $ map (\t -> Expression [Monomial 1 t]) s)) $ filter (\(_,s) -> s /= []) cnegs
    ; let dmap = DGA_Map cexps
    ; b' <- pinch x b
    ; return $ (dmap, b')
    }

nullGraph = Leaf [DGA_Map []]

pinchgraphh :: [(DGA_Map,AugBraid)] -> Int -> LevelGraph DGA_Map
pinchgraphh last 1 = Leaf $ map fst last
pinchgraphh last i = let thisnext = map (\(m1,b1) ->
                            let preforest = catMaybes $ map (\y -> pinchMap (y-1) b1) [1..(length $ get_word $ toStdBraid b1)]
                                forest = map (\(m2,b2) -> (compose_maps m1 m2,b2)) preforest
                             in ((m1,b1),forest)
                            ) last
                         next = nub $ concat $ (map snd thisnext)
                         this = map (\(a,as) -> (fst a,catMaybes $ map (\s -> getnum next s) as)) thisnext
                      in Level this $ pinchgraphh next $ i-1

pinchGraph :: StdBraid -> LevelGraph DGA_Map
pinchGraph (StdBraid 0 _) = nullGraph
pinchGraph (StdBraid _ []) = nullGraph
pinchGraph b = let l = [0..(length $ get_word b)-1]
                   nodes = catMaybes $ map (\x -> pinchMap x $ fromStdBraid b) l
                in Level [(DGA_Map [],l)] $ pinchgraphh nodes (length l)

getnum :: Eq a => [a] -> a -> Maybe Int
getnum [] _ = Nothing
getnum (a:as) t = if a == t then Just 1 else (return . (+) 1) =<< (getnum as t)

getUniques :: StdBraid -> [[Expression]]
getUniques b = nub $ map (\m -> (map (applyDGAMap m) $ map (\(c,_) -> Expression [Monomial 1 [c]]) $ algebra_footprint b)) $ leaves $ pinchGraph b

numAugmentations :: StdBraid -> Integer
numAugmentations = genericLength . getUniques
