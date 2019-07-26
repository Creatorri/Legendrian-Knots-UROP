module Augmentation.Pinch
    (pinch
    ,pinchMap
    ) where

import Algebra
import Augmentation.Braid
import Augmentation.Disks
import Braid

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad

remove_unused :: AugBraid -> AugBraid
remove_unused (AugBraid w ws) = case (unusedh w ws 0) of (w',ws') -> AugBraid w ws'

unusedh :: Int -> [Either (Int,Char) Int] -> Int -> (Int, [Either (Int,Char) Int])
unusedh w ws s
    | s == w = (w,ws)
    | (and $ map (\s' -> not $ elem s' $ rights ws) [s,s+1]) = (min wm (w-1), map (\x ->
        case x of (Left (i,c)) -> Left (if i == s then 0 else if i > s then i-1 else i,c)
                  (Right i) -> Right (if i > s then i-1 else i)) wms)
    | otherwise = unusedh w ws (s+1)
    where (wm,wms) = unusedh (w-1) ws s

pinch :: Int -> AugBraid -> Maybe AugBraid
pinch x b = (Just . remove_unused) =<< (pinchh x (get_word b) (get_width b) (newChars x b))

pinchh :: Int -> [Either (Int,Char) Int] -> Int -> Maybe FreeGroup -> Maybe AugBraid
pinchh _ [] _ _ = Nothing
pinchh x ((Left (i,c)):cs) w t = (\b -> Just $ AugBraid w $ (Left (i,c)):(get_word b)) =<< (pinchh x cs w t)
pinchh 0 ((Right i):cs) w t = (\(E c) -> return $ AugBraid w $ (Left (i,c)):cs) =<< t
pinchh x ((Right i):cs) w t = (\b -> Just $ AugBraid w $ (Right i):(get_word b)) =<< (pinchh (x-1) cs w t)

ithcross :: Int -> AugBraid -> Maybe FreeGroup
ithcross i b = crossh i (get_word b) (algebra_footprint b)

crossh _ [] _ = Nothing
crossh i w f = if (isRight (head w))
                then if (i == 0) then case (fst $ head f) of G (E c) -> Just $ E c
                                                             _ -> Nothing
                                 else crossh (i-1) (tail w) (tail f)
                else crossh i (tail w) (tail $ tail f)

newChars :: Int -> AugBraid -> Maybe FreeGroup
newChars x (AugBraid _ ws) | x > (length ws) = Nothing
newChars x _ = case (s x) of (G c) -> Just c
                             _ -> Nothing

charh _ [] _ = Nothing
charh 0 ((Right _):_) t = Just $ E $ toEnum (t+1)
charh x ((Right _):cs) t = charh (x-1) cs (t+2)
charh x ((Left _):cs) t = charh x cs (t+2)

pinchMap :: Int -> AugBraid -> Maybe (DGA_Map, AugBraid)
pinchMap _ (AugBraid 0 _) = Just (DGA_Map [], AugBraid 0 [])
pinchMap _ (AugBraid _ []) = pinchMap 0 (AugBraid 0 [])
pinchMap x b = do
    { let chars = algebra_footprint b
    ; let foot' = zipWith (\c x -> (c,isCross b x)) chars [0..]
    ; change <- ithcross x b
    ; changec <- case change of E c -> Just c
                                _ -> Nothing
    ; c <- newChars x b
    ; let cinv = invert c
    ; let cnegs = catMaybes $ map (\((c0,i),k) -> if c0 == (G change)
            then case c0 of G (E c0') -> Just (c0',G c)
                            _ -> Nothing
            else if i == 0
                then Nothing
                else if k
                    then case c0 of G (E c0') -> Just (c0',c0 + (sum $ map (\d -> (G $ (<>) cinv $ mconcat $ neg d)) $ augmentationDisks b changec c0'))
                                    _ -> Nothing
                    else Nothing
            ) (foot' :: [((Algebra,Int),Bool)])
    --; let cexps = map (\(c,s) -> (c,sum $ map (\t -> G $ E t) s)) $ filter (\(_,s) -> s /= []) cnegs
    ; let dmap = DGA_Map cnegs
    ; b' <- pinch x b
    ; return $ (dmap, b')
    }

