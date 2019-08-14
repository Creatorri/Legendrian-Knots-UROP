module Augmentation.DGA
    (DGA_Map (DGA_Map)
    ,Augmentation (Aug)
    ,applyDGAMap
    ,compose_maps
    ,Algebra
    ,fromDGAMap
    ) where

import Algebra
import Braid
import Data.Maybe
import Numeric.LinearAlgebra as N
import Data.List
import Debug.Trace

default (Int,Double)

data DGA_Map = DGA_Map [(Char,Algebra)] deriving Eq
instance Show DGA_Map where
    show (DGA_Map l) = "[" ++ (foldr (\(c,e) xs -> [c] ++ "→" ++ show e ++ (if xs /= "]" then "," else "") ++ xs) "]" l)

data Augmentation = Aug StdBraid [(Char,[Vector Z])]
instance Show Augmentation where
    show (Aug b m) = (show b) ++ "\n" ++ (concat $ map (\(c,vs) -> [c] ++ "→" ++ (foldr (\x xs -> show x ++ if xs == "" then "" else "+") "" vs)) m)
instance Eq Augmentation where
    (Aug b1 m1) == (Aug b2 m2) = (b1 == b2) && eqh m1 m2

isUpperTri :: (Eq a, Num a,Element a) => Matrix a -> Bool
isUpperTri mat = let cols = toColumns mat
                     ls = concat $ map (\(i,l) -> map (\(j,x) -> (i,j,x)) l) $ zip [1..] $ map (zip [1..] . toList) cols
                  in and $ map (\(i,j,x) -> if i > j then x == 0 else True) ls

eqh :: [(Char,[Vector Z])] -> [(Char,[Vector Z])] -> Bool
eqh l1 l2 = maybe False id $ do
            { l1' <- mapM (\c -> lookup c l1) $ map fst l2
            ; let l2' = map snd l2
            ; let dims1 = nub $ map size $ concat l1'
            ; let dims2 = nub $ map size $ concat l2'
            ; dim <- if length dims1 == 1 && length dims2 == 1 && dims1 == dims2 then Just $ head dims1 else Nothing
            ; let m2 = fromRows $ map (fromZ :: Vector Z -> Vector R) $ concat l2'
            ; let n0s = map (\l -> (0,(length $ permutations l)-1)) l1'
            ; let suc mns = if mns == [] then [] else if (fst $ head mns) == (snd $ head mns) then (0,snd $ head mns):(suc $ tail mns) else(1 + (fst $ head mns), snd $ head mns):(tail mns)
            ; let bound (acc,mns) = if mns == [] then acc else bound (((snd $ head mns)+1)*acc,tail mns)
            ; let ubound = bound (1,n0s)
            ; let mat mns = fromRows $ map (fromZ :: Vector Z -> Vector R) $ concat $ zipWith (\l (m,_) -> (permutations l) !! m) l1' mns
            ; let check m = maybe False id $ do
                            { let (l,_,p,s) = lu m
                            ; let (lR,lC) = size l
                            ; let sq = abs $ lR - lC
                            ; let linv = inv $ if lC == lR then l else if lC < lR then l ||| (konst 0 (lC,sq) === ident sq) else l === (konst 0 (sq,lR) ||| ident sq)
                            ; pinv <- if s == 0 then Nothing else return $ inv p
                            ; let mat' = pinv N.<> linv N.<> m2
                            ; let cond = isUpperTri mat'
                            ; let cond' = and $ map (\x -> (x - (fromIntegral $ floor x) < cutoff) || (((fromIntegral $ ceiling x) - x) < cutoff)) $ toList $ flatten mat'
                            ; return $ cond && cond'
                            }
            ; let checkAll k mns = if size (mat mns) /= size m2 then False else if check $ mat mns then True else if k > ubound then False else checkAll (k+1) (suc mns)
            ; return $ checkAll 0 n0s
            }

fromDGAMap :: StdBraid -> DGA_Map -> [Char] -> Maybe Augmentation
fromDGAMap b (DGA_Map l) chars = do
                                { l' <- mapM (\(c,a) -> (represent chars a) >>= (\vs -> return (c,vs))) l
                                ; return $ Aug b l'
                                }

compose_maps :: DGA_Map -> DGA_Map -> DGA_Map
compose_maps (DGA_Map map1) (DGA_Map map2) = DGA_Map $ (map (\(c,exp) -> (c,applyDGAMap (DGA_Map map2) exp)) map1) ++ (filter (\(c,_) -> not $ elem c $ map fst map1) map2)

applyDGAMap :: DGA_Map -> Algebra -> Algebra
applyDGAMap (DGA_Map alist) a = appmaph alist a

appmaph::[(Char,Algebra)] -> Algebra -> Algebra
appmaph [] = id
appmaph cs = plugIn (\c -> case (lookup c cs) of Just e -> e
                                                 Nothing -> G $ E c)
