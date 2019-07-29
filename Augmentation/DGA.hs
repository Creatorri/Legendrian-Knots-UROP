module Augmentation.DGA
    (DGA_Map (DGA_Map)
    ,applyDGAMap
    ,compose_maps
    ,Algebra
    ) where

import Algebra

import Data.List

data DGA_Map = DGA_Map [(Char,Algebra)] deriving Eq
instance Show DGA_Map where
    show (DGA_Map l) = "[" ++ (foldr (\(c,e) xs -> [c] ++ "→" ++ show e ++ (if xs /= "]" then "," else "") ++ xs) "]" l)

compose_maps :: DGA_Map -> DGA_Map -> DGA_Map
compose_maps (DGA_Map map1) (DGA_Map map2) = DGA_Map $ (map (\(c,exp) -> (c,applyDGAMap (DGA_Map map2) exp)) map1) ++ (filter (\(c,_) -> not $ elem c $ map fst map1) map2)

applyDGAMap :: DGA_Map -> Algebra -> Algebra
applyDGAMap (DGA_Map alist) a = appmaph alist a

appmaph::[(Char,Algebra)] -> Algebra -> Algebra
appmaph [] = id
appmaph cs = plugIn (\c -> case (lookup c cs) of Just e -> e
                                                 Nothing -> G $ E c)
