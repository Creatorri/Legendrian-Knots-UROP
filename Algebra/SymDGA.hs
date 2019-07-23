module Algebra.SymDGA
    (CE_DGA (CE_DGA,get_gens,to_cross,get_braid)
    ,DGA_Map (Map)
    ,applyDGAMap
    ,composeMaps
    ) where

import Augmentation.Braid
import Algebra.Symbolic

import Data.List

type Expression = Expr Double

data CE_DGA = CE_DGA {get_gens::[Char], to_cross::Char->Maybe Int, get_braid::AugBraid}

data DGA_Map = Map [(Char,Expr Double)] deriving Eq
instance Show DGA_Map where
    show (Map l) = "[" ++ (foldr (\(c,e) xs -> [c] ++ "→" ++ show e ++ (if xs /= "]" then "," else "") ++ xs) "]" l)

composeMaps :: DGA_Map -> DGA_Map -> DGA_Map
composeMaps (Map map1) (Map map2) = Map $ (map (\(c,exp) -> (c,applyDGAMap (Map map1) exp)) map2) ++ (filter (\(c,_) -> not $ elem c $ map fst map2) map1)

applyDGAMap :: DGA_Map -> Expression -> Expression
applyDGAMap (Map alist) ex = foldr (\(c,p) e -> plugIn c p e) ex alist
