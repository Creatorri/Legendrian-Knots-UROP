module Algebra.DGA
    (CE_DGA (CE_DGA,get_gens,to_cross,get_braid)
    ,DGA_Map (DGA_Map)
    ,applyDGAMap
    ,compose_maps
    ) where
import Braid
import Algebra

import Data.List

data CE_DGA = CE_DGA {get_gens::[Char], to_cross::Char->Maybe Int, get_braid::AugBraid}

data DGA_Map = DGA_Map [(Char,Expression)] deriving Eq

compose_maps :: DGA_Map -> DGA_Map -> DGA_Map
compose_maps (DGA_Map map1) (DGA_Map map2) = DGA_Map $ (map (\(c,exp) -> (c,applyDGAMap (DGA_Map map2) exp)) map1) ++ (filter (\(c,_) -> not $ elem c $ map fst map1) map2)

applyDGAMap :: DGA_Map -> Expression -> Expression
applyDGAMap (DGA_Map alist) (Expression ex) = sum $ map (\m -> (appmaph alist m)) ex

appmaph::[(Char,Expression)] -> Monomial -> Expression
appmaph [] m = Expression [m]
appmaph alist (Monomial c ms) = foldl (\x m -> x * (maybe (Expression [(Monomial 1 [m])]) id $ lookup m alist)) (fromInteger c) ms
