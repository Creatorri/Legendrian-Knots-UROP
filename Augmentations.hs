module Augmentations
    (pinch
    ,torus_pinch
    ,CE_DGA (CE_DGA)
    ,legendrian_alphabet 
    ,DGA_Map (DGA_Map)
    ,applyDGAMap
    ,compose_maps
    ) where

import Algebra
import HolomorphicDisks
import Braid

import Data.List
import Data.Maybe
import Control.Monad
import Libs.List

import Debug.Trace

pinch :: Int -> Braid -> Braid -- Execute a Lagrangian pinch move on a braid at a crossing whose number is determined by position in the braid word (1-indexed)
pinch _ (Braid l []) = Braid l []
pinch 0 (Braid l (k:ks)) = Braid l ks
pinch i (Braid l (k:ks))
    | i < 0 = Braid l (k:ks)
    | otherwise = Braid l (k:(word $ pinch (i-1) (Braid l ks)))

torus_pinch :: Int -> Int -> Braid -> Braid -- Execute a Lagrangian pinch on a torus braid at the specified coordinates (1-indexed)
torus_pinch i j k = pinch ((j-1) + (i-1)*((width k) -1)) k

data CE_DGA = CE_DGA {get_gens::[Char], to_cross::Char->Maybe Int, get_braid::Braid}

legendrian_alphabet :: Braid -> [Char] -- assigns unicodde characters to each crossing in increasing order
legendrian_alphabet b = alph b 65

alph :: Braid -> Int -> [Char] -- Helper for legendrian_alphabet
alph (Braid _ []) _ = [] -- skip loopy bois for conveinience
alph (Braid 0 _) _ = []
--alph (Braid w []) k = (toEnum k):(alph (Braid (w-1) []) (k+1)) -- loopy bois
alph (Braid w (c:cs)) k = (toEnum k):(alph (Braid w cs) (k+1)) -- crossings

data DGA_Map = DGA_Map [(Char,Expression)]

compose_maps :: DGA_Map -> DGA_Map -> DGA_Map
compose_maps (DGA_Map map1) (DGA_Map map2) = DGA_Map $ (map (\(c,exp) -> (c,applyDGAMap (DGA_Map map2) exp)) map1) ++ (filter (\(c,_) -> not $ elem c $ map fst map1) map2)

applyDGAMap :: DGA_Map -> Expression -> Expression
applyDGAMap (DGA_Map alist) (Expression ex) = sum $ map (\m -> (appmaph alist m)) ex

appmaph::[(Char,Expression)] -> Monomial -> Expression
appmaph [] m = Expression [m]
appmaph alist (Monomial c ms) = foldl (\x m -> x * (maybe (Expression [(Monomial 1 [m])]) id $ lookup m alist)) (fromInteger c) ms

