module Augmentations
    (pinch
    ,torus_pinch
    ,legendrian_alphabet
    ) where

import Algebra
import Algebra.DGA
import Augmentation.Disks
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

legendrian_alphabet :: Braid -> [Char] -- assigns unicodde characters to each crossing in increasing order
legendrian_alphabet b = alph b 65

alph :: Braid -> Int -> [Char] -- Helper for legendrian_alphabet
alph (Braid _ []) _ = [] -- skip loopy bois for conveinience
alph (Braid 0 _) _ = []
--alph (Braid w []) k = (toEnum k):(alph (Braid (w-1) []) (k+1)) -- loopy bois
alph (Braid w (c:cs)) k = (toEnum k):(alph (Braid w cs) (k+1)) -- crossings
