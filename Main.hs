module Main
    (main
    ) where

import Augmentations
import Augmentation.Disks
import Algebra
import Algebra.DGA
import Braid
import Braid.GenBraid
import Control.Monad.Random
import qualified System.IO
import Data.List
import Data.Maybe
import Data.Tree

b0 :: AugBraid
b0 = genTorusBraid 2 3
--b1 = pinch 1 b0
m1 = DGA_Map [('A',Expression [Monomial 1 "\564"]),('E',Expression [Monomial 1 "E",Monomial 1 "\565B"]),('F',Expression [Monomial 1 "F",Monomial 1 "\565C"]),('G',Expression [Monomial 1 "G", Monomial 1 "\565D"]),('H',Expression [Monomial 1 "H",Monomial 1 "\565"])]
--b2 = pinch 1 b1
m2 = DGA_Map [('B',Expression [Monomial 1 "\568"]),('F',Expression [Monomial 1 "F",Monomial 1 "\569EC"]),('G',Expression [Monomial 1 "G",Monomial 1 "\569ED"]),('H',Expression [Monomial 1 "H",Monomial 1 "\569E"]),('I',Expression [Monomial 1 "I",Monomial 1 "\569C"]),('J',Expression [Monomial 1 "J",Monomial 1 "\569D"]),('K',Expression [Monomial 1 "K",Monomial 1 "\569"])]
--b3 = pinch 1 b2
m3 = DGA_Map [('C',Expression [Monomial 1 "\571"]),('G',Expression [Monomial 1 "G",Monomial 1 "\572F"]),('J',Expression [Monomial 1 "J",Monomial 1 "\572I"])]
map1 = (applyDGAMap m1)
map2 = (applyDGAMap m2) . map1
map3 = (applyDGAMap m3) . map2
map1' = (applyDGAMap m1)
map2' = (applyDGAMap $ compose_maps m1 m2)
map3' = (applyDGAMap $ compose_maps m1 $ compose_maps m2 m3)
alph = map (\c -> Expression [Monomial 1 [c]]) "ABCDEFGHIJKL"

tree = pinchTree b0
printableTree = fmap (\(m,b) -> (show $ map (applyDGAMap m) alph) ++ "\n" ++ (show b)) tree
thing = drawTree printableTree

main = do { System.IO.hSetEncoding System.IO.stdout System.IO.utf8
          ; putStrLn "Step 1"
          ; mapM_ (print . map1) alph
          ; putStrLn "Step 2"
          ; mapM_ (print . map2) alph
          ; putStrLn "Step 3"
          ; mapM_ (print . map3) alph
          ; putStrLn "Step 1"
          ; mapM_ (print . map1') alph
          ; putStrLn "Step 2"
          ; mapM_ (print . map2') alph
          ; putStrLn "Step 3"
          ; mapM_ (print . map3') alph
          }

