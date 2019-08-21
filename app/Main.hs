module Main
    (main
    ) where

import qualified Conjecture as C
import Libs.Graph
import Augmentation.Braid
import Augmentation
import Algebra
import Braid
import Control.Monad.Random
import qualified System.IO
import Data.List
import Data.Maybe
import Data.Tree
import Data.Functor.Identity

fac 0 = 1
fac n = n * (fac $ n-1)
catalan  n = div (fac $ 2*n) $ (fac $ n+1) * (fac n)
testnums m n = 0.5*((fac $ 2 * m) * (fac $ 2 * n))/((fac $ m+n)*(fac m)*(fac n))
numAug m n = numAugmentations $ genTorusBraid m n

file = "data/NumAugs.csv"

putInFile :: [Int] -> Int -> IO ()
putInFile (i:is) n = do { let ks = map (getAugs . genTorusBraid i) [1..n]
                        ; mapM_ (\k -> (print $ maybe 0 length k) >> (appendFile file $ show (maybe 0 length k) ++ ",")) ks
                        ; appendFile file "\n"
                        ; putInFile is n
                        } 

skrubAugs b@(StdBraid _ w) = do
        { let dim = length w
        ; let chars = map sChar [1..dim]
        --; let rel = relations' b 
        ; let alph = map fst $ algebra_footprint b
        ; let lg = pinchGraph b
        ; ls <- mapM (\(DGA_Map l,_) -> mapM (\(c,a) -> (represent chars a) >>= (\r -> Just (c,r))) l) $ leaves lg
        ; let augs = map (\l -> Aug b $ (\l -> filter (\v -> 1 == ((length $ filter (==v) l) `mod` 2)) $ nub l) $ map (\(c,vs) -> (c,{-map (\v -> rel #> v)-} vs)) l) ls
        ; return augs
        }

numeach :: LevelGraph a -> [Int]
numeach (Leaf as) = [length as]
numeach (Level ans lg) = (length ans):(numeach lg)

main = do
        { print "Running"
        ; appendFile file "\n"
        ; putInFile [2..5] 7
        }
