module Braid.GenBraid
    (genTorusBraid
    ,genRandPosBraid
    ) where

import Control.Monad.Random.Lazy
import Braid

genTorusBraid :: Int -> Int -> Braid
genTorusBraid m n = Braid m $ map (\x -> (+) 1 $ mod x (m-1)) [0..(m-1)*n]

genRandPosBraid :: RandomGen g => Int -> Int -> Rand g Braid
genRandPosBraid w l = liftRand (\g -> (Braid w $ take l $ randomRs (1,w-1) g,snd $ split g))
