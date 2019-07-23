module Braid.GenBraid
    (genTorusBraid
    ,genRandPosBraid
    ) where

import Control.Monad.Random.Lazy
import Braid.Class

genTorusBraid :: (Braid a) => Int -> Int -> a
genTorusBraid m n = fromStdBraid $ StdBraid m $ map (\x -> (+) 1 $ mod x (m-1)) [0..((m-1)*n-1)]

genRandPosBraid :: (RandomGen g,Braid a) => Int -> Int -> Rand g a
genRandPosBraid w l = liftRand (\g -> (fromStdBraid $ StdBraid w $ take l $ randomRs (1,w-1) g,snd $ split g))
