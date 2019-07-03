module GenBraid
    (genTorusBaid
    ,genRandPosBraid
    ,genRandNegBraid
    ,genRandBraid
    ) where

import Control.Monad.Random.Lazy

genTorusBraid :: Int -> Int -> Braid
genTorusBraid m n = 
