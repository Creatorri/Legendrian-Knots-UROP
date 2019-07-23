module SymMain
    (main
    ) where
import Augmentation.SymGraph
import Augmentation.Braid
import Augmentation.SymPinch
import Braid
import Algebra.SymDGA
import Algebra.Symbolic
import Libs.Graph
import Data.List
import qualified Augmentation as A

graph m n = map fst $ map (\(m',b') -> (composeMaps (relations m n) m',b')) $ leaves $ pinchGraph $ genTorusBraid m n

s k = toEnum (k*2 + 459)
relations m n
    | (m,n) == (3,2) = Map [(s 3,(recip $ Var (s 2) * Var (s 2))*Var (s 1)),(s 4,(recip $ Var $ s 2))]
    -- | m == 2 = Map [(s n,recip $ product $ map (\k -> Var $ s k) $ [1..n-1])]
    | otherwise = Map []

main = do{ putStrLn "New"
         ; mapM_ print $ graph 2 3 
         ; putStrLn "Old"
         ; mapM_ print $ map fst $ leaves $ A.pinchGraph $ genTorusBraid 2 3
         }
