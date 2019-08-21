import Conjecture
import Control.Monad.Random
import Braid

main = do
        { putStrLn "\n phi isotopy conjecture"
        ; print $ map fst $ filter (not . snd) $ ordCon $ genTorusBraid 3 2
        ; print $ map fst $ filter (not . snd) $ ordCon $ genTorusBraid 2 3
        --; bools <- mapM (\w -> mapM (\l -> mapM (\_ -> evalRandIO $ ordTest w l) [1]) [2..10]) [2..5]
        --; mapM_ print $ filter (\(_,l) -> l /= []) $ concat $ concat bools
        }
