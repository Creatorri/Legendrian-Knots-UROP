import Data.List

component :: Integral a => a -> a -> a -> [a]
component s p q = orbith (s `mod` q) ((s+p) `mod` q) q (+p)

orbith::Integral a => a -> a -> a -> (a -> a) -> [a]
orbith n0 n q f
    | n0 == (n `mod` q) = [n0]
    | otherwise = (n `mod` q):(orbith n0 ((f n) `mod` q) q f)

lsub::Eq a => [a]->[a]->[a]
lsub xs [] = xs
lsub [] ys = []
lsub (x:xs) (y:ys) = if x == y then lsub xs ys else x:(lsub xs ys)

compcount::Integral a => a -> a -> a
compcount p q = compcounth [] p q

compcounth::Integral a => [a] -> a -> a -> a
compcounth l p q
    | (sort l) == [0..q-1] = 0
    | otherwise = 1 + (compcounth (l++(component (head $ lsub [0..q-1] (sort l)) p q)) p q)

printForm:: Int -> IO ()
printForm n = mapM_ (\l -> putStrLn $ show l) $ map (\q -> (q,map (\p -> (p,compcount p q)) [1..q-1])) [1..n]
