import Data.List

-- secretly identical to c(1) = 1, c(n) = 2*3^(n-1) -- proof by OEIS A025192
countfills :: Int -> Int
countfills 1 = 1
countfills n = sum $ map (\i -> (countfills i) + (countfills $ n - i)) [1..n-1]
