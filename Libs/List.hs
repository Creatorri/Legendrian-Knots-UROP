module Libs.List
    (remove
    ,split
    ,foldrM'
    ,mapM'
    ) where

import Data.List

remove :: Eq a => a -> [a] -> [a]
remove x l
    | l == [] = []
    | (head l) == x = tail l
    | otherwise = (head l):(remove x (tail l))

split :: String -> String -> [String]
split [] s = [s]
split key s
    | (length key) > (length s) = [s]
    | otherwise = (\(s',ss) -> s:(split key ss)) $ splith key s
splith _key [] = ([],[])
splith key s@(c:cs)
    | (length key) > (length s) = (s,[])
    | s' == key = ([], ss)
    | otherwise = (\(s'',ss) -> (c:s'',ss)) $ splith key cs
    where s' = take (length key) s
          ss = (iterate tail s) !! ((length key) -1)

foldrM' :: (Monad m) => (a -> b -> m b) -> b -> m [a] -> m b
foldrM' f b l  = l >>= (\l' -> case l' of [] -> return b
                                          (a:as) -> (foldrM' f b $ return as) >>= (\bs -> f a bs))

mapM' :: (Monad m) => (a -> m b) -> m [a] -> m [b]
mapM' f l = l >>= (\l' -> case l' of [] -> return []
                                     (a:as) -> do
                                                { rest <- mapM' f $ return as
                                                ; a' <- f a
                                                ; return $ a':rest
                                                })
