module Libs.Sanatize
    (Sanatize
    ,sanatize
    ) where

data Sanatize a = Good (a -> Bool) a
                | Bad

sanatize :: (a -> Bool) -> a -> Sanatize a
sanatize f a = if f a then Good f a else Bad

instance Monad Sanatize where
    return a = Good (\x -> True) a
    (Good _ a) >>= g = case g a of Good f b -> if f b then Good f b else Bad
                                   Bad -> Bad
