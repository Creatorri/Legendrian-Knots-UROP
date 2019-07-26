module Libs.Graph
    (LevelGraph (Level,Leaf)
    ,leaves
    ) where

-- A graph which can be organized into layers which only have edges connecting to the next layer
data LevelGraph a = Level [(a,[Int])] (LevelGraph a)
                  | Leaf [a]
instance (Show a) => Show (LevelGraph a) where
    show (Level l ls) = (foldr (\x xs -> show x ++ " " ++ xs) "\n" $ l) ++ show ls
    show (Leaf l) = foldr (\x xs -> show x ++ " " ++ xs) "" l
instance Functor LevelGraph where
    fmap f (Level ans ls) = Level (map (\(a,ns) -> (f a,ns)) ans) $ fmap f ls
    fmap f (Leaf as) = Leaf $ map f as
leaves :: LevelGraph a -> [a]
leaves (Leaf l) = l
leaves (Level _ l) = leaves l
