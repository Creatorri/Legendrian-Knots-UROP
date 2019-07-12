module Libs.Graph
    (LevelGraph (Level)
    ,prune
    ) where

-- A graph which can be organized into layers which only have edges connecting to the next layer
data LevelGraph a = Level [(a,[Int])] (LevelGraph a)
                  | Leaf [a]

--Gets only the unique elements from each level and sanatizes any incorrect edges
prune :: Eq a => LevelGraph a -> LevelGraph a
prune (Leaf as) = Leaf $ nub as
prune (Level xns (Leaf as)) = Level (nub $ filter (not . null . snd) $ map (\(a,ns) -> (a,filter (\n -> n < (length as') ns))) xns) (Leaf as')
prune (Level xns ls) = Level (nub $ case (prune ls) of Level xns' _ -> filter (not . null . snd) $ map (\(a,ns) -> (a,nub $ filter (\n -> n < (length xns')) ns)) xns
                                                       Leaf as      -> filter (not . null . snd) $ map (\(a,ns) -> (a,nub $ filter (\n -> n < (length as  )) ns)) xns) (prune ls)

--Depth-first folds using f and collects each path's results
collapse :: (a -> b -> b) -> b -> LevelGraph a -> [b]

