module Augmentation.Disks
    (Holomorphic_Disk (Script_M)
    ,augmentationDisks
    ) where
import Data.List
import Data.Maybe
import Control.Monad
import Braid
import Libs.List
import Safe

data Holomorphic_Disk = Script_M {pos::[Char],neg::[Char]} -- J-Holomorphic moduli disk with #pos + #neg points removed from the boundary
instance Show Holomorphic_Disk where
    show (Script_M po ne) = "Disk_{" ++ po ++ "," ++ (if ne == "" then "-" else ne) ++ "}"
instance Eq Holomorphic_Disk where
    (Script_M p1 n1) == (Script_M p2 n2) = let eq = (\a b -> a == b || a == (reverse b))
                                            in (eq p1 p2) && (eq n1 n2)

type Braid_Fragment = [(Char,Int)]

data Path = Node (Char,Int) Int Path --Node (c,k) i p: Change in row at (c,k) to i, p continues
          | Straight (Char,Int) [Int] Path --Straight (c,k) i p: No change in row(s) at (c,k), watching elements of i for negative quadrants, p continues
          | End Int --End i: stop the path at row i
instance Show Path where
    show (End _) = "End"
    show (Node c i p) = "N" ++ show c ++ "<" ++ show i ++ ">|" ++ show p
    show (Straight c i p) = "S" ++ show c ++ "<" ++ show i ++ ">|"++show p
instance Eq Path where
    (End i) == (End j) = i == j
    (Node c1 i1 p1) == (Node c2 i2 p2) = c1 == c2 && (i1 == i2) && p1 == p2
    (Straight c1 i1 p1) == (Straight c2 i2 p2) = c1 == c2 && i1 == i2 && p1 == p2
    _ == _ = False

getNodes :: Path -> Braid_Fragment --getNodes p: gets all the node elements of p, without row data
getNodes (Node c _ p) = c:(getNodes p)
getNodes (End _) = []
getNodes (Straight _ _ p) = getNodes p

getStraights :: Path -> [((Char,Int),[Int])] --getStraights p: gets all the straight elements of p, with row data
getStraights (Node _ _ p) = getStraights p
getStraights (End _) = []
getStraights (Straight c j p) = (c,j):(getStraights p)

lastofpath :: Path -> Path --lastofpath p: gets last non-end element of p
lastofpath (End i) = End i
lastofpath (Node c j (End i)) = Node c j (End i)
lastofpath (Straight c j (End i)) = Straight c j (End i)
lastofpath (Node _ _ p) = lastofpath p
lastofpath (Straight _ _ p) = lastofpath p

initpath :: Path -> Path --initpath p: gets all but the last non-end element of p
initpath (End i) = End i
initpath (Node _ _ (End i)) = End i
initpath (Straight _ _ (End i)) = End i
initpath (Node c i p) = Node c i $ initpath p
initpath (Straight c i p) = Straight c i $ initpath p

plusPath :: Path -> Path -> Path --plusPath p1 p2: appends p1 to p2, ignores end of p1
plusPath (End _) p = p
plusPath (Node c i (End _)) p = Node c i p
plusPath (Straight c i (End _)) p = Straight c i p
plusPath (Node c i p1) p2 = Node c i $ plusPath p1 p2
plusPath (Straight c i p1) p2 = Straight c i $ plusPath p1 p2

reversePath :: Path -> Int -> Path --reversePath p i: reverses p, starting at i
reversePath p i = case (lastofpath p) of Node c j _ -> Node c j rec
                                         Straight c j _ -> Straight c j rec
                                         End _ -> End i
    where rec = reversePath (initpath p) i

getPaths :: Int -> Int -> Braid_Fragment -> [Maybe Path] --getPaths i j fra: gets all paths starting at i and ending at j (which are not in fra) with points from the braid fragment fra
getPaths i j [] = if i == j then [Just $ End j] else [Nothing]
getPaths i j ((c,k):cks) = case abs (k - i) of 1 -> (ladder) ++ (bump) ++ (straight) --can change direction by laddering (si_path), bumping (bump_path), or just continue straight
                                               0 -> [Nothing] --ran into a crossing of the same row with more fra left, so has to stop
                                               _ -> straight --continue on
    where ladder = (maybe [] id $ do 
                   { pck <- si_path k i j cks
                   ; let ps = adjoin k pck
                   ; return ps
                   })
          bump = let bp = catMaybes $ bump_path k i j cks
                  in concat $ map (adjoin i) bp
          adjoin = (\x (pat,cks') -> map (\mp -> mp >>= (\p'-> return $ Node (c,k) k $ plusPath pat p')) $ getPaths x j cks')
          straight = map (\p -> p >>= (Just . Straight (c,k) [i])) (getPaths i j cks)

si_path :: Int -> Int -> Int -> Braid_Fragment -> Maybe (Path,Braid_Fragment) --si_path x i j fra: tries to find a path in fra that climbs to row x, if it does, it returns the required path and the remaining braid fragment, otherwise it returns Nothing
si_path _x _i _j [] = Nothing --Nothing found
si_path x i j ((c,k):cks)
    | k == i = Just (Node (c,k) x (End i),cks) --done
    | k == x = Nothing --ran into crossing above
    | otherwise = (si_path x i j cks) >>= (\(p,cks') -> Just (Straight (c,k) [i] p,cks')) --continue on

bump_path :: Int -> Int -> Int -> Braid_Fragment -> [Maybe (Path,Braid_Fragment)] --bump_path x i j fra: tries to find paths in fra that dip down to row x
bump_path _ _ _ [] = [Nothing] --Nothing found
bump_path x i j ((c,k):cks)
    | k == i = [Nothing] --ran into a crossing above
    | k == x  = [Just (Node (c,k) i (End i),cks)] --done
    | k == x' = straight ++ to_x' --go down another row
    | otherwise = straight --continue on
    where straight = map (\p -> p >>= (\(a,b) -> Just (Straight (c,k) [x,i] a,b))) (bump_path x i j cks)
          to_x' = concat $ catMaybes $ map (\p -> do
                        { (a0,b) <- p
                        ; let a = Node (c,k) x' a0
                        ; let after = bump_path x i j b
                        ; return $ map (\p' -> do{ (a',b') <- p'
                                                 ; return (plusPath a a',b')
                                                 }) after 
                        }) (bump_path x' x j cks)
          x' = if x > i then x + 1 else x - 1

splitBy :: [a] -> (a->Bool) -> ([a],[a])
splitBy [] _f = ([],[])
splitBy (x:xs) f = if f x then ([],x:xs) else (\(a,b) -> (x:a,b)) $ splitBy xs f

getDisk :: Path -> Path -> (Char,Int) -> (Char,Int) -> Maybe Holomorphic_Disk --getDisk p1 p2 c1 c2: given crossings c1 and c2, find all holomorphic disks with positive ends c1 and c2
getDisk p1 p2 (c1,i) (c2,j)
    | c1 == c2 = Nothing --Cannot start and end at the same place
    | (fromEnum c1) > (fromEnum c2) = Nothing --Must traverse left to right
    | p1 == p2 = output --If they're equal, go ahead
    | n1 `intersect` n2 /= [] = Nothing --If the two paths share nodes, stop
    | p1 `above` p2 && p2 `above` p1 = output --If neither of the paths is strictly above the other, go ahead
    | p2 `above` p1 = getDisk p2 p1 (c1,i) (c2,j) --If p2 is above p1, then flip the order
    | otherwise = output --All filters passed, go ahead
    where n1 = getNodes p1
          n2 = getNodes p2
          s1 = {-filter (\(s,_) -> not $ s `elem` n2) $-} getStraights p1
          s2 = {-filter (\(s,_) -> not $ s `elem` n1) $-} getStraights p2
          aboves = map (fst . fst) $ filter (\((_,i),j) -> or $ map (\j' -> i == j'-1) j) s1 -- negative points adjacent to the upper straights
          belows = map (fst . fst) $ filter (\((_,i),j) -> or $ map (\j' -> i == j'+1) j) s2 -- negative points adjacent to the lower straights
          output = Just $ Script_M [c1,c2] $ belows ++ (reverse aboves)

augmentationDisks :: Braid -> Char -> Char -> [Holomorphic_Disk] --augmentationDisks b c1 c2: given a braid and a pair of crossing references, find all holomorphic disks with postive ends at c1 and c2
augmentationDisks b p q
    | p < q = disks
    | p > q = map (\(Script_M po ne) -> Script_M (reverse po) (reverse ne)) $ augmentationDisks b q p
    | otherwise = []
    where i = (fromEnum p) - 65 -- convert char to int and control for offset
          j = (fromEnum q) - 65
          pth = (word b) !! i
          qth = (word b) !! j
          fra = initSafe $ tailSafe $ map (\x -> ((toEnum $ x + 65),abs $ (word b) !! x)) [i..j]
          paths = catMaybes $ getPaths pth qth fra
          disks = nub $ catMaybes $ concat $ map (\p1 -> map (\p2 -> getDisk p1 p2 (p,pth) (q,qth)) paths) paths

above :: Path -> Path -> Bool --p1 `above` p2: checks if p1 is strictly above p2
above (End _) (End _) = True
above (Node _ i p1) (Node _ j p2) = i <= j && p1 `above` p2
above (Straight _ i p1) (Straight _ j p2) = (or $ concat $ map (\i' -> map (\j' -> i' <= j') j) i) && p1 `above` p2
above (Straight _ i p1) (Node _ j p2)   = (or $ map (\i' -> i' <= j) i) && p1 `above` p2
above (Node _ i p1) (Straight _ j p2)   = (or $ map (\j' -> i <= j') j) && p1 `above` p2

interior :: Path -> Path -> Maybe Braid_Fragment --interior p1 p2: finds all the crossings between but not on p1 and p2
interior (End i) (End j) = if i == j then Just [] else Nothing
interior (End _) _ = Nothing
interior _ (End _) = Nothing
interior (Node _ _ p1) (Node _ _ p2) = interior p1 p2
interior (Straight (c,j) i1 p1) (Straight (c',j') i2 p2) = (\col -> return $ concat $ concat $
                map (\i ->
                    map (\i' ->
                        let l = initSafe $ tailSafe $ if i' > i then [i..i'] else [i'..i]
                         in (if j `elem` l then [(c,j)] else []) ++ (if j' `elem` l then [(c',j')] else []) ++ col) i2) i1) =<< interior p1 p2
interior (Node _ i  p1) (Straight (c,j) i2 p2) = (\col -> return $ concat $ 
                map (\i' ->
                    let l = initSafe $ tailSafe $ if i' > i then [i..i'] else [i'..i]
                     in (if j `elem` l then [(c,j)] else []) ++ col) i2) =<< interior p1 p2
interior (Straight c1 i p1) (Node c2 j p2) = interior (Node c2 j p2) (Straight c1 i p1)