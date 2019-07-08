module HolomorphicDisks
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

data Path = Node (Char,Int) Int Path
          | Straight (Char,Int) [Int] Path
          | End Int
instance Show Path where
    show (End _) = "End"
    show (Node c i p) = "N" ++ show c ++ "<" ++ show i ++ ">|" ++ show p
    show (Straight c i p) = "S" ++ show c ++ "<" ++ show i ++ ">|"++show p
instance Eq Path where
    (End i) == (End j) = i == j
    (Node c1 i1 p1) == (Node c2 i2 p2) = c1 == c2 && (i1 == i2) && p1 == p2
    (Straight c1 i1 p1) == (Straight c2 i2 p2) = c1 == c2 && i1 == i2 && p1 == p2
    _ == _ = False

getNodes :: Path -> [(Char,Int)]
getNodes (Node c _ p) = c:(getNodes p)
getNodes (End _) = []
getNodes (Straight _ _ p) = getNodes p

getStraights :: Path -> [((Char,Int),[Int])]
getStraights (Node _ _ p) = getStraights p
getStraights (End _) = []
getStraights (Straight c j p) = (c,j):(getStraights p)

lastofpath :: Path -> Path
lastofpath (End i) = End i
lastofpath (Node c j (End i)) = Node c j (End i)
lastofpath (Straight c j (End i)) = Straight c j (End i)
lastofpath (Node _ _ p) = lastofpath p
lastofpath (Straight _ _ p) = lastofpath p

initpath :: Path -> Path
initpath (End i) = End i
initpath (Node _ _ (End i)) = End i
initpath (Straight _ _ (End i)) = End i
initpath (Node c i p) = Node c i $ initpath p
initpath (Straight c i p) = Straight c i $ initpath p

plusPath :: Path -> Path -> Path
plusPath (End _) p = p
plusPath (Node c i (End _)) p = Node c i p
plusPath (Straight c i (End _)) p = Straight c i p
plusPath (Node c i p1) p2 = Node c i $ plusPath p1 p2
plusPath (Straight c i p1) p2 = Straight c i $ plusPath p1 p2

reversePath :: Path -> Int -> Path
reversePath p i = case (lastofpath p) of Node c j _ -> Node c j rec
                                         Straight c j _ -> Straight c j rec
                                         End _ -> End i
    where rec = reversePath (initpath p) i

getPaths :: Int -> Int -> [(Char,Int)] -> [Maybe Path]
getPaths i j [] = if i == j then [Just $ End j] else [Nothing]
getPaths i j ((c,k):cks) = case abs (k - i) of 1 -> (ladder) ++ (bump) ++ (straight)
                                               0 -> [Nothing]
                                               _ -> straight
    where ladder = (maybe [] id $ do 
                   { pck <- si_path k i j cks
                   ; let ps = adjoin k pck
                   ; return ps
                   })
          bump = let bp = catMaybes $ bump_path k i j cks
                  in concat $ map (adjoin i) bp
          adjoin = (\x (pat,cks') -> map (\mp -> mp >>= (\p'-> return $ Node (c,k) k $ plusPath pat p')) $ getPaths x j cks')
          straight = map (\p -> p >>= (Just . Straight (c,k) [i])) (getPaths i j cks)

si_path :: Int -> Int -> Int -> [(Char,Int)] -> Maybe (Path,[(Char,Int)])
si_path _x _i _j [] = Nothing
si_path x i j ((c,k):cks)
    | k == i = Just (Node (c,k) x (End i),cks)
    | k == x = Nothing
    | otherwise = (si_path x i j cks) >>= (\(p,cks') -> Just (Straight (c,k) [i] p,cks'))

bump_path :: Int -> Int -> Int -> [(Char,Int)] -> [Maybe (Path,[(Char,Int)])]
bump_path _ _ _ [] = [Nothing]
bump_path x i j ((c,k):cks)
    | k == i = [Nothing]
    | k == x  = [Just (Node (c,k) i (End i),cks)]
    | k == x' = straight ++ to_x'
    | otherwise = straight
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


getDisk :: Path -> Path -> (Char,Int) -> (Char,Int) -> Maybe Holomorphic_Disk
getDisk p1 p2 (c1,i) (c2,j)
    | c1 == c2 = Nothing
    | (fromEnum c1) > (fromEnum c2) = Nothing
    | p1 == p2 = output
    | n1 `intersect` n2 /= [] = Nothing
    | p1 `above` p2 && p2 `above` p1 = output
    | p2 `above` p1 = getDisk p2 p1 (c1,i) (c2,j)
    | otherwise = output 
    where n1 = getNodes p1
          n2 = getNodes p2
          s1 = {-filter (\(s,_) -> not $ s `elem` n2) $-} getStraights p1
          s2 = {-filter (\(s,_) -> not $ s `elem` n1) $-} getStraights p2
          aboves = map (fst . fst) $ filter (\((_,i),j) -> or $ map (\j' -> i == j'-1) j) s1 -- negative points adjacent to the upper straights
          belows = map (fst . fst) $ filter (\((_,i),j) -> or $ map (\j' -> i == j'+1) j) s2 -- negative points adjacent to the lower straights
          output = Just $ Script_M [c1,c2] $ belows ++ (reverse aboves)

augmentationDisks :: Braid -> Char -> Char -> [Holomorphic_Disk]
augmentationDisks b p q
    | p < q = disks
    | p > q = map (\(Script_M po ne) -> Script_M (reverse po) (reverse ne)) $ augdisks b q p
    | otherwise = []
    where i = (fromEnum p) - 65 -- convert char to int and control for offset
          j = (fromEnum q) - 65
          pth = (word b) !! i
          qth = (word b) !! j
          fra = initSafe $ tailSafe $ map (\x -> ((toEnum $ x + 65),abs $ (word b) !! x)) [i..j]
          paths = catMaybes $ getPaths pth qth fra
          disks = nub $ catMaybes $ concat $ map (\p1 -> map (\p2 -> getDisk p1 p2 (p,pth) (q,qth)) paths) paths

above :: Path -> Path -> Bool
above (End _) (End _) = True
above (Node _ i p1) (Node _ j p2) = i <= j && p1 `above` p2
above (Straight _ i p1) (Straight _ j p2) = (or $ concat $ map (\i' -> map (\j' -> i' <= j') j) i) && p1 `above` p2
above (Straight _ i p1) (Node _ j p2)   = (or $ map (\i' -> i' <= j) i) && p1 `above` p2
above (Node _ i p1) (Straight _ j p2)   = (or $ map (\j' -> i <= j') j) && p1 `above` p2

interior :: Path -> Path -> Maybe [(Char,Int)]
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
