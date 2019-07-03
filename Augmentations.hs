module Augmentations
    (pinch
    ,torus_pinch
    ,CE_DGA (CE_DGA)
    ,Moduli_Disk (Script_M)
    ,legendrian_alphabet
    ,augdisks
    ,getPaths
    ,getDisk
    ,above
    ,DGA_Map (DGA_Map)
    ,applyDGAMap
    ,compose_maps
    ) where

import Algebra
import Data.List
import Data.Maybe
import Control.Monad
import Braid
import Libs.List

import Safe
import Debug.Trace

infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
l  !? 0 = Just $ head l
l  !? n = (tail l) !? (n-1)

pinch :: Int -> Braid -> Braid -- Execute a Lagrangian pinch move on a braid at a crossing whose number is determined by position in the braid word (1-indexed)
pinch _ (Braid l []) = Braid l []
pinch 0 (Braid l (k:ks)) = Braid l ks
pinch i (Braid l (k:ks))
    | i < 0 = Braid l (k:ks)
    | otherwise = Braid l (k:(word $ pinch (i-1) (Braid l ks)))

torus_pinch :: Int -> Int -> Braid -> Braid -- Execute a Lagrangian pinch on a torus braid at the specified coordinates (1-indexed)
torus_pinch i j k = pinch ((j-1) + (i-1)*((width k) -1)) k

data CE_DGA = CE_DGA {generators::[Char], grading::Char -> Int, differential::Char -> Expression}
data Moduli_Disk = Script_M {pos::[Char],neg::[Char]} deriving (Show,Eq) -- J-Holomorphic moduli disk with #pos + #neg points removed from the boundary

data Path = Node (Char,Int) Path
          | Straight (Char,Int) Int Path
          | End Int
instance Show Path where
    show (End _) = "End"
    show (Node c p) = "N " ++ show c ++ "|"++show p
    show (Straight c i p) = "S " ++ show c ++ " " ++ show i ++ "|"++show p
instance Eq Path where
    (End i) == (End j) = i == j
    (Node c1 p1) == (Node c2 p2) = c1 == c2 && p1 == p2
    (Straight c1 i1 p1) == (Straight c2 i2 p2) = c1 == c2 && i1 == i2 && p1 == p2
    _ == _ = False

getNodes :: Path -> [(Char,Int)]
getNodes (Node c p) = c:(getNodes p)
getNodes (End _) = []
getNodes (Straight _ _ p) = getNodes p

getStraights :: Path -> [((Char,Int),Int)]
getStraights (Node c p) = getStraights p
getStraights (End _) = []
getStraights (Straight c j p) = (c,j):(getStraights p)

lastofpath :: Path -> Path
lastofpath (End i) = End i
lastofpath (Node c (End i)) = Node c (End i)
lastofpath (Straight c j (End i)) = Straight c j (End i)
lastofpath (Node _ p) = lastofpath p
lastofpath (Straight _ _ p) = lastofpath p

initpath :: Path -> Path
initpath (End i) = End i
initpath (Node _ (End i)) = End i
initpath (Straight _ _ (End i)) = End i
initpath (Node c p) = Node c $ initpath p
initpath (Straight c i p) = Straight c i $ initpath p

reversePath :: Path -> Int -> Path
reversePath p i = case (lastofpath p) of Node c _ -> Node c rec
                                         Straight c j _ -> Straight c j rec
                                         End _ -> End i
    where rec = reversePath (initpath p) i

getPaths :: Int -> Int -> [(Char,Int)] -> [Maybe Path]
getPaths i j [] = if i == j then [Just $ End j] else [Nothing]
getPaths i j ((c,k):cks) = case abs (k - i) of 1 -> (map (\p -> p >>= (Just . Node (c,k)))       (getPaths k j cks))     ++ (map (\p -> p >>= (Just . Straight (c,k) i)) (getPaths i j cks    ))
                                               0 -> (map (\p -> p >>= (Just . Node (c,k)))       (getPaths (i+1) j cks)) ++ (map (\p -> p >>= (Just . Node (c,k)))       (getPaths (i-1) j cks))
                                               _ ->  map (\p -> p >>= (Just . Straight (c,k) i)) (getPaths i j cks)

getDisk :: Path -> Path -> (Char,Int) -> (Char,Int) -> Maybe Moduli_Disk
getDisk p1 p2 (c1,i) (c2,j)
    | c1 == c2 = trace "Start and end are the same" Nothing
    | (fromEnum c1) > (fromEnum c2) = (getDisk (reversePath p1 i) (reversePath p2 i) (c2,j) (c1,i)) >>= (\d -> case d of Script_M p n -> return $ Script_M (reverse p) (reverse n))
    -- | p1 == p2 = output
    | n1 `intersect` n2 /= [] = trace "Has Shared Nodes" Nothing
    | n1 `intersect` (map fst s2) /= [] = trace "n1^s2/=[]" Nothing
    | n2 `intersect` (map fst s1) /= [] = trace "n2^s1/=[]" Nothing
    | (maybe True (not.null) $ interior p1 p2) = trace "Has interior" Nothing
    | p1 `above` p2 && p2 `above` p1 = output
    | p2 `above` p1 = getDisk p2 p1 (c1,i) (c2,j)
    | otherwise = output 
    where n1 = getNodes p1
          n2 = getNodes p2
          s1 = filter (\(s,_) -> not $ s `elem` n2) $ getStraights p1
          s2 = filter (\(s,_) -> not $ s `elem` n1) $ getStraights p2
          aboves = map (fst . fst) $ filter (\((_,i),j) -> i == j-1) s1 -- negative points adjacent to the upper straights
          belows = map (fst . fst) $ filter (\((_,i),j) -> i == j+1) s2 -- negative points adjacent to the lower straights
          output = Just $ Script_M [c1,c2] $ belows ++ (reverse aboves)

augdisks :: Braid -> Char -> Char -> [Moduli_Disk]
augdisks b p q = trace ("Disk_"++[p,q]++": (p,pth)="++show (p,pth)++" (q,qth)="++show (q,qth)++" fra="++show fra) $
                    nub $ catMaybes $ (\ps -> concat $ map (\p1 -> map (\p2 -> getDisk p1 p2 (p,pth) (q,qth)) ps) ps) $ catMaybes $ getPaths pth qth fra
    where i = (fromEnum p) - 65 -- convert char to int and control for offset
          j = (fromEnum q) - 65
          pth = (word b) !! i
          qth = (word b) !! j
          fra = initSafe $ tailSafe $ map (\x -> ((toEnum $ x + 65),abs $ (word b) !! x)) [i..j]

above :: Path -> Path -> Bool
above (End _) (End _) = True
above (Node _ p1) (Node _ p2) = p1 `above` p2
above (Straight _ i p1) (Straight _ j p2) = i > j && p1 `above` p2
above (Straight _ i p1) (Node (_,j) p2)   = p1 `above` p2
above (Node (_,i) p1) (Straight _ j p2)   = p1 `above` p2

interior :: Path -> Path -> Maybe [(Char,Int)]
interior (End i) (End j) = if i == j then Just [] else Nothing
interior (End _) _ = Nothing
interior _ (End _) = Nothing
interior (Node _ p1) (Node _ p2) = interior p1 p2
interior (Straight (c,j) i p1) (Straight (c',j') i' p2) = let l = initSafe $ tailSafe $ if i' > i then [i..i'] else [i'..i]
                                                           in (\col -> return $ (if j `elem` l then [(c,j)] else []) ++ (if j' `elem` l then [(c',j')] else []) ++ col) =<< interior p1 p2
interior (Node (_,i) p1) (Straight (c,j) i' p2)         = let l = initSafe $ tailSafe $ if i' > i then [i..i'] else [i'..i]
                                                           in (\col -> return $ (if j `elem` l then [(c,j)] else []) ++ col) =<< interior p1 p2
interior (Straight c1 i p1) (Node c2 p2) = interior (Node c2 p2) (Straight c1 i p1)

legendrian_alphabet :: Braid -> [Char] -- assigns unicodde characters to each crossing in increasing order
legendrian_alphabet b = alph b 65

alph :: Braid -> Int -> [Char] -- Helper for legendrian_alphabet
alph (Braid _ []) _ = [] -- skip loopy bois for conveinience
--alph (Braid 0 []) _ = []
--alph (Braid w []) k = (toEnum k):(alph (Braid (w-1) []) (k+1)) -- loopy bois
alph (Braid w (c:cs)) k = (toEnum k):(alph (Braid w cs) (k+1)) -- crossings

data DGA_Map = DGA_Map [(Char,Expression)]

compose_maps :: DGA_Map -> DGA_Map -> DGA_Map
compose_maps (DGA_Map map1) (DGA_Map map2) = DGA_Map $ (map (\(c,exp) -> (c,applyDGAMap (DGA_Map map2) exp)) map1) ++ (filter (\(c,_) -> not $ elem c $ map fst map1) map2)

applyDGAMap :: DGA_Map -> Expression -> Expression
applyDGAMap (DGA_Map alist) (Expression ex) = sum $ map (\m -> (appmaph alist m)) ex

appmaph::[(Char,Expression)] -> Monomial -> Expression
appmaph [] m = Expression [m]
appmaph alist (Monomial c ms) = foldl (\x m -> x * (maybe (Expression [(Monomial 1 [m])]) id $ lookup m alist)) (fromInteger c) ms
