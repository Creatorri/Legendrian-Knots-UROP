module Augmentations
    (pinch
    ,torus_pinch
    ,CE_DGA (CE_DGA)
    ,Moduli_Disk (Script_M)
    ,legendrian_alphabet
    ,augdisks
    ,augdisks2
    ,si_finder
    ,DGA_Map (DGA_Map)
    ,applyDGAMap
    ,compose_maps
    ) where

import Algebra
import Data.List
import Data.Maybe
import Control.Monad
import Braid.Braid
import Braid.WordProblem
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

--dim :: Moduli_Disk -> Int -- Calculates "dimension" of a moduli disk
--dim (Script_M (CE_DGA _ g _) pos neg) = (sum $ map (\x -> g x) pos) - (sum $ map (\x -> g x) neg) - 1 + (length pos)

augdisks :: Braid -> Char -> Char -> [Moduli_Disk]
augdisks b p q
    | p > q = map (\(Script_M pl ne) -> Script_M (reverse pl) (reverse ne)) $ augdisks b q p
    | p == q = []
    | pth == qth = map (\neg -> Script_M [p,q] neg) $ catMaybes $ rowdiskh pth fra
    | otherwise  = map (\neg -> Script_M [p,q] neg) $ catMaybes $ ladderdiskh (Left pth) qth fra
    where i = (fromEnum p) - 65 -- convert char to int and control for offset
          j = (fromEnum q) - 65
          pth = abs $ (word b) !! i
          qth = abs $ (word b) !! j
          fra = initSafe $ tailSafe $ map (\x -> ((toEnum $ x + 65),abs $ (word b) !! x)) [i..j]

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
    | c1 == c2 = Nothing
    | (fromEnum c1) > (fromEnum c2) = (getDisk (reversePath p1 i) (reversePath p2 i) (c2,j) (c1,i)) >>= (\d -> case d of Script_M p n -> return $ Script_M (reverse p) (reverse n))
    -- | p1 == p2 = output
    | n1 `intersect` n2 /= [] = Nothing
    | n1 `intersect` (map fst s2) /= [] = Nothing
    | n2 `intersect` (map fst s1) /= [] = Nothing
    | (interiorful p1 p2) = Nothing 
    | p1 `above` p2 && p2 `above` p1 = Nothing
    | p2 `above` p1 = getDisk p2 p1 (c1,i) (c2,j)
    | otherwise = output 
    where n1 = getNodes p1
          n2 = getNodes p2
          s1 = filter (\(s,_) -> not $ s `elem` n2) $ getStraights p1
          s2 = filter (\(s,_) -> not $ s `elem` n1) $ getStraights p2
          aboves = map (fst . fst) $ filter (\((_,i),j) -> i == j-1) s1 -- negative points adjacent to the upper straights
          belows = map (fst . fst) $ filter (\((_,i),j) -> i == j+1) s2 -- negative points adjacent to the lower straights
          output = Just $ Script_M [c1,c2] $ belows ++ (reverse aboves)

augdisks2 :: Braid -> Char -> Char -> [Moduli_Disk]
augdisks2 b p q = catMaybes $ (\ps -> trace (show ps) $ zipWith (\p1 p2 -> getDisk p1 p2 (p,pth) (q,qth)) ps ps) $ catMaybes $ getPaths pth qth fra
    where i = (fromEnum p) - 65 -- convert char to int and control for offset
          j = (fromEnum q) - 65
          pth = abs $ (word b) !! i
          qth = abs $ (word b) !! j
          fra = initSafe $ tailSafe $ map (\x -> ((toEnum $ x + 65),abs $ (word b) !! x)) [i..j]

above :: Path -> Path -> Bool
above (End _) (End _) = True
above (Node _ p1) (Node _ p2) = p1 `above` p2
above (Straight _ i p1) (Straight _ j p2) = i > j && p1 `above` p2
above (Straight _ i p1) (Node (_,j) p2)   = i > j && p1 `above` p2
above (Node (_,i) p1) (Straight _ j p2)   = i > j && p1 `above` p2

interiorful :: Path -> Path -> Bool
interiorful (End i) (End j) = i /= j
interiorful (End _) _ = True
interiorful _ (End _) = True
interiorful (Node _ p1) (Node _ p2) = interiorful p1 p2
interiorful (Straight (_,j) i p1) (Straight (_,j') i' p2) = let l = if i' > i then [i..i'] else [i'..i] in j `elem` l || j' `elem` l || interiorful p1 p2
interiorful (Node (_,i) p1) (Straight (_,j) i' p2)        = let l = if i' > i then [i..i'] else [i'..i] in j `elem` l || interiorful p1 p2
interiorful (Straight c1 i p1) (Node c2 p2) = interiorful (Node c2 p2) (Straight c1 i p1)

rowdiskh :: Int -> [(Char,Int)] -> [Maybe [Char]]
{-rowdiskh i fra = map (\con -> let poswatch = filter (>0) con
                                  negwatch = filter (<0) con
                               in rowdhh i i poswatch False
                     ) $ map (map (\c -> c - i)) consets
    where (mi,ma) = (\l -> ((min l) -1, (max l)+1)) $ map snd fra
          wildpermute n l = if n == length l then permutations l else (wildpermute (n+1) l) ++ (map (take n) $ permutations l)
          consets = filter (\l -> foldr (\j b -> b && j+1 `elem` l) l) $ filter (\l -> i `elem` l) $ nub $ map sort $ wildpermute 1 [mi..ma]   --all connected subsets of the width of the braid -}
rowdiskh p fra = if p `elem` (map snd fra) then [Nothing] else [Just $ (map fst $ filter (\(_,x) -> x == p + 1) fra) ++ (reverse $ map fst $ filter (\(_,x) -> x == p - 1) fra)]
--rowdiskh p fra = [map fst $ (filter (\(_,x) -> (abs x) - p == 1) fra) ++ (reverse $ filter (\(_,x) -> p - (abs x) == 1) fra)] ++ (concat $ map (\x -> triangediskh p [p] x fra) [mi..ma])
--    where (mi,ma) = (\l -> ((min l) - 1, (max l) + 1)) $ map snd fra

{- rowdhh :: Int -> Int -> [Int] -> Bool -> [(Char,Int)] -> [Maybe [(Char,Int)]]
rowdhh i j l b fra
    | i == j && b = [Just fra]
    | j == (last l) && (not b) = concat $ map (\l -> (rowdhh i jn True l $ (iterate tailSafe fra) !! ((length l)+1)) >>= (map (\l' -> l'++l))) $ map init $ filter (\l -> (snd $ last l) == jn) $ si_finder [i..j] fra
    | b = concat $ map (\l -> (rowdhh i jn True l $ (iterate tailSafe fra) !! ((length l)+1)) >>= (map (\l' -> l'++l ))) $ map init $ filter (\l -> (snd $ last l) == jn) $ si_finder [i..j] fra 
    | otherwise = concat $ map (\l -> (rowdhh i j' False l $ (iterate tailSafe fra) !! ((length l)+1)) >>= (map (\l' -> l++l' ))) $ map init $ filter (\l -> (snd $ last l) == j') $ si_finder [i..j] fra
    where j' = if i < head l then j+1 else j-1
          jn = if i < head l then j-1 else j+1 -}

--trianglediskh :: Int -> [Int] -> Int -> [(Char,Int)] -> [[Char]]
--trianglediskh i l h fra = 

ladderdiskh :: Either Int (Int,Int) -> Int -> [(Char,Int)] -> [Maybe [Char]]
ladderdiskh (Left i) j fra = if i == j then rowdiskh j fra else trace ("\n Left " ++ show i ++ ": " ++ show fra ++ "\n sip,sism=" ++ show sisp ++ ", " ++ show sism) $
        {-if sisp == [] && sism == []
           then [Nothing]
           else-} concat $ map (\(sis,i') -> if sis == [] then [Nothing] else concat $ map (\si ->
                let l = maybe [] (ladderdiskh (Right (i,i')) j) $ (iterate tailSafe fra) !? (length si)
                    si' = initSafe si
                 in map (\lm -> do
                              { l' <- lm
                              ; return $ (map fst $ filter (\(_,x) -> x == i + 1) si') ++ l' ++ (reverse $ map fst $ filter (\(_,x) -> x == i - 1) si')
                              }) l
                ) sis) $ [(sisp,i+1),(sisp,i-1)]
    where sif = si_finder [i] fra
          sisp = filter (\l -> if l == [] then False else (snd $ last l) == i+1) sif
          sism = filter (\l -> if l == [] then False else (snd $ last l) == i-1) sif
ladderdiskh (Right (i,i')) j fra = trace ("\n Right " ++ show i ++ ", " ++ show i' ++ ": " ++ show fra ++ "\n sisi,sisi'="++show sisi++", "++show sisi') $
        {-if sisi == [] && sisi' == []
          then [Nothing]
          else-} concat $ map (\(sis,i'') -> if sis == [] then [Nothing] else concat $ map (\si -> 
               let l = maybe [] (ladderdiskh (Left i'') j) $ (iterate tailSafe fra) !? ((length si)+1)
                in map (\lm -> do
                            { l' <- lm
                            ; return $ (map fst $ filter (\(_,x) -> x == i + 1) si) ++ l' ++ (reverse $ map fst $ filter (\(_,x) -> x == i - 1) si)
                            }) l
                ) sis) $ filter (\(l,_) -> not $ null l) $ [(sisi,i'),(sisi',i')]
    where sif   = si_finder [i,i'] fra
          first = (\l -> (\l' -> if l' == [] then -1 else snd $ head l') $ (iterate tailSafe fra) !! (length l))
          sisi  = filter (\l -> i  == first l) sif
          sisi' = filter (\l -> i' == first l) sif

si_finder :: [Int] -> [(Char,Int)] -> [[(Char,Int)]]
si_finder i word = sih i word (length word)

sih :: [Int] -> [(Char,Int)] -> Int -> [[(Char,Int)]]
sih i w len
    | i == [] = [w]
    | w == [] = []
    | len == 0 = [[]]
    | otherwise = (sih i w (len - 1)) ++ ((\l -> if (filter (\(_,x) -> not $ x `elem` i) l) == l then [l] else []) $ take len w) --map (\n -> (n,take len $ (iterate tailSafe w) !! n)) [0..(length w)-1]) 

--getDGA :: Braid -> CE_DGA
--getDGA b = CE_DGA (legendrian_alphabet b) (gradh b) (diffh b)

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
