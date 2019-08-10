{-# LANGUAGE TypeFamilies #-}
module Augmentation.Braid
    (AugBraid (AugBraid)
    ,relations
    ,sChar
    ,s
    ,relations'
    ) where

import Algebra
import Augmentation.DGA
import Braid.Class
import Libs.List

import Data.List
import Data.Either
import Data.Maybe
import Control.Monad
import Data.Functor.Identity

import Numeric.LinearAlgebra

import Debug.Trace

sChar :: Int -> Char
sChar 0 = error "s 0"
sChar k = toEnum $ (2*k)+459

invsChar :: Char -> Int
invsChar c = if fromEnum c > 459 then (fromEnum c - 459) `div` 2 else error "invsChar: c out of bounds"

s :: Int -> Algebra
s = G . E . sChar
{-
sVec :: Int -> Vector R
sVec i = i |> [if j == (i-1) then 1 else 0 | j <- [1..]]
-}
bph :: StdBraid -> Int -> [Int]
bph b@(StdBraid k w) i = nub $ sort $ take k $ iterate (bphh w) i

bphh :: [Int] -> Int -> Int
bphh [] i = i
bphh (w:ws) i
    | w == i = bphh ws (i+1)
    | w == (i-1) = bphh ws (i-1)
    | otherwise = bphh ws i

extractChar :: Algebra -> Maybe Char
extractChar (G (E c)) = Just c
extractChar e = Nothing

sVec :: Int -> Int -> Vector Z
sVec dim i = dim |> [toEnum $ if j == i then 1 else 0 | j <- [1..]]

eqVec :: StdBraid -> Int -> Maybe (Vector Z)
eqVec (StdBraid w ws) i = do
                            { let dim = length ws
                            ; let v = sum $ zipWith (\w x -> if w == i then sVec dim x else if w == (i-1) then negate $ sVec dim x else 0) ws [1..]
                            ; if i > w then Nothing else Just v
                            }

relations' :: StdBraid -> Maybe (Matrix Z)
relations' b@(StdBraid w ws) = do
                                { let basePoints = map head $ nub $ map sort $ map (bph b) [1..w]
                                ; let dim = length ws
                                ; vs <- mapM (eqVec b) $ filter (\i -> not $ i `elem` basePoints) [1..w]
                                ; let mat = fromColumns vs 
                                ; let (q,_) = thinQR $ (fromZ mat :: Matrix R)
                                ; let qqt = q Numeric.LinearAlgebra.<> (tr q)
                                ; n <- (\(a,b) -> if a == b then Just a else Nothing) $ size qqt
                                ; return $ fromColumns $ map (toZ . roundVector) $ toColumns $ 2 * ((ident n) - qqt)
                                }

{-
addVec :: Vector R -> Vector R -> Vector R
addVec v1 v2
    | diff <  0 = addVec v2 v1
    | diff == 0 = v1 + v2
    | otherwise = v1 + (fromList $ toList v2 ++ zers)
    where diff = (size v1) - (size v2)
          zers = take diff repeat 0

eqVec :: StdBraid -> Int -> Maybe (Vector R)
eqVec (StdBraid w ws) i = do
                            { let v = sum $ zipWith (\w x -> if w == i then sVec x else if w == (i-1) then negate $ sVec x else fromInteger 0) ws [1..]
                            ; if i > w then Nothing else Just v
                            }

relations' :: StdBraid -> Maybe (Matrix R)
relations' b@(StdBraid w ws) = do
                                { let vecs = map (eqVec b) [1..w]
                                ; dim <- if [V.length $ head vecs] == (nub $ map V.length vecs) then Just $ V.length $ head vecs else Nothing
                                ; let mat = fmap fromIntegral $ M.transpose $ foldr (\x m -> (colVector x) <|> m) dim vecs
                                ; let zer = zeros (nrows mat) (ncols mat)
                                ; (_,l,p,_) <- luDecomp mat
                                ; 
                                    
-}
eqh :: StdBraid -> Int -> Maybe (DGA_Map,DGA_Map,DGA_Map,DGA_Map)
eqh (StdBraid _ ws) i = do
                            { let word = zipWith (\w x ->
                                        if w == i
                                            then s x
                                            else if w == (i-1) then recip $ s x
                                                               else 1) ws [1..]
                            ; chars <- mapM (\(x,t) ->
                                    (extractChar x) >>= (\x' -> return (x',t))) $ catMaybes $ map (\(x,t) ->
                                        if x == i then Just (s t,True) else if x == i-1 then Just (s t,False) else Nothing) $ zip ws [1..]
                            ; let (s1,r1) = head chars
                            ; let (s2,r2) = last chars
                            ; let tword = tail word
                            ; let iword = init word
                            ; let prod l = if l == [] then 1 else (head l) * (prod $ tail l)
                            ; let g1 = if r1 then recip else id
                            ; let g2 = if r2 then recip else id
                            ; let m11 = DGA_Map [(s1,g1 $ prod tword)]
                            ; let m12 = DGA_Map [(s1,g1 $ prod $ reverse $ tword)]
                            ; let m21 = DGA_Map [(s2,g2 $ prod iword)]
                            ; let m22 = DGA_Map [(s2,g2 $ prod $ reverse $ iword)]
                            ; return (m11,m12,m21,m22)
                            }

nst :: Int -> (a,a,a,a) -> Maybe a
nst 0 (a,_,_,_) = return a
nst 1 (_,a,_,_) = return a
nst 2 (_,_,a,_) = return a
nst 3 (_,_,_,a) = return a
nst _ _ = Nothing

relations :: StdBraid -> Maybe [DGA_Map]
relations b@(StdBraid w _) = do
                                { let basePoints = map head $ nub $ map sort $ map (bph b) [1..w]
                                ; let solutions = catMaybes $ map (eqh b) $ filter (\x -> not $ x `elem` basePoints) [1..w]
                                --; let toPerm l = foldr (\x xs -> (x `mod` 4) * (4^(length xs)) + xs) l
                                ; let fromPerm m n = if m == 0 then [] else (fromPerm (m-1) ((n - (n `mod` 4)) `div` 4)) ++ [n `mod` 4]
                                ; let perms = map (fromPerm (length solutions)) [0..(4^(length solutions))-1]
                                ; let maps = nub $ catMaybes $ map (\l -> do
                                                    { ms <- zipWithM nst l solutions
                                                    ; let none = DGA_Map []
                                                    ; let left = foldl compose_maps none ms
                                                    ; let right = foldr compose_maps none ms
                                                    ; if left == right && (and $ map (\x -> x == compose_maps x x) ms) then return right else Nothing
                                                    }) perms
                                ; return maps
                                }
{-
eqh :: StdBraid -> Int -> Maybe ((Char,(Algebra,Algebra)),(Char,(Algebra,Algebra)))
eqh b@(StdBraid _ ws) i = do
                            { let word = zipWith (\w x -> if w == i
                                    then s x
                                    else if w == (i-1) then recip $ s x
                                                       else 1) ws [1..]
                            ; c0 <- mapM (\(x,t) -> if x == i then Just (s t,False) else if x == i-1 then Just (s t,True) else Nothing) $ filter (\(x,_) -> (x == i) || (x == i-1)) $ zip ws [1..]
                            ; chars <- mapM (\(x,b) -> (extractChar x) >>= (\x' -> return $ (x',b))) c0
                            ; let (s1,r1) = head chars
                            ; let (s2,r2) = last chars
                            ; let tword = tail word
                            ; let iword = init word
                            ; let pre = ((s1,(product $ tword, product $ reverse $ tword)),(s2,(product $ iword,product $ reverse $ iword)))
                            ; let g1 = if r1 then recip else id
                            ; let g2 = if r2 then recip else id
                            ; let f ((c1,(e11,e12)),(c2,(e21,e22))) = ((c1,(g1 e11,g1 e12)),(c2,(g2 e21, g2 e22)))
                            ; return $ f pre
                            }

relations :: StdBraid -> Maybe [DGA_Map]
relations b@(StdBraid w _) = do
                                { let basePoints = map head $ nub $ map sort $ map (bph b) [1..w]
                                ; solutions <- mapM (eqh b) $ filter (\x -> not $ x `elem` basePoints) [1..w]
                                ; let solution = catMaybes $ map (\((c1,(a11,a12)),(c2,(a21,a22))) -> if c2 == c1 then Nothing else Just [(c1,a11),(c1,a12),(c2,a21),(c2,a22)]) solutions
                                ; let splited = map (\x -> compose_maps x x) $ map (\x -> DGA_Map $ zipWith (\l x' -> l !! x') solution x) $ permutations [0..3]
                                ; let fixed = nub $ filter (\x -> x == (compose_maps x x)) splited
                                ; return fixed
                                }
-}
buildMaph :: Maybe [(Char,Algebra)] -> Maybe [(Char,Algebra)] -> Maybe [(Char,Algebra)]
buildMaph m m'
    | m == m' = m
    | otherwise = buildMaph (m >>= (\l -> Just $ map (\(c,x) -> (c,applyDGAMap (DGA_Map l) x)) l)) m

footprinth :: Int -> [Either (Int,Char) Int] -> [(FreeGroup,Int)]
footprinth _k [] = []
footprinth k ((Right i):xs) = (E $ toEnum k,i):(footprinth (k+1) xs)
footprinth k ((Left (i,c)):xs) = [(E c,i-1),(invert $ E c,i+1)] ++ (footprinth (k+1) xs)

iscrossh :: [Either (Int,Char) Int] -> Int -> Bool
iscrossh [] _ = False
iscrossh ((Left _):_) 0 = False
iscrossh ((Left _):_) 1 = False
iscrossh ((Right _):_) 0 = True
iscrossh ((Right _):cs) x = iscrossh cs (x-1)
iscrossh ((Left _):cs) x = iscrossh cs (x-2)
 
data AugBraid = AugBraid Int [Either (Int,Char) Int] -- Either element of H1(L), and its position or an integer representing the corresponding braid group element
instance Braid AugBraid where
    type M AugBraid = Either (Int,Char)
    get_word (AugBraid _ w) = w
    algebra_footprint (AugBraid _ w) = map (\(g,i) -> (G g,i)) $ footprinth 65 w
    toStdBraid (AugBraid w ws) = StdBraid w (rights ws)
    fromStdBraid (StdBraid w ws) = AugBraid w (map Right ws)
    isCross (AugBraid _ ws) x = iscrossh ws x
    cross_art b row (Right s) = cross_art (toStdBraid b) row (Identity s)
    cross_art _b row (Left (s,s')) = let s1 = head $ show $ E s'
                                         s2 = show $ invert $ E s'
                                      in if (row - (abs $ s-1)*3) `elem` [0..3]
                        then Just $ (case ((row-(s-1)*3) `mod` 4) of 0 -> "---" ++ [s1] ++ "----"
                                                                     1 -> "   " ++  " " ++ "    "
                                                                     2 -> "   " ++  " " ++ "    "
                                                                     3 -> "--" ++ s2 ++ "---")
                        else Nothing
instance Eq AugBraid where
    (==) = equal
instance Show AugBraid where
    show = showBraid
