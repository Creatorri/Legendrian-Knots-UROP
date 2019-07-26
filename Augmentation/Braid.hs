{-# LANGUAGE TypeFamilies #-}
module Augmentation.Braid
    (AugBraid (AugBraid)
    ,relations
    ,s
    ) where

import Algebra
import Braid.Class
import Libs.List

import Data.List
import Data.Either
import Data.Functor.Identity

s :: Int -> Algebra
s k = G $ E $ toEnum $ (2*k)+461

bph :: StdBraid -> Int -> [Int]
bph b@(StdBraid _ w) i = let i' = bphh w i
                          in if i' == i then [i] else nub $ i':(bph b i')

bphh :: [Int] -> Int -> Int
bphh [] i = i
bphh (w:ws) i
    | w == i = bphh ws (i+1)
    | w == (i-1) = bphh ws (i-1)
    | otherwise = bphh ws i

extractChar :: Algebra -> Maybe (Char,Bool)
extractChar (G (E c)) = Just (c,False)
extractChar e = case (recip e) of (G (E c)) -> Just (c,True)
                                  _ -> Nothing

eqh :: StdBraid -> Int -> Maybe ((Char,(Algebra,Algebra)),(Char,(Algebra,Algebra)))
eqh b@(StdBraid _ ws) i = do
                            { let word = zipWith (\w x -> if w == i
                                    then s x
                                    else if w == (i-1) then recip $ s x
                                                       else 1) ws [1..]
                            ; (s1,r1) <- extractChar $ head word
                            ; (s2,r2) <- extractChar $ last word
                            ; let tword = tail word
                            ; let iword = init word
                            ; let pre = ((s1,(product $ tword, product $ reverse $ tword)),(s2,(product $ iword,product $ reverse $ iword)))
                            ; let g1 = if r1 then recip else id
                            ; let g2 = if r2 then recip else id
                            ; let f ((c1,(e11,e12)),(c2,(e21,e22))) = ((c1,(g1 e11,g1 e12)),(c2,(g2 e21, g2 e22)))
                            ; return $ f pre
                            }

relations :: StdBraid -> Maybe DGA_Map
relations b@(StdBraid w _) = themap >>= (return . DGA_Map)
    where basePoints = map head $ nub $ map sort $ map (bph b) [1..w]
          solutions = mapM (eqh b) $ filter (\x -> not $ x `elem` basePoints) [1..w]
          solution = solutions >>= (Just . map (\((c,(a,_)),_) -> (c,a))) 
          themap = buildMaph solution Nothing

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
