module Main
    (main
    ) where

import Libs.Graph
import Augmentation.Pinch
import Augmentation.Braid
import Augmentation.Graph
import qualified Augmentation.Tree as A
import Augmentation.Disks
import Algebra
import Algebra.DGA
import Braid
import Braid.GenBraid
import Control.Monad.Random
import qualified System.IO
import Data.List
import Data.Maybe
import Data.Tree
import Data.Functor.Identity

b0 :: AugBraid
b0 = genTorusBraid 3 5

{--5 3
--b1 = pinch 1 b0
m1 = DGA_Map [('A',Expression [Monomial 1 "\564"])
             ,('E',Expression [Monomial 1 "E",Monomial 1 "\565B"])
             ,('F',Expression [Monomial 1 "F",Monomial 1 "\565C"])
             ,('G',Expression [Monomial 1 "G", Monomial 1 "\565D"])
             ,('H',Expression [Monomial 1 "H",Monomial 1 "\565"])]
--b2 = pinch 1 b1
m2 = DGA_Map [('B',Expression [Monomial 1 "\568"])
             ,('F',Expression [Monomial 1 "F",Monomial 1 "\569EC"])
             ,('G',Expression [Monomial 1 "G",Monomial 1 "\569ED"])
             ,('H',Expression [Monomial 1 "H",Monomial 1 "\569E"])
             ,('I',Expression [Monomial 1 "I",Monomial 1 "\569C"])]
--b3 = pinch 1 b2
m3 = DGA_Map [('C',Expression [Monomial 1 "\571"])
             ,('G',Expression [Monomial 1 "G",Monomial 1 "\572DF"])
             ,('J',Expression [Monomial 1 "J",Monomial 1 "\572DI"])]
map1' = (applyDGAMap m1)
map2' = (applyDGAMap $ compose_maps m1 m2)
map3' = (applyDGAMap $ compose_maps m1 $ compose_maps m2 m3)
(m1',b1) = fromJust $ pinchMap 0 b0
(m2',b2) = fromJust $ pinchMap 0 b1
(m3',b3) = fromJust $ pinchMap 0 b2
map1p = applyDGAMap m1'
map2p = applyDGAMap $ compose_maps m1' m2'
map3p = applyDGAMap $ compose_maps m1' $ compose_maps m2' m3'
--}
{--
--2 3
m123 = DGA_Map [('A',Expression [Monomial 1 "\461"])
               ,('B',Expression [Monomial 1 "\464",Monomial 1 "\462"])
               ,('C',Expression [Monomial 1 "\467",Monomial 1 "\465"])]
m123' = fromJust $ do
            { (m1,b1) <- pinchMap 0 b0
            ; (m2,b2) <- pinchMap 0 b1
            ; (m3,b3) <- pinchMap 0 b2
            ; return $ compose_maps m1 $ compose_maps m2 m3
            }
m132 = DGA_Map [('A',Expression [Monomial 1 "\461"])
               ,('B',Expression [Monomial 1 "\464",Monomial 1 "\462",Monomial 1 "\468"])
               ,('C',Expression [Monomial 1 "\467"])]
m132' = fromJust $ do
            { (m1,b1) <- pinchMap 0 b0
            ; (m2,b2) <- pinchMap 1 b1
            ; (m3,b3) <- pinchMap 0 b2
            ; return $ compose_maps m1 $ compose_maps m2 m3
            }
m312 = m132
m312' = fromJust $ do
            { (m1,b1) <- pinchMap 2 b0
            ; (m2,b2) <- pinchMap 0 b1
            ; (m3,b3) <- pinchMap 0 b2
            ; return $ compose_maps m1 $ compose_maps m2 m3
            }
m213 = DGA_Map [('A',Expression [Monomial 1 "\461",Monomial 1 "\465"])
               ,('B',Expression [Monomial 1 "\464"])
               ,('C',Expression [Monomial 1 "\467",Monomial 1 "\462",Monomial 1 "\465"])]
m213' = fromJust $ do
            { (m1,b1) <- pinchMap 1 b0
            ; (m2,b2) <- pinchMap 0 b1
            ; (m3,b3) <- pinchMap 0 b2
            ; return $ compose_maps m1 $ compose_maps m2 m3
            }
m231 = DGA_Map [('A',Expression [Monomial 1 "\461",Monomial 1 "\465",Monomial 1 "\468"])
               ,('B',Expression [Monomial 1 "\464"])
               ,('C',Expression [Monomial 1 "\467",Monomial 1 "\465"])]
m231' = fromJust $ do
            { (m1,b1) <- pinchMap 1 b0
            ; (m2,b2) <- pinchMap 1 b1
            ; (m3,b3) <- pinchMap 0 b2
            ; return $ compose_maps m1 $ compose_maps m2 m3
            }
m321 = DGA_Map [('A',Expression [Monomial 1 "\461",Monomial 1 "\465"])
               ,('B',Expression [Monomial 1 "\464",Monomial 1 "\468"])
               ,('C',Expression [Monomial 1 "\467"])]
m321' = fromJust $ do
            { (m1,b1) <- pinchMap 2 b0
            ; (m2,b2) <- pinchMap 1 b1
            ; (m3,b3) <- pinchMap 0 b2
            ; return $ compose_maps m1 $ compose_maps m2 m3
            }
--}
alph = map (\(c,_) -> Expression [Monomial 1 [c]]) $ algebra_footprint b0

showlh [] = "]"
showlh [a] = a ++ "]"
showlh (a:as) = a ++ "," ++ showlh as
{-
tree = pinchTree b0
printableTree = fmap (\(DGA_Map m,b) -> ((++) (show b) $ (++) "[" $ showlh $ map (\(c,s) -> "("++[c]++","++show s++")") m) {-++ "\n\n" ++ (show b)-}) tree
uniqs = (++) "[" $ showlh $ map (\s -> s ++ "\n") $ nub $ leaves printableTree
thing = drawTree printableTree
leafyBois = mapM_ (\l -> print l) $ nub $ map (\(m,b) -> map (applyDGAMap m) alph) $ leaves tree

lefnum = length $ nub $ leaves printableTree
numbers = numAugmentations b0
uniques = mapM_ print $ getUniques b0
-}
fac 0 = 1
fac n = n * (fac $ n-1)
catalan  n = div (fac $ 2*n) $ (fac $ n+1) * (fac n)
numAug m n = numAugmentations $ genTorusBraid m n
{-
out = outh [2..5] 3
outh::[Int]->Int->IO ()
outh [] _ = ""
outh (x:xs) m = ((++) (show x ++ ": , ") $ foldr (\s ss -> (show s) ++ ", " ++ ss) "\n" $ map (numAug x) [1..m]) ++ (outh xs m)
-}
putInFile :: [Int] -> Int -> IO ()
putInFile (i:is) n = do { let ks = map (numAug i) [1..n]
                        ; mapM_ (\k -> appendFile "Data.csv" $ show k ++ ",") ks
                        --; print ks
                        ; appendFile "Data.csv" "\n"
                        ; putInFile is n
                        } 

numeach :: LevelGraph a -> [Int]
numeach (Leaf as) = [length as]
numeach (Level ans lg) = (length ans):(numeach lg)

fileperms m n = mapM_ (\x -> appendFile ("data/" ++ show m ++ "," ++ show n ++ "TorusPerms") (show x ++ "\n")) $ A.equivPerms $ A.pinchTree $ genTorusBraid m n

main = do { print "Running" 
          ; appendFile "Data.csv" "\n"
          ; putInFile [4,5] 3
          --; mapM_ (fileperms 2) [1..5]
          --; mapM_ (fileperms 3) [1..5]
          --; mapM_ (fileperms 4) [1..3]
          --; mapM_ (fileperms 5) [1..3]
          -- putInFile [2..5] 5
          --; putStrLn out
          --; writeFile "data.csv" out
          --; putStrLn "Test Braid and leaves"
          --; putStrLn thing
          --; uniques
          --; putStrLn "Test Cases"
          --; putStrLn "123"
          --; mapM_ putStrLn $ map (\x -> (show x) ++ ":" ++ show (applyDGAMap m123 x) ++ "\t\t\t" ++ show (applyDGAMap m123' x)) alph
          --; putStrLn "132"
          --; mapM_ putStrLn $ map (\x -> (show x) ++ ":" ++ show (applyDGAMap m132 x) ++ "\t\t\t" ++ show (applyDGAMap m132' x)) alph
          --; putStrLn "312"
          --; mapM_ putStrLn $ map (\x -> (show x) ++ ":" ++ show (applyDGAMap m312 x) ++ "\t\t\t" ++ show (applyDGAMap m312' x)) alph
          --; putStrLn "213"
          --; mapM_ putStrLn $ map (\x -> (show x) ++ ":" ++ show (applyDGAMap m213 x) ++ "\t\t\t" ++ show (applyDGAMap m213' x)) alph
          --; putStrLn "231"
          --; mapM_ putStrLn $ map (\x -> (show x) ++ ":" ++ show (applyDGAMap m231 x) ++ "\t\t\t" ++ show (applyDGAMap m231' x)) alph
          --; putStrLn "321"
          --; mapM_ putStrLn $ map (\x -> (show x) ++ ":" ++ show (applyDGAMap m321 x) ++ "\t\t\t" ++ show (applyDGAMap m321' x)) alph
          --; print b0
          --; putStrLn "Bad Guess:"
          --; print lefnum
          --; putStrLn "Worse Guess:"
          --; print numbers
          --; leafyBois
          --; putStrLn "2,n Experiment"
          --; let n = 10
          --; let aug = map (numAug 2) [1..10]
          --; let cat = map catalan [1..10] 
          --; putStrLn "Upper Bound"
          --; print $ map fac [1..10]
          --; putStrLn "Program"
          --; print aug 
          --; putStrLn "Target"
          --; print cat 
          --; putStrLn "Same?"
          --; print $ aug == cat
          --; putStrLn "Test Cases"
          --; print b1
          --; mapM_ (\c -> putStrLn $ (show $ map1' c) ++ "\t" ++ (show $ map1p c)) alph
          --; print b2
          --; mapM_ (\c -> putStrLn $ (show $ map2' c) ++ "\t" ++ (show $ map2p c)) alph
          --; print b3
          --; mapM_ (\c -> putStrLn $ (show $ map3' c) ++ "\t" ++ (show $ map3p c)) alph
          }
{-
main = do { System.IO.hSetEncoding System.IO.stdout System.IO.utf8
          ; putStrLn "Step 1"
          ; mapM_ (print . map1) alph
          ; putStrLn "Step 2"
          ; mapM_ (print . map2) alph
          ; putStrLn "Step 3"
          ; mapM_ (print . map3) alph
          ; putStrLn "Step 1"
          ; mapM_ (print . map1') alph
          ; putStrLn "Step 2"
          ; mapM_ (print . map2') alph
          ; putStrLn "Step 3"
          ; mapM_ (print . map3') alph
          }
-}
