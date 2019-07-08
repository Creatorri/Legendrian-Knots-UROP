module Main
    (main
    ) where

import Augmentations
import Augmentation.Disks
import Algebra
import Algebra.DGA
import Braid
import Braid.GenBraid
import Control.Monad.Random
import qualified System.IO
import Data.List
import Data.Maybe

pincher b = do{ k <- evalRandIO $ liftRand (randomR (1,length $ word b))
              ; cont <- getLine
              ; let b' = Braid (width b) $ word $ pinch k b
              ; if cont == "" then print b' else return ()
              ; if cont == "" then print k else return ()
              ; if cont == "" then pincher b' else return ()
              }

b0 = genTorusBraid 5 3
alphabet = legendrian_alphabet b0
b1 = torus_pinch 1 1
m1 = DGA_Map [('A',Expression [Monomial 1 "\564"]),('E',Expression [Monomial 1 "E",Monomial 1 "\565B"]),('F',Expression [Monomial 1 "F",Monomial 1 "\565C"]),('G',Expression [Monomial 1 "G", Monomial 1 "\565D"]),('H',Expression [Monomial 1 "H",Monomial 1 "\565"])]
b2 = torus_pinch 1 1
m2 = DGA_Map [('B',Expression [Monomial 1 "\568"]),('F',Expression [Monomial 1 "F",Monomial (-1) "\569EC"]),('G',Expression [Monomial 1 "G",Monomial (-1) "\569ED"]),('H',Expression [Monomial 1 "H",Monomial (-1) "\569E"]),('I',Expression [Monomial 1 "I",Monomial 1 "\569C"]),('J',Expression [Monomial 1 "J",Monomial 1 "\569D"]),('K',Expression [Monomial 1 "K",Monomial 1 "\569"])]
b3 = torus_pinch 1 1
m3 = DGA_Map [('C',Expression [Monomial 1 "\571"]),('G',Expression [Monomial 1 "G",Monomial (-1) "\572F"]),('J',Expression [Monomial 1 "J",Monomial (-1) "\572I"])]
map1 = applyDGAMap m1
map2 = (applyDGAMap m2) . map1
map3 = (applyDGAMap m3) . map2

printMap :: (Expression->Expression) -> Braid -> IO ()
printMap map' b = mapM_ putStrLn $ map (\(a,b')-> (show a) ++ " ↦ " ++ (show $ inZp 2 $ map' b')) $ map (\x -> (x,Expression [Monomial 1 [x]])) $ legendrian_alphabet b

test = do { b <- evalRandIO $ genRandPosBraid 5 12
          ; let alph = legendrian_alphabet b
          ; putStrLn $ show b
          ; putStrLn alph
          ; putStrLn $ show $ nub $ concat $ filter (not . null) $ concat $ map (\c -> map (\d -> augmentationDisks b c d) alph) alph
          }

gen = mkStdGen 7649764563452548
rb = (flip evalRand) gen $ genRandPosBraid 5 12
ralph = legendrian_alphabet rb
rdisk = nub $ concat $ filter (not . null) $ concat $ map (\c -> map (\d -> augmentationDisks rb c d) ralph) ralph

correct = nub $ (\l -> l ++ (map (\(Script_M p n) -> Script_M (reverse p) (reverse n)) l)) $
            [Script_M "AE" "BCD",Script_M "AE" "D",Script_M "AE" "B",Script_M "AF" "BC",Script_M "AF" ""
            ,Script_M "BC" ""
            ,Script_M "CD" ""
            ,Script_M "DF" "E",Script_M "DG" ""
            ,Script_M "EG" "F",Script_M "EH" ""
            ,Script_M "FH" "G"
            ,Script_M "HJ" "I",Script_M "HL" "K"
            ,Script_M "IL" "KJ",Script_M "IL" ""
            ,Script_M "JK" ""]

rights = filter (\(Script_M po ne) -> Nothing /= (find (==(Script_M po ne)) correct)) rdisk
lefts  = filter (\(Script_M po ne) -> Nothing == (find (==(Script_M po ne)) correct)) rdisk

test2 = do { putStrLn $ show rb
           ; putStrLn ralph
           ; mapM_ (\(Script_M po ne) -> putStrLn $ po ++ " " ++ if ne == "" then "-" else ne ++ (if Nothing == (find (\(Script_M p' n') -> (p' == po) && (n' == ne)) correct) then " x" else " ✓")) rdisk
           ; putStrLn $ "Right: " ++ (show $ length rights) ++ " of " ++ (show $ length correct)
           ; putStrLn $ "Wrong: " ++ (show $ length lefts ) ++ " of " ++ (show $ length rdisk)
           }


main = do { System.IO.hSetEncoding System.IO.stdout System.IO.utf8
          ; putStrLn "Step 1"
          ; printMap map1 b0
          ; putStrLn "Step 2"
          ; printMap map2 b0
          ; putStrLn "Step 3"
          ; printMap map3 b0
          }

{-
main = do{ braid <- evalRandIO $ random_braid 10 30
         ; print $ word braid
         ; print braid
         ; pincher braid
         }
-}
