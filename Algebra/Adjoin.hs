module Algebra.Adjoin
    (Adjoin (N,G)
    ,plugIn
    ) where

import Data.List
import Algebra.Group

infixl 4 :+:
infixl 5 :*:

data Adjoin f g = N f
             | G g
             | (Adjoin f g) :+: (Adjoin f g)
             | (Adjoin f g) :*: (Adjoin f g)
             | Recip (Adjoin f g)
             | Fail

plusChain :: (Eq a,Fractional a,Eq b, Group b) => Adjoin a b -> [Adjoin a b]
plusChain (N a) = [N a]
plusChain (G a) = [G a]
plusChain (N 1 :*: a) = if a == G mempty then [N 1] else [a]
plusChain (N 0 :*: _) = [N 0]
plusChain m@(a :*: b) = [m]
plusChain (Recip a) = [Recip a]
plusChain (a :+: b) = (plusChain a) ++ (plusChain b)
instance (Show f,Show g) => Show (Adjoin f g) where
    show (N a) = show a
    show (G a) = show a
    show (a :+: b) = show a ++ "+" ++ show b
    show (a :*: b) = show a ++ show b
    show (Recip a) = show a ++ "\8315\185"
instance (Eq f,Eq g,Fractional f,Group g) => Eq (Adjoin f g) where
    Recip a == b = sieq (a * b) (N 1)
    a == Recip b = Recip b == a
    a == b = sieq (a - b) (N 0)

sieq :: (Eq f,Eq g,Fractional f,Group g) => Adjoin f g -> Adjoin f g -> Bool
sieq (N a) (N b) = a == b
sieq (G a) (G b) = a == b
sieq (a :+: b) (c :+: d) = (sieq a c && sieq b d) || (sieq a d && sieq b c)
sieq (a :*: b) (c :*: d) = sieq a c && sieq b d
sieq (Recip a) (Recip b) = sieq a b
sieq _ _ = False

simp :: (Eq f,Eq g,Fractional f,Group g) => Adjoin f g -> Adjoin f g
simp (Fail :+: _) = Fail
simp (_ :+: Fail) = Fail
simp (Fail :*: _) = Fail
simp (_ :*: Fail) = Fail
simp (Recip Fail) = Fail

simp (G a) | a == mempty = N 1

simp (N a :+: N b) = N $ a + b
simp (N a :*: N b) = N $ a * b
simp (G a :*: G b) = G $ a <> b

simp (a :+: N 0) = simp a
simp (N 0 :+: b) = simp b
simp (a :*: N 1) = simp a
simp (N 1 :*: b) = simp b
simp (a :*: G b) | b == mempty = simp a
simp (G a :*: b) | a == mempty = simp b
simp (a :*: N 0) = N 0
simp (N 0 :*: b) = N 0

simp (G a :*: N b) = N b :*: G a
simp (G a :+: G b) | a == b = (N 2) :*: G a
simp (G a :+: (N b :*: G c)) | a == c = (N $ b+1) :*: G a
simp ((N a :*: G b) :+: G c) | b == c = (N $ a+1) :*: G b
simp ((N a :*: G b) :+: (N c :*: G d)) | b == d = (N $ a + c) :*: G b

simp (N a :*: (N b :*: c)) = (N $ a * b) :*: (simp c)
simp (N a :*: b :*: N c) = (N $ a * c) :*: (simp b)
simp (a :*: N b :*: N c) = (N $ b * c) :*: (simp a)
simp (a :*: b :*: N c) = (N c) :*: (simp $ a :*: b)
simp (a :*: N b :*: c) = (N b) :*: (simp $ a :*: c)
simp (N a :*: b :*: c) = (N a) :*: (simp $ b :*: c)

simp (a :*: (b :+: c)) = (simp $ a :*: b) :+: (simp $ a :*: c)
simp ((a :+: b) :*: c) = (simp $ a :*: c) :+: (simp $ b :*: c)

simp (Recip (N a)) = if a == 0 then Fail else N $ recip a
simp (Recip (G a)) = G $ invert a
simp (Recip (Recip a)) = a

simp (a :*: (Recip b)) | sieq a b = N 1
simp ((Recip a) :*: b) | sieq a b = N 1

simp (Recip (a :*: b)) = (simp $ Recip b) :*: (simp $ Recip a)
simp (a :+: b) = simp a :+: simp b
simp (a :*: b) = simp a :*: simp b
simp (Recip a) = Recip $ simp a
simp x = x

collectTerms :: (Eq f,Eq g,Fractional f,Group g) => Adjoin f g -> Adjoin f g
collectTerms a = simp $ foldr (\x xs -> simp $ x :+: xs) (N 0) $ map (\x -> foldr (\x xs -> simp $ x :+: xs) (N 0) $ filter (\k -> eq k x) pc) uniqs
    where pc = plusChain a
          uniqs = nubBy eq pc
          eq (N _ :*: a) (N _ :*: b) = sieq a b
          eq a (N _ :*: b) = sieq a b
          eq (N _ :*: a) b = sieq a b
          eq (N _) (N _) = True
          eq a b = sieq a b

fullsmp a = sh a (N 0)
    where sh a b = if sieq a b then a else sh (collectTerms $ simp a) a

instance (Eq f,Eq g,Fractional f,Group g) => Num (Adjoin f g) where
    a + b = fullsmp $ a :+: b
    a * b = fullsmp $ a :*: b
    abs (N a) = N (abs a)
    abs (G a) = G a
    abs (a :+: b) = (abs a) + (abs b)
    abs (a :*: b) = (abs a) * (abs b)
    abs (Recip a) = Recip (abs a)
    signum (N a) = N (signum a)
    signum (G a) = G a
    signum (a :+: b) = (signum a) + (signum b)
    signum (a :*: b) = (signum a) * (signum b)
    signum (Recip a) = Recip $ signum a
    fromInteger x = N $ fromInteger x
    negate (N a) = N $ negate a
    negate (G a) = (N (-1)) * (G a)
    negate (N a :*: G b) = (N $ negate a) * (G b)
    negate (a :*: b) = (negate a) * (negate b)
    negate (a :+: b) = (negate a) + (negate b)
instance (Eq f,Eq g,Fractional f,Group g) => Fractional (Adjoin f g) where
    recip (N a) = N $ recip a
    recip (G a) = G $ invert a
    recip (a :*: b) = (recip b) * (recip a)
    recip (a :+: b) = Recip (a + b)
    recip (Recip a) = a
    fromRational = N . fromRational
compareh :: (Show g,Eq f,Eq g,Fractional f,Group g,Ord f) => Adjoin f g -> f
compareh (N a) = a
compareh (G a) = fromInteger $ genericLength $ show a
compareh (a :+: b) = max (compareh a) (compareh b)
compareh (a :*: b) = max (compareh a) (compareh b)
instance (Show g,Eq f,Eq g,Fractional f,Group g,Ord f) => Ord (Adjoin f g) where
    compare a b = if a == b then EQ else compare (compareh a) (compareh b)

plugIn :: (Eq f,Eq g,Fractional f,Group g) => (g -> Adjoin f g) -> Adjoin f g -> Adjoin f g
plugIn f (N a) = N a
plugIn f (G a) = f a
plugIn f (a :+: b) = (plugIn f a) + (plugIn f b)
plugIn f (a :*: b) = (plugIn f a) * (plugIn f b)
plugIn f (Recip a) = recip $ plugIn f a
