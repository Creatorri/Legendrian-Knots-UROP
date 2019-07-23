module Alg2
    (Expr (Var,Const,Fail)
    ,plugIn) where

{- Strongly based on the following blog post by Benjamin Kovach:
 - https://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
 -}

infixl 4 :+:
infixl 5 :*:

data Expr a = Var Char
            | Const a
            | (Expr a) :+: (Expr a)
            | (Expr a) :*: (Expr a)
            | Invert (Expr a)
            | Fail

instance (Num a,Eq a) => Eq (Expr a) where
    (Const a) == (Const b) = a == b
    (Var a) == (Var b) = a == b
    Fail == Fail = True
    (Invert (Const 0)) == Fail = True
    Fail == (Invert (Const 0)) = True
    Fail == (a :+: b) = a == Fail || b == Fail
    (a :+: b) == Fail = a == Fail || b == Fail
    Fail == (a :*: b) = a == Fail || b == Fail
    (a :*: b) == Fail = a == Fail || b == Fail
    (Invert Fail) == Fail = True
    (a :+: b) == (c :+: d) = (a == c && b == d) || (a == d && b == c)
    (a :*: b) == (c :*: d) = a == c && b == d
    (Invert a) == (Invert b) = a == b
    _ == _ = False

simplify' :: (Eq a, Fractional a) => Expr a -> Expr a
simplify' (Fail :+: _) = Fail
simplify' (_ :+: Fail) = Fail
simplify' (Fail :*: _) = Fail
simplify' (_ :*: Fail) = Fail
simplify' (Invert Fail) = Fail

simplify' (Const a :+: Const b) = Const $ a+b
simplify' (Const a :*: Const b) = Const $ a*b
simplify' (Invert (Const a)) = if a == 0 then Fail else Const $ 1/a
simplify' (Const 0 :+: b) = simplify b
simplify' (a :+: Const 0) = simplify a
simplify' (Const 0 :*: b) = Const 0
simplify' (a :*: Const 0) = Const 0
simplify' (Const 1 :*: b) = simplify b
simplify' (a :*: Const 1) = simplify a

simplify' (a :+: (Const (-1) :*: b)) | (simplify' a) == (simplify' b) = Const 0
simplify' ((Const (-1) :*: a) :+: b) | (simplify' a) == (simplify' b) = Const 0
simplify' (a :+: (b :*: Const (-1))) | (simplify' a) == (simplify' b) = Const 0
simplify' ((a :*: Const (-1)) :+: b) | (simplify' a) == (simplify' b) = Const 0

simplify' (Invert (a :*: b)) = (simplify' $ Invert b) :*: (simplify' $ Invert a)
simplify' (Invert (a :+: b)) = Invert     $ simplify a :+: simplify' b
simplify' (a :*: (b :+: c))  = simplify'  $ simplify' (a :*: b) :+: simplify' (a :*: c)
simplify' ((a :+: b) :*: c)  = simplify'  $ simplify' (a :*: c) :+: simplify' (b :*: c)

simplify' (a :*: (Invert b)) | a == b = Const 1
simplify' ((Invert a) :*: b) | a == b = Const 1
simplify' (Const a :*: (Invert (Const b))) = if b == 0 then Fail else Const $ a/b
simplify' ((Invert (Const a)) :*: Const b) = if b == 0 then Fail else Const $ a/b

simplify' (a :*: Const b) = Const b :*: (simplify a)

simplify' (a :*: (Const b :*: Const c)) = (Const $ b*c) :*: (simplify a)
simplify' (Const a :*: (b :*: Const c)) = (Const $ a*c) :*: (simplify b)
simplify' (Const a :*: (Const b :*: c)) = (Const $ a*b) :*: (simplify c)

simplify' x = x

simplify :: (Eq a, Fractional a) => Expr a -> Expr a
simplify expr = fullsimp expr (Const 0)
    where fullsimp cur end | cur == end = cur
                           | otherwise = fullsimp (simplify' cur) cur

abs' :: Num a => Expr a -> Expr a
abs' Fail = Fail
abs' v@(Var a) = v
abs' (Const a) = Const $ abs a
abs' (a :+: b) = (abs' a) :+: (abs' b)
abs' (a :*: b) = (abs' a) :*: (abs' b)
abs' (Invert a) = Invert $ abs' a

negate' :: Num a => Expr a -> Expr a
negate' Fail = Fail
negate' a@(Var _) = (Const (-1)) :*: a
negate' (Const a) = Const $ negate a
negate' (a :+: b) = (negate' a) :+: (negate' b)
negate' (a :*: b) = (negate' a) :*: (negate' b)
negate' (Invert a) = Invert $ negate' a

signum' :: Num a => Expr a -> Expr a
signum' Fail = Fail
signum' v@(Var a) = v
signum' (Const a) = Const $ signum a
signum' (a :+: b) = (signum' a) :+: (signum' b)
signum' (a :*: b) = (signum' a) :*: (signum' b)
signum' (Invert a) = Invert $ signum' a

instance (Eq a,Fractional a) => Num (Expr a) where
    a + b = simplify $ a :+: b
    a * b = simplify $ a :*: b
    abs = simplify . abs'
    signum = simplify . signum'
    fromInteger x = Const $ fromInteger x
    negate = simplify . negate'
instance (Eq a,Fractional a) => Fractional (Expr a) where
    fromRational x = Const $ fromRational x
    recip = simplify . Invert
instance (Show a,Eq a,Fractional a) => Show (Expr a) where
    show a = let a' = simplify a
                 showh Fail = "Fail"
                 showh (Const a) = show a
                 showh (Var a) = [a]
                 showh (a :+: b) = "(" ++ (showh a) ++ "+" ++ (showh b) ++ ")"
                 showh (a :*: b) = (showh a) ++ (showh b)
                 showh (Invert a) = "(" ++ (showh a) ++ ")\8315\185"
              in showh a'

mapVar :: (Char -> Expr a) -> Expr a -> Expr a
mapVar f (Var a) = f a
mapVar _ c@(Const _) = c
mapVar f (a :+: b) = (mapVar f a) :+: (mapVar f b)
mapVar f (a :*: b) = (mapVar f a) :*: (mapVar f b)
mapVar f (Invert a) = Invert $ mapVar f a

plugIn :: (Eq a,Fractional a) => Char -> Expr a -> Expr a -> Expr a
plugIn v e = simplify . mapVar (\x -> if x == v then e else Var x)

isVar :: Char -> Expr a -> Bool
isVar c (Var k) = c == k
isVar c (Const _) = False
isVar c (a :+: b) = isVar c a || isVar c b
isVar c (a :*: b) = isVar c a || isVar c b
isVar c (Invert a) = isVar c a
