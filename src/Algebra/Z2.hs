module Algebra.Z2
    (Z2
    ) where

data Z2 = O | I deriving Eq
instance Show Z2 where
    show O = "0"
    show I = "1"
instance Num Z2 where
    I + I = O
    O + O = O
    _ + _ = I
    I * I = I
    _ * _ = O
    abs = id
    signum = id
    fromInteger x = case (x `mod` 2) of 0 -> O
                                        1 -> I
    (-) = (+)
instance Ord Z2 where
    compare I O = GT
    compare O I = LT
    compare O O = EQ
    compare I I = EQ
instance Fractional Z2 where
    recip I = I
    recip O = error "Div by 0"
    fromRational x = O
