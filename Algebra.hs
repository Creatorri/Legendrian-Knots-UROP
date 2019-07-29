module Algebra
    (Adjoin (..)
    ,FreeGroup (..)
    ,Group (..)
    ,Z2
    ,plugIn
    ,Algebra
    ) where

import Algebra.Adjoin
import Algebra.Group
import Algebra.FreeGroup
import Algebra.Z2

type Algebra = Adjoin Z2 FreeGroup

plugIn :: (Eq f,Fractional f) => (Char -> Adjoin f FreeGroup) -> Adjoin f FreeGroup -> Adjoin f FreeGroup
plugIn f = adjPlugIn (groupPlugIn f)
