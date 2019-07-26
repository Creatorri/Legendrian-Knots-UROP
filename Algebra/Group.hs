module Algebra.Group
    (Group (..)
    ) where

class Monoid m => Group m where
    invert :: m -> m 
