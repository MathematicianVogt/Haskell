--Ryan Vogt
-- For this lab, we will implement our own version of a list and some supporting
-- functions. It will be your responsibility to insure that the quick-check
-- tests pass, as well as write your own.
{- Module: ListT
 - Description: Implementation of a list type called ListT
 -}
module ListT where

import Test.QuickCheck
import Control.Monad

-- | A type representing a list of values
data ListT a =   Nil 
               | Cons a (ListT a)
               deriving (Eq, Show)

-- | Instance for generating arbitrary lists with quickcheck
instance Arbitrary a => Arbitrary (ListT a) where 
    arbitrary = oneof [return Nil, 
                       liftM2 Cons arbitrary arbitrary]
