--Ryan Vogt
{- Module : PalListT
 - Description : Wrapper of ListT implementing palindrome lists
 -}
module PalListT (PalindromeListT,
                 empty,
                 palcons,
                 palListFoldl,
                 palListConcat,
                 prop_genPalindromeTest,
                 prop_consPalindromeTest) where

import Test.QuickCheck
import Control.Monad
import ListT
import ListTFns

-- |Wrapper of ListT. Data of this type must be created using smart
-- constructors included in this module.
newtype PalindromeListT a = PalListT (ListT a)
                            deriving (Eq, Show)

instance Arbitrary a => Arbitrary (PalindromeListT a) where 
    arbitrary = liftM palFromPList arbitrary

-- |Convert a regular list into a palindrome list by successively adding
-- elements of the original list from left to right. 
palFromPList :: [a] -> PalindromeListT a
palFromPList xs = fromPList' xs empty
               where fromPList' [] acc = empty
                     fromPList' (x:xs) acc = fromPList' xs (palcons x acc)

-- The following functions don't need to be changed for PalindromeListTs. Assuming that 
-- only a valid palindrome can be in a PalindromeListT it is impossible that
-- one of these functions will generate a new list that is not a palindrome.

-- |Map for palindrome lists.
palListMap :: (a -> b) -> PalindromeListT a -> PalindromeListT b 
palListMap fn (PalListT xs) = PalListT $ listTMap fn xs

-- | Concat two palindrome lists together, insuring that they remain a
-- palindrome
palListConcat :: PalindromeListT a -> PalindromeListT a -> PalindromeListT a
palListConcat (PalListT p1) (PalListT p2) = PalListT $ listTConcat cat (listTRev cat)
                                            where cat = listTConcat p1 p2 

-- | Foldl for palindrome lists
palListFoldl fn start (PalListT p) = PalListT $ listTFoldl fn start p

-- |Create an empty palindrome list
empty :: PalindromeListT a
empty = PalListT Nil

-- |Cons for palindrome lists, in addition to adding an element to the
-- beginning of a list, it must also add the element to the end.
palcons :: a -> PalindromeListT a -> PalindromeListT a
palcons a (PalListT xs) =  (PalListT (mainBuild a xs))
                           where
                              mainBuild a b = Cons a (buildingPal a b)
                              buildingPal a Nil = (Cons a Nil)
                              buildingPal a (Cons h t) =Cons h (buildingPal a t)

-- | Head for palindrome lists, if the list is empty return Nothing.
palHd :: PalindromeListT a -> Maybe a
palHd (PalListT (Cons v _)) = Just v
palHd (PalListT Nil) = Nothing

-- |  Return the tail of this list. To maintain the input's status as a
-- palindrome list, you must also remove the corresponding end element
-- of the PalindromeListT in addition to the head element.
palTl :: PalindromeListT a -> Maybe (PalindromeListT a)
palTl (PalListT b) = Just ( PalListT (remakePal b))
                                where
                                     remakePal (Cons _ (Cons h1 t)) = Cons h1 (remakePal t)
                                     remakePal (Cons _ Nil) = Nil
                               

-- Test properties for palindrome lists

-- |  Given a PalindromeListT, test that the generator always creates
-- random PalindromeListTs that are palindromes.
prop_genPalindromeTest :: PalindromeListT Int -> Bool
prop_genPalindromeTest (PalListT a) = if( (listTRev a) == a)
                                            then True
                                            else False                                    

-- | Given a value and a PalindromeListT of ints, randomly generate a value,
-- cons it onto the list, and check that the resultant list is still a
-- palindrome.
prop_consPalindromeTest :: PalindromeListT Int -> Property
prop_consPalindromeTest plst = forAll custGen testFn
                                     -- This generates an additional random variable
                                         where custGen = arbitrary :: Gen Int
                                     -- This helper function should cons v onto the lst
                                     -- and then check that it is a palindrome
                                               testFn v = let PalListT lst' = palcons v plst 
                                                              hdEq = case lst' of
                                                                    Cons v2 _ -> v2 == v
                                                              onlyLast _ (Cons v _) = v
                                                              last (Cons v Nil) = v
                                                              last (Cons _ tl) = last tl
                                                          in
                                                            last lst' == v && hdEq
                                                              