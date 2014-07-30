--Ryan Vogt
module ListTTest where

import ListT
import ListTFns
import Test.QuickCheck

-- | Test conversion from Prelude lists to ListTs, 
-- check that each element is in the right position
prop_fromPListCorrect :: [Int] -> Bool
prop_fromPListCorrect xs = testOriAndNew xs (buildingTList xs)
                                  where 
                                        buildingTList (x:xs) = Cons x (buildingTList xs)
                                        buildingTList [] = Nil
                                        testOriAndNew (x:xs) (Cons h t) = if (x==h)
                                                                           then  testOriAndNew xs t
                                                                           else False
                                        testOriAndNew  [] (Cons h t)= False
                                        testOriAndNew (x:xs)  Nil = False
                                        testOriAndNew [] Nil =True


-- | Test that listTFoldl produces the same results as the Prelude foldl. I used
-- summation and subtraction of list elements as a test.
prop_listTFoldlCheck :: [Int] -> Bool
prop_listTFoldlCheck xs = fxsp == fxspl && fxsm == fxsml
                          where pxs = fromPList xs
                                fxsp = foldl (+) 0 xs
                                fxsm = foldl (-) 0 xs
                                fxspl = listTFoldl (+) 0 pxs
                                fxsml = listTFoldl (-) 0 pxs

-- |Check that doing one reverse after another results in the original list
prop_listTRevId :: ListT Int -> Bool
prop_listTRevId xs = testOriAndNew (toPList xs) (listTRev (listTRev xs))
                     where 
                          testOriAndNew (x:xs) (Cons h t) = if (x==h)
                                then  testOriAndNew xs t
                                else False
                          testOriAndNew  [] (Cons h t)= False
                          testOriAndNew (x:xs)  Nil = False
                          testOriAndNew [] Nil =True

-- |Check if listTElem works the same as the Prelude elem for 
-- ListTs. 
prop_listTelem :: Int -> Property
prop_listTelem v = forAll listGen (listTelem v)
                      where -- | Special purpose generator, makes sure that the
                            -- arbitrary value is in the list somewhere
                            listGen :: Gen (ListT Int)
                            listGen = do pre <- arbitrary
                                         post <- arbitrary
                                         return $ listTConcat pre $ Cons v post
                                                
                          