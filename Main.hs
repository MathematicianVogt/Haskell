--Ryan Vogt
module Main where

import ListT
import ListTTest
import PalListT
import Test.QuickCheck
import Control.Monad

-- | These quickcheck functions take lists
list_properties :: [(String, [Int] -> Bool)]
list_properties = [("prop_fromPListCorrect", prop_fromPListCorrect), 
                   ("prop_listTFoldlCheck", prop_listTFoldlCheck)
                   ]
-- | These properties take Ints and return a property
listT_intProp :: [(String, Int -> Property)]
listT_intProp = [("prop_listTelem", prop_listTelem)]

-- | These quickcheck functions take ListTs and produce Bools
listT_properties :: [(String, ListT Int -> Bool)]
listT_properties = [("prop_listTRevId", prop_listTRevId)] 

-- | These quickcheck functions take PalindromeListTs
palListT_Boolfns :: [(String, PalindromeListT Int -> Bool)] 
palListT_Boolfns = [("prop_genPalindromeTest", prop_genPalindromeTest)]

-- | These quickcheck functions produce a Property
palListT_genProp  :: [(String, PalindromeListT Int -> Property)]
palListT_genProp = [("prop_consPalindromeTest", prop_consPalindromeTest)]

-- | Print the name of the test, then run quickcheck
printAndCheck :: Testable prop => (String, prop) -> IO ()
printAndCheck (s, p) = do putStrLn $ "Testing: " ++ s ++ ":"
                          quickCheck p
                          putStrLn ""

-- | Main function which runs tests
main :: IO ()
main = do mapM_ printAndCheck list_properties
          mapM_ printAndCheck listT_intProp
          mapM_ printAndCheck listT_properties
          mapM_ printAndCheck palListT_Boolfns
          mapM_ printAndCheck palListT_genProp