--Ryan Vogt

--Ropes can take three forms as followed
module Rope ( sizeOf, i_th,dissect  ) where
data Rope = TextRope{ropeText::String}
			| ConcatRope{rope1 :: Rope, rope2 :: Rope}
			| SubRope{subRopetext :: Rope, starting :: Integer, ending :: Integer}











--takes a string and returns the elements of the string from start to end-1
sub :: (Eq b,Num b)=> [a]-> b->b ->[a]
sub  (x:xs) 0 0 = []
sub (x: xs) 0 end = x : sub xs 0 (end-1)
sub (x:xs) start end = sub xs (start-1) end 
--takes two strings and puts them together
concatt ::  String->String->String
concatt a b = a ++ b  

--gets the length of a string
theLength :: String -> Integer
theLength (x:xs) = 1+ (theLength xs)
theLength [] = 0
--gets the length of a rope object
sizeOf :: Rope -> Integer
sizeOf (TextRope s) = theLength s
sizeOf (ConcatRope a b) = sizeOf(a) +sizeOf (b)

--get the specific character in a string at element i
getCharac :: [Char]->Integer-> Char
getCharac (x:xs) 0 = x
getCharac (x:xs) i = getCharac xs (i-1)



--gets the ith character in a rope 
i_th :: Rope-> Integer -> Char
i_th (TextRope s) i  =getCharac s i 

--returns a string representation of the datastructure.
dissect :: Rope -> String
dissect a = show a

  



--shows different kinds of ropes
instance Show Rope where
    show (TextRope s) = s
    show (ConcatRope a b) = show a ++ show b
    show (SubRope (TextRope s) start end) = show  (sub s start end)
    show (SubRope (ConcatRope a b) start end) = show  (sub (concatt (ropeText a)  (ropeText b))  start end)

    