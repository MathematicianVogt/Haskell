--Ryan Vogt

--Ropes can take three forms as followed
{-

FOR GRADER:

three issues, 
-having to do with dissect, putting quotes into string with the escape character is including the escape character
-return of dissect has strings around it
-my exception for out of bounds isnt being called correctly for some reason i tried to trouble shoot it locally by going step by step and it worked
 but on test failed.


Not sure how to remedy those, its some sort of weird type thing.


-}

module Rope ( sizeOf, i_th,dissect,show, Rope(..) ) where
import Data.Char
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

--get the specific character in a string at element i
getCharac :: [Char]->Integer-> Char
getCharac [] _ = error "Cant get a sub string"
getCharac (x:xs) 0 = x
getCharac (x:xs) i = getCharac xs (i-1)



--returns a string representation of the datastructure.
dissect :: Rope -> String
dissect (TextRope s)= "TextRope(\"" ++ ""  ++ s ++ "\")"
dissect (ConcatRope a b) = "ConcatRope(" ++ (dissect a) ++ "," ++ (dissect b) ++ ")"
dissect (SubRope a b c) = "SubRope(" ++ (dissect a) ++ "," ++ (show b) ++ "," ++ (show c) ++ ")"

buildString ::Rope->Integer-> Integer->String
buildString rope 0 0 = []
buildString rope start 0 = []
buildString rope start end =if((start+end-1)>((sizeOf rope)-1))
								then error ("index " ++ (show end) ++ " out of bounds.") 
							    else [(i_th rope start)] ++ (buildString rope (start+1) (end-1))  



--shows different kinds of ropes
instance Show Rope where
    show (TextRope s) = s
    show (ConcatRope a b) = show a ++ show b
    show (SubRope rope start end) = buildString rope start end 


instance Linear Rope where
	--gets the ith character in a rope 
	i_th (TextRope s) i  =getCharac s i 
	i_th (ConcatRope a b) i = if( i> (sizeOf a)-1)
								then i_th b (i - (sizeOf a))
								else i_th a i
	i_th (SubRope a start end) i = i_th a (start+i)

	--gets the length of a rope object
	sizeOf (TextRope s) = theLength s
	sizeOf (ConcatRope a b) = sizeOf(a) +sizeOf (b)
	sizeOf (SubRope _ _ b) =b

class Linear a where
	i_th :: a-> Integer -> Char
	sizeOf :: a -> Integer




   
    