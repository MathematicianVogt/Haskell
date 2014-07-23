data Rope = TextRope{ropeText :: String}
			| ConcatRope{rope1 :: Rope, rope2 :: Rope}
			| SubRope{subRopetext :: Rope, starting :: Integer, ending :: Integer}












sub :: (Eq b,Num b)=> [a]-> b->b ->[a]
sub  (x:xs) 0 0 = []
sub (x: xs) 0 end = x : sub xs 0 (end-1)
sub (x:xs) start end = sub xs (start-1) end 

concatt ::  String->String->String
concatt a b = a ++ b  


theLength :: String -> Integer
theLength (x:xs) = 1+ (theLength xs)
theLength [] = 0

sizeOf :: Rope -> Integer
sizeOf (TextRope s) = theLength s
sizeOf (ConcatRope a b) = sizeOf(a) +sizeOf (b)
sizeOf (SubRope a b c) =  sizeOf ( sub a b c )




instance Show Rope where
    show (TextRope s) = s
    show (ConcatRope a b) = show a ++ show b
    show (SubRope (TextRope s) start end) = show  (sub s start end)
    show (SubRope (ConcatRope a b) start end) = show  (sub (concatt (ropeText a)  (ropeText b))  start end)