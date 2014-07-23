fixedPoint ::(Show a,Num a, Ord a) => (a -> a) -> a -> a -> a
fixedPoint f xn h = if ( (abs (xn1 -xn)) >h)
						then (fixedPoint f xn1 h)
						else  xn1
							where
								xn1=f xn
