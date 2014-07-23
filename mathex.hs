import Debug.Trace
import Graphics.EasyPlot
makeGrid :: (Enum a, Num a)=>a->a->a->[a]

makeGrid start end delta = [start,(start+delta)..end]





eulers t0 t1 n y0  = reverse (eulersMethod1 (\t y -> y) (makeGrid t0 t1 ((t1-t0)/n)) [y0] ((t1-t0)/n))

eulersMethod1  diffEq (t:ts) yList h = eulersMethod2 diffEq ts yList h

eulersMethod2 diffEq [] yList h = yList

eulersMethod2 diffEq (t:ts) (y:ys) h = eulersMethod2 diffEq ts ((y+ (h*(diffEq t y))) : (y:ys)) h

fixedPoint ::(Show a,Num a, Ord a) => (a -> a) -> a -> a -> a
fixedPoint f xn h = if ( (abs (xn1 -xn)) >h)
						then (fixedPoint f xn1 h)
						else  xn1
							where
								xn1=f xn





showSolution = plot (makeGrid0 0 10 .00001) (eulers 0 10 1000000 1)






--(\x->(((-x^2) +((-4)*x))/4))

--makeDiff diffEq = (\t y -> y)

--evalDiffEq diffEq tn yn = diffEq tn yn