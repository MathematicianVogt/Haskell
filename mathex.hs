makeGrid :: (Enum a, Num a)=>a->a->a->[a]

makeGrid start end delta = [start,(start+delta)..end]


evalDiffEq diffEq tn yn = diffEq tn yn


eulersMethod  diffEq tList yList =0

eulers t0 t1 h y0 diffEq = eulersMethod (\t y -> diffEq) (makeGrid t0 t1 ((t1-t0)/h)) [y0]


makeDiff diffEq = (\t y -> diffEq)