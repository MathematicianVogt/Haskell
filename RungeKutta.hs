--Runge Kutta solving differential equations
makeGrid start end h = [start,(start+h)..end]
returnTail (x:xs) = xs

rungeKuttaSolver diffEq [] yList h = yList
rungeKuttaSolver diffEq (tn:ts) (yn:ys) h = rungeKuttaSolver diffEq ts ( (yn  + (h/6)*(k1 + 2*k2 + 2*k3 + k4)): (yn:ys)) h
											where
											k1 = diffEq tn yn 
											k2= diffEq (tn +(h/2)) (yn + (h/2)*k1)
											k3= diffEq (tn + (h/2)) (yn + (h/2)*k2)
											k4 = diffEq (tn + h) (yn+(h*k3))


rungeKutta diffEq t0 t1 y0 n = reverse( rungeKuttaSolver diffEq (returnTail (makeGrid t0 t1 ((t1-t0)/n))) [y0] ((t1-t0)/n))


