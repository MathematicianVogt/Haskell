import Debug.Trace


makeGridx start end h = [start,(start+h)..end]
makeGridt start end h = [start,(start+h)..end]

generateBaseLine f (x:xs) = (x,0,(f x)) : (generateBaseLine f xs)
generateBaseLine f [] = [] 

third (_,_,x)=x



--dt change in t step
--dx change in x step
--lb value at left boundary
--rb value at right boundary
--finite difference method : procedural non par implementation
fdm:: (Enum t, Eq t, Fractional t) =>t -> t -> t -> t -> t -> t -> t -> (t -> t) -> [[(t, t, t)]]
fdm alpha startt endt startx endx dt dx bbFunction = work alpha (makeGridx startx endx dx) (tail (makeGridt startt endt dt)) dx dt startx endx (generateBaseLine bbFunction (makeGridx startx endx dx)) 

work:: (Eq t3, Fractional t2, Num t3) =>t2-> [t3]-> [t4]-> t2-> t2-> t-> t1-> [(t3, t4, t2)]-> [[(t3, t4, t2)]]
work alpha (x:xs) (t:ts) dx dt startx endx (hLine:(h1Line : (h2Line : tLine))) = [(hLine:(h1Line : (h2Line : tLine)))] ++ [(generateNewLine alpha (x:xs) (t:ts) dx dt (hLine:(h1Line : (h2Line : tLine))) [] [])]

generateNewLine:: (Eq t1, Fractional t, Num t1) =>t-> [t1]-> [t2]->t-> t-> [(t1, t2, t)]-> [(t1, t2, t)]-> [t1]-> [(t1, t2, t)]
generateNewLine alpha (x:xs) (t:ts) dx dt (hLine:(h1Line : (h2Line : tLine))) nextList xNew = if(x==0 || (null xs))
																								then (x,t,0) :(generateNewLine alpha xs (t:ts) dx dt (hLine:(h1Line : (h2Line : tLine))) (nextList ++ [(x,t,0)]) (xNew ++ [x]) )
																								else (x,t,(newPhi (third hLine) (third h1Line) (third h2Line) dx dt alpha )) : (generateNewLine alpha xs (t:ts) dx dt (h1Line : (h2Line : tLine)) (nextList ++ [(x,t,(newPhi (third hLine) (third h1Line) (third h2Line) dx dt alpha ))]) (xNew ++ [x]) )

generateNewLine alpha [x1] (t:ts) dx dt  _ nextList  xList = [(x1,t,0)] ++ generateNewLine alpha (xList ++ [x1]) ts dx dt (nextList ++ [(x1,t,0)]) [] []
generateNewLine _ _ [] _ _ _ _ _= []




--calculates solution of PDE at a time step increasse
newPhi:: Fractional a=> a->a->a->a->a->a->a
newPhi phiL phiC phiR dx dt alpha= phiC + (alpha * (dt/(dx^2)))*(phiR -(2*phiC) + phiL) 
