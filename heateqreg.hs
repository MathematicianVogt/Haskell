
module Heateqreg where
import Debug.Trace

makeGridx start end h = [start,(start+h)..end]
makeGridt start end h = [start,(start+h)..end]

generateBaseLine f (x:xs) = if (null xs)
							then (x,0,0) : []
							else if(x==0)
									then (x,0,0) : (generateBaseLine f xs)
									else (x,0,(f x)) : (generateBaseLine f xs)

third (_,_,x)=x



--dt change in t step
--dx change in x step
--lb value at left boundary
--rb value at right boundary
--finite difference method : procedural non par implementation

fdm alpha startt endt startx endx dx dt bbFunction = work alpha (makeGridx startx endx dx) (tail (makeGridt startt endt dt)) dx dt startx endx (generateBaseLine bbFunction (makeGridx startx endx dx)) 


work alpha (x:xs) (t:ts) dx dt startx endx (hLine:(h1Line : (h2Line : tLine))) = (hLine:(h1Line : (h2Line : tLine))) ++ (generateNewLine alpha (x:xs) (t:ts) dx dt (hLine:(h1Line : (h2Line : tLine))) [] [])


generateNewLine alpha (x:xs) (t:ts) dx dt (hLine:(h1Line : (h2Line : tLine))) nextList xNew = if(x==0 || (null xs))
																								then (x,t,0) :(generateNewLine alpha xs (t:ts) dx dt (hLine:(h1Line : (h2Line : tLine))) (nextList ++ [(x,t,0)]) (xNew ++ [x]) )
																								else (x,t,(newPhi (third hLine) (third h1Line) (third h2Line) dx dt alpha )) : (generateNewLine alpha xs (t:ts) dx dt (h1Line : (h2Line : tLine)) (nextList ++ [(x,t,(newPhi (third hLine) (third h1Line) (third h2Line) dx dt alpha ))]) (xNew ++ [x]) )

generateNewLine alpha [x1] (t:ts) dx dt  _ nextList  xList = [(x1,t,0)] ++ generateNewLine alpha (xList ++ [x1]) ts dx dt (nextList ++ [(x1,t,0)]) [] []
generateNewLine _ _ [] _ _ _ _ _= []


count (x:xs) = 1 + (count xs)
count [] = 0

--calculates solution of PDE at a time step increasse
newPhi:: Fractional a=> a->a->a->a->a->a->a
newPhi phiL phiC phiR dx dt alpha= phiC + (alpha * (dt/(dx^2)))*(phiR -(2*phiC) + phiL) 


