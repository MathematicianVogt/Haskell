import Control.Parallel.Strategies

makeGridx:: (Enum a,Num a)=>a->a->a->[a]
makeGridx start end h = [start,(start+h)..end]
makeGridt:: (Enum a, Num a)=>a->a->a->[a]
makeGridt start end h = [start,(start+h)..end]

generateBaseLine:: (Eq a,Num a)=>(a->a)-> [a] -> [(a,a,a)]
generateBaseLine f (x:xs) = if (null xs)
							then [(x,0,0)]
							else if(x==0)
								then (x,0,0) : (generateBaseLine f xs)
								else (x,0,(f x)) : (generateBaseLine f xs)

--fdm :: (Enum a,Num a) =>a->a->a->a->a->a->a->(a->a)->[(a,a,a)]
fdm alpha startt endt startx endx dt dx bbFunction = start alpha (makeGridx startx endx dx) (makeGridt startt endt dt) (generateBaseLine bbFunction (makeGridx startx endx dx)) dt dx

--start:: Num a=>a->[a]->[a]->[(a,a,a)]->a->a->[(a,a,a)]
start alpha (x:xs) (t:ts) (phi:phis) dt dx =  (phi:phis) ++ (startPar alpha (x:xs) (ts) (phi:phis) dt dx [] [])

--startPar:: Num a =>a->[a]->[a]->[(a,a,a)]->a->a->[(a,a,a)]->[a]->[(a,a,a)]
startPar alpha (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dt dx newPhis newXs = if(x==0 || (null xs))
																				then (x,t,unEval(rpar( newPhi (phi1,ph2,ph3,dx,dt,alpha,1)))) : (startPar alpha xs (t:ts) (phi1:(ph2:(ph3:phis))) dt dx (newPhis ++ [(x,t,unEval(rpar( newPhi (phi1,ph2,ph3,dx,dt,alpha,1))))]) (newXs ++ [x]))
																				else (x,t,unEval(rpar( newPhi (phi1,ph2,ph3,dx,dt,alpha,0)))) :  (startPar alpha xs (t:ts) (ph2:(ph3:phis)) dt dx (newPhis ++ [(x,t,unEval(rpar( newPhi (phi1,ph2,ph3,dx,dt,alpha,0))))]) (newXs ++ [x]))

startPar alpha [x1] (t:ts)  _ dx dt  newPhis  newXs = [(x1,t,unEval(rpar( newPhi (1,2,3,4,5,6,1))))] ++ (startPar alpha (newXs ++ [x1]) ts (newXs ++ [(x1,t,unEval(rpar( newPhi (1,2,3,4,5,6,1))))]) dx dt  [] [])
startPar _ _ [] _ _ _ _ _= []



newPhi:: ( Num b,Eq b,Fractional a)=> (a,a,a,a,a,a,b)->a
newPhi (phiL,phiC,phiR,dx,dt,alpha,0)= phiC + (alpha * (dt/(dx^2)))*(phiR -(2*phiC) + phiL)
newPhi (phiL,phiC,phiR,dx,dt,alpha,1)= 0 



