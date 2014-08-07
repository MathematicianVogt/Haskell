--Ryan Vogt
import Control.Monad.Par

--make one dimension grids to solve equation on 
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

fdm :: (Eq a,Enum a,Num a,NFData a, Fractional a) =>a->a->a->a->a->a->a->(a->a)->[(a,a,a)]
fdm alpha startt endt startx endx dx dt bbFunction = startPar alpha (makeGridx startx endx dx) (makeGridt startt endt dt) (generateBaseLine bbFunction (makeGridx startx endx dx)) dx dt



startPar:: (Eq a, Num a, NFData a,Fractional a) =>a->[a]->[a]->[(a,a,a)]->a->a->[(a,a,a)]
startPar alpha (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt = (phi1:(ph2:(ph3:phis))) ++ (buildPhiListIds alpha (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt [] [])

--goves over a line for every x create a thread, when there are no more x;s collect the threads and recurse to do it again, 
buildPhiListIds:: (Eq a,NFData a, Num a, Fractional a)=> a->[a]->[a]->[(a,a,a)]->a->a->[Par (IVar (a, a, a))]->[a]->[(a,a,a)]															    

buildPhiListIds alpha (0:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt phiIds newX = let newSolId = spawn (return (newPhi (0:xs) t (1,2,3,4,5,6)))			
																	 			in	buildPhiListIds alpha xs (t:ts) (phi1:(ph2:(ph3:phis))) dx dt (phiIds ++ [newSolId]) (newX ++ [0])



buildPhiListIds alpha (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt phiIds newX = let newSolId =  spawn( return (newPhi (x:xs) t (one,two,three,dx,dt,alpha)) )
																		 		in buildPhiListIds alpha xs (t:ts) (ph2:(ph3:phis)) dx dt (phiIds ++ [newSolId]) (newX ++ [x])
																		 		where
																		 			one=third phi1
																					two=third ph2
																					three=third ph3


buildPhiListIds alpha [] (t:ts) _ dx dt phiIds newX = currentSol ++ (buildPhiListIds alpha newX  ts currentSol  dx dt [] []) 
																			where
																				currentSol = (getSolutions (getTuples(getSolutions phiIds)))

buildPhiListIds _ _ [] _ _ _ _ _ = []


--changes  thread to an answer
getTuples :: [IVar a] -> [Par a]
getTuples = map get

--changes monad to solution
getSolutions :: [Par a] -> [a]
getSolutions = runPar . sequence 


third (_,_,x)=x
     
newPhi:: (Eq a,Fractional a)=> [a]->a->(a,a,a,a,a,a)->(a,a,a)
newPhi (0:xs) t (phiL,phiC,phiR,dx,dt,alpha)= (0,t,0)
newPhi (x:[]) t (phiL,phiC,phiR,dx,dt,alpha)= (x,t,0)
newPhi (x:xs) t (phiL,phiC,phiR,dx,dt,alpha)= (x,t,(phiC + (alpha * (dt/(dx^2)))*(phiR -(2*phiC) + phiL)))


