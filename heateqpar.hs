import Control.Monad.Par

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
--fdm alpha startt endt startx endx dx dt bbFunction = start alpha (makeGridx startx endx dx) (makeGridt startt endt dt) (generateBaseLine bbFunction (makeGridx startx endx dx)) dx dt

--start:: Num a=>a->[a]->[a]->[(a,a,a)]->a->a->[(a,a,a)]
--start alpha (x:xs) (t:ts) (phi:phis) dx dt =  (startPar alpha (x:xs) (ts) (phi:phis) dx dt [] [])

--startPar:: Num a =>a->[a]->[a]->[(a,a,a)]->a->a->[(a,a,a)]->[a]->[(a,a,Par (a,a,a))]
startPar alpha (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt = (phi1:(ph2:(ph3:phis))) ++ (buildPhiListIds (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt [] [])
																		    
buildPhiListIds alpha (x:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt phiIds newX = do 
																		one<-third phi1
																		two<-third ph2
																		three<-third ph3
																		newSolId<- spawn( return (newPhi (x:xs) t (one,two,three,dx,dt,alpha) ))
																		buildPhiListIds alpha xs (t:ts) (ph2:(ph3:phis)) dx dt (phiIds ++ [newSolId]) (newX ++ [x])

buildPhiListIds alpha (0:xs) (t:ts) (phi1:(ph2:(ph3:phis))) dx dt phiIds newX = do 
																		newSolId<-spawn (return (newPhi (0:xs) t (1,2,3,4,5,6)))			
																		buildPhiListIds alpha xs (t:ts) (phi1:(ph2:(ph3:phis))) dx dt (phiIds ++ [newSolId]) (newX ++ [0])

buildPhiListIds alpha [] (t:ts) (phi1:(ph2:(ph3:phis))) dx dt phiIds newX = do
																			(getTuples(getSolutions phiIds)) ++ (buildPhiListIds alpha newX  ts (getTuples(getSolutions phiIds))  dx dt [] []) 

buildPhiListIds _ _ [] _ _ _ _ _ = []


getSolutions::[IVar a]->[Par a]
getSolutions (x:xs) = (get x) : (getSolutions xs)
getSolutions [] = []  

getTuples:: [Par a]->[a]
getTuples (x:xs) = (runPar x):(getTuples xs)
getTuples [] = []


third (_,_,x)=x

ex f g x = runPar $ do
      fx <- spawn (return (f x))  
      gx <- spawn (return (g x))  
      a <- get fx       
      b <- get gx       
      return (a,b)      
newPhi:: (Eq a,Fractional a)=> [a]->a->(a,a,a,a,a,a)->(a,a,a)
newPhi (0:xs) t (phiL,phiC,phiR,dx,dt,alpha)= (0,t,0)
newPhi (x:[]) t (phiL,phiC,phiR,dx,dt,alpha)= (x,t,0)
newPhi (x:xs) t (phiL,phiC,phiR,dx,dt,alpha)= (x,t,(phiC + (alpha * (dt/(dx^2)))*(phiR -(2*phiC) + phiL)))


