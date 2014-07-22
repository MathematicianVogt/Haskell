mylength :: [t]-> Int
mylength [] = 0
mylength (h:t)= 1+ length(t)

mylast :: [t]->t
mylast [] = error "Empty list no last"
mylast (x:[])=x
mylast (x:xs)=last(xs) 

mysum :: [t]->Num
mysum [] = 0
mysum (h:t) = h+mysum(t)