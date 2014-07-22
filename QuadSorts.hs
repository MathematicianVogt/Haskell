module QuadSorts ( bsort, isort ) where
	--Ryan Vogt

	
	--IsSorted a function that retturns a bool on a given list that tells if a list is sorted from least to greatest
	isSorted :: Ord a =>[a]->Bool
	isSorted []=True
	isSorted [x]=True
	isSorted (h:(h1:t)) = if(h<=h1)
							then isSorted(h1:t)
							else False
	--buble a helper function for the bsort function which sorts a single element 
	--in the list at a time, putting the least element in the head of the list when finished
	bubble :: Ord a=>[a]->[a]
	bubble [] = []
	bubble [x] = [x]
	bubble (h:t) = let y= bubble(t) in 
						if(h> (head y))
							then (head y) : (bubble (h : (tail y)))
							else h:y

	--bsort, a function that uses bubblesort method
	--takes a list and bubbles it, resulting in the least element first, followed with the bubble of the tail
	--which will result in the next smallest element etc, until the list is only the singleton list, which would
	--result in being the greatest value in the list
	bsort :: Ord a=>[a]->[a]
	bsort [] = []
	bsort [x]=[x]
	bsort (h:t) = bubble((h:t))



	--insert function, take a list and sees where a value
	--can be inserted to correctly go from smallest to greatest
	insert :: Ord a=>a->[a]->[a]
	insert x [y] =if (x>y)
						then y:[x]
						else x:[y]
	insert x (h:t) = if(x>h)
						then h:(insert x t)
						else x:h:t

	--isort calls insert over and over to correctly inserting values to be ordered from largest to greatest 
	isort :: Ord a=>[a]->[a]
	isort [] = []
	isort [x]= [x]
	isort (h:t) = insert h (isort t)





