fold :: (a->b->b)->[a]->b->b
--right to left 
fold f [] start = start
fold f (h:t) start = fold f t (f h start)


--fold (\x y -> x+y) [1..5] 0
--fold fold (\x y -> x*y) [1..5] 0
--fold fold (\x y -> x-y) [1..5] 0