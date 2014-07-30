--ploting haskell gnu

plotFunc [PNG "./mygraph.png"] (linearScale 1000 (-20,20)) (\x -> sin x / x)
