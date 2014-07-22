data Node a = Node {value :: a, leftNode :: Node a, rightNode :: Node a}
			| Empty
			deriving Show


find:: Ord a=> Node a -> a -> Bool
find Empty val = False
find tree val = if (val == (value tree))
					then True
					else 
						if(val> (value tree))
							then (find (rightNode tree) val )
							else (find  (leftNode tree) val)

bstInsert :: Ord a => Node a -> a -> Node a
bstInsert Empty value = Node value Empty Empty
bstInsert node @ (Node v left right) value 
					|value < v = Node v  (bstInsert left value) right
					|value >v = Node v left (bstInsert right value)
					|True=node


numNodes::Node a -> Integer
numNodes Empty = 0
numNodes tree = 1+ (numNodes (leftNode tree)) + (numNodes (rightNode tree))

{-

Ryan Vogt

ryans-mbp-3:Haskell MathematicianVogt$ ghci tree.hs 
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling Main             ( tree.hs, interpreted )
Ok, modules loaded: Main.
*Main> let tree = foldr (flip bstInsert) Empty [7,5,3,1,6,2,4]
*Main> tree
Node {value = 4, leftNode = Node {value = 2, leftNode = Node {value = 1, leftNode = Empty, rightNode = Empty}, rightNode = Node {value = 3, leftNode = Empty, rightNode = Empty}}, rightNode = Node {value = 6, leftNode = Node {value = 5, leftNode = Empty, rightNode = Empty}, rightNode = Node {value = 7, leftNode = Empty, rightNode = Empty}}}
*Main> find tree 2
True
*Main> find tree 65
False
*Main> numNodes tree
7




-}