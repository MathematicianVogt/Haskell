--Ryan Vogt
{- Module: ListTFns
 - Description: Supporting functions for ListTs
 -}
module ListTFns where

import ListT

-- | Convert a Prelude list into ListT
fromPList :: [a] -> ListT a
fromPList (x:xs) = Cons x (fromPList xs)
fromPList [] = Nil

-- | Convert a ListT to a prelude list
toPList :: ListT a -> [a]
toPList (Cons h t) = h: (toPList t)
toPList Nil = []

-- |Implementation of foldl for ListT
listTFoldl :: (b -> a -> b) -> b -> ListT a -> b  
listTFoldl fn acc (Cons v tl) = listTFoldl fn (fn acc v) tl
listTFoldl _ acc Nil = acc

-- |  Implementation of foldr for ListTs
listTFoldr :: (a -> b -> b) -> b -> ListT a -> b  
listTFoldr fn acc (Cons h t) = listTFoldr fn (fn h acc) t
listTFoldr fn acc Nil = acc




-- |  Implementation of reverse for ListT.
listTRev :: ListT a -> ListT a
listTRev lst = listTRev' lst Nil
				where 
					listTRev' (Cons h t) acc = listTRev' t (Cons h acc)
					listTRev' Nil acc = acc
--listTRev (Cons h Nil) = Cons h Nil
--listTRev  Nil = Nil

-- | Implementation of zip for ListT
listTzip :: ListT a -> ListT b -> ListT (a, b)
listTzip xs ys = listTzip' xs ys Nil
              where listTzip' (Cons x xs) (Cons y ys) acc = listTzip' xs ys (Cons (x, y) acc)
                    listTzip' Nil _ acc = listTRev acc
                    listTzip' _ Nil acc = listTRev acc

-- |Return True if an element is in the list
listTelem :: (Eq a) => a -> ListT a -> Bool
listTelem _ Nil = False
listTelem e (Cons v tl) = (e == v) || listTelem e tl 

-- |Append instance for ListTs
listTConcat :: ListT a -> ListT a -> ListT a
listTConcat Nil o = o
listTConcat (Cons v tl) o = Cons v $ listTConcat tl o

-- |Implementation of the map function for ListTs. 
listTMap :: (a -> b) -> ListT a -> ListT b
listTMap _ Nil = Nil
listTMap fn (Cons h tl) = Cons (fn h) (listTMap fn tl)

-- |  Check if two ListTs are equal
listTEqual :: Eq a => ListT a -> ListT a -> Bool
listTEqual (Cons h1 t1) (Cons h2 t2) = if ( h1==h2)
										then listTEqual t1 t2
										else False
listTEqual Nil Nil = True
listTEqual (Cons h1 t1) _ = False
listTEqual _ (Cons h2 t2) = False


-- | This function gets the last element in the list
listTLast :: ListT a -> a
listTLast (Cons v Nil) = v
listTLast (Cons _ tl) = listTLast tl