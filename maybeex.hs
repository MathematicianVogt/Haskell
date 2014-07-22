--Ryan Vogt
import Data.Maybe
type AssocList k v = [(k,v)]

get :: Eq k => (AssocList k v) -> k -> Maybe v

get [] _ = Nothing
get((key1,value1):kvs) key=if(key==key1)
							then (Just value1)
							else (get kvs key)
contains :: Eq k => (AssocList k v) -> k-> Bool
contains [] _  = False
contains ((key1,value1):kvs) key = if (key==key1)
								then True
								else contains kvs key


put:: Eq k => (AssocList k v) -> k -> v -> (AssocList k v)

put [] k v = [(k,v)]
put ((k1,v1):kvs) k v = if(k1==k)
					then ((k1,v):kvs)
					else
						(k1,v1): (put kvs k v)


{-

myList=[("one",1),("two",2),("three",3)]
myMap=foldr (\(key,val) assoc -> put assoc key val) [] myList
keys = ["zero","one","two","three","four"]
map (contains myMap) keys
map (get myMap) keys


*Main> let myList=[("one",1),("two",2),("three",3)]
*Main> let myMap=foldr (\(key,val) assoc -> put assoc key val) [] myList
*Main> let keys = ["zero","one","two","three","four"]
*Main> map (contains myMap) keys
[False,True,True,True,False]
*Main> map (get myMap) keys
[Nothing,Just 1,Just 2,Just 3,Nothing]
*Main> 



-}