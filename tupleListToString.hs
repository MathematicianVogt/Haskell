
conversion (x:xs) = x : ',' : (conversion xs)
conversion (x:[])=x : []