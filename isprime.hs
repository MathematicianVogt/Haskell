--Ryan Vogt
is_prime 1 = False
is_prime n = let notDivSoFar divisor prevResult = (prevResult) && (n `mod` divisor /= 0)
			in foldr notDivSoFar True [2..(n-1)] 


