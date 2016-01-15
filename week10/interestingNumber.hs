interestingNumber :: Int -> Bool
interestingNumber n
	| n < 1 = False
	| otherwise = n == sumDivisors ( sumDivisors n 1 ) 1

sumDivisors x y
	| x == y = 0
	| (mod x y) == 0 = y + sumDivisors x ( y + 1 )
	| otherwise = sumDivisors x ( y + 1 )
