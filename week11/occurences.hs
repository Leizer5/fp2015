--Напишете функция occurrences, която взима два списъка от числа като аргумент - l1 и l2. 
--Функцията връща списък, който се състои от броя срещания на всеки елемент от l1 в l2.
occurences :: [Int] -> [Int] -> [Int]
occurences [] _ = []
occurences (x:l1) l2 = find x l2 : occurences l1 l2
    where
	find :: Int->[Int]->Int
	find _ [] = 0
	find x (y:ys)
	    | x == y = 1 + find x ys
	    | otherwise = find x ys
