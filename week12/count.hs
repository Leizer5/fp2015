--Трябва да върне броя на срещания на първия аргумент в списъка.
count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys)
 |x==y = 1 + count x ys
 |otherwise = count x ys
