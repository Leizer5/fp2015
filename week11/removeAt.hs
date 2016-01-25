--Напишете функция removeAt която по даден индекс и списък, премахва елемента на индекса и връща новия списък.
removeAt n [] = error "The list is empty"
removeAt 0 ys = tail ys
removeAt n ys
    |n > length ys && n < 0 = error "Integer out of bounds!"
    |otherwise = iter (ys !! n) ys 
        where
            iter number [] = []
            iter number xs
                |not (number == head xs) = head xs : iter number (tail xs)
                |otherwise = iter number (tail xs)
