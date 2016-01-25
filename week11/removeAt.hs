--Напишете функция removeAt която по даден индекс и списък, премахва елемента на индекса и връща новия списък.
removeAt :: Int -> [a] -> [a]
removeAt ind l
    | ind < 0 || ind >= length l = error "Index out of bounds"
    | otherwise = take ind l ++ drop (ind + 1) l
