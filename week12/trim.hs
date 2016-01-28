--Функцията трябва да премахне всяко срещане на whitespace от началото и края на низа. 
trim :: String -> String
trim ss = reverse ( dropWhile (\s -> s==' ') (reverse (dropWhile (\s -> s==' ') ss))) 
