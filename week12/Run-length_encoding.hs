--Run-Length encoding е прост начин за компресия на текст, при който последователните срещания на един елемент се заменят с 
--<брой на срещанията><елемента>, а в случай че тази замяна би заела повече символи, отколкото оригиналния текст 
-- се запазва оригиналния текст. Да се напише функция lengthEncode str, която компресира низ по зададения метод.
lengthEncode :: String -> String
lengthEncode [] = []
lengthEncode (s:ss)
 |length (takeWhile (\k -> k==s) (s:ss)) == 1 =  s : lengthEncode (dropWhile (\k -> k==s) ss)
 |otherwise = show (length (takeWhile (\k -> k==s) (s:ss)) ) ++ s : lengthEncode (dropWhile (\k -> k==s) ss)
