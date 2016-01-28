lengthDecode :: String -> String
lengthDecode [] = []
lengthDecode (s:ss) 
 |isDigit s = (repeat (toDigit (takeWhile isDigit (s:ss) ) ) (dropWhile isDigit (s:ss)) ) ++ lengthDecode (dropWhile isDigit ss)
 |otherwise = s : lengthDecode ss
 where 
  repeat :: Int -> String -> String
  repeat _ [] = []
  repeat 1 _ = []
  repeat i (x:xs) = x : (repeat (i - 1) (x:xs))
  isDigit :: Char ->Bool
  isDigit k = (k >= '0' && k <= '9')
  toDigit:: String -> Int
  toDigit s = read s::Int
