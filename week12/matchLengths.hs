--Напишете предикат matchLengths, който приема два непразни списъка l1 и l2 с еднаква дължина.
matchLengths :: [[Int]] -> [[Int]] -> Bool
matchLengths [] [] = True
matchLengths [x1,y1] [x2,y2] = ((length x1) - (length x2)) == ((length y1) - (length y2))
matchLengths (x1:y1:z1) (x2:y2:z2) = ((length x1) - (length x2)) == ((length y1) - (length y2)) && (matchLengths (y1:z1) (y2:z2))
