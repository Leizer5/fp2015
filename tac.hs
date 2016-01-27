-- Валери Николов ф.н 81229
-- Дъската е списък съдържащ елементите:
-- e0, e1, e2
-- e3, e4, e5
-- e6, e7, e8
-- и във всеки може да се запише 0 (празна позиция) 1 (кръстче) или 2 (нула)
--
-- показва образите на кръстчетата и нулите в конзолата
sign :: Int -> String
sign 1 = " X "
sign (-1) = " O "
sign n = "   "
-- рисува дъската чрез елементите на "board"
drawgame :: [ Int ] -> String
drawgame board = "\n" ++ sign (head board) ++ "|" ++ sign (board !! 1) ++ "|" ++ sign (board !! 2) 
        ++ "\n" ++ "---+---+---" ++ "\n" ++  sign (board !! 3) ++ "|" ++ sign (board !! 4) 
        ++ "|" ++ sign (board !! 5) ++ "\n" ++ "---+---+---" ++ "\n" ++
        sign (board !! 6) ++ "|" ++ sign (board !! 7) ++ "|" ++ sign (last board) ++ "\n"
-- текстът, който принтираме при край на играта
gameover :: Int -> String
gameover 1 = "Играч X печели!"
gameover (-1) = "Играч О печели!"
gameover n = "Равенство!"
-- проверява дали играта е прикючила (всички ходове са изиграни)
checkgameover :: [ Int ] -> Bool
checkgameover board -- ако, в която и да е позиция присъства 0, играта не е приключила
        | 0 `elem` board = False
        | otherwise = True                                        
-- Проверява дали играта е спечелена от играч и връща резултат тип bool.
checkwinner :: Int -> [ Int ] -> Bool
checkwinner ch board 
        -- проверка на редовете
        | head board == ch && board !! 1 == ch && board !! 2 == ch = True
        | board !! 3 == ch && board !! 4 == ch && board !! 5 == ch = True
        | board !! 6 == ch && board !! 7 == ch && last board == ch = True
        -- проверка на колоните
        | head board == ch && board !! 3 == ch && board !! 6 == ch = True
        | board !! 1 == ch && board !! 4 == ch && board !! 7 == ch = True
        | board !! 2 == ch && board !! 5 == ch && last board == ch = True
        -- проверка на главния диагонал
        | head board == ch && board !! 4 == ch && last board == ch = True
        -- проверка на вторичния диагонал
        | board !! 2 == ch && board !! 4 == ch && board !! 6 == ch = True
        -- ако няма победител
        | otherwise = False
-- ход на играч - връща новата дъска
playerturn :: Int -> Int -> [ Int ] -> [ Int ]
playerturn 0 ch board -- ако позицията е празна прави хода
        | head board == 0 = ch:tail board
        -- ако не, компютъра прави хода вместо играча.
        | otherwise = computerturn ch board                                          
playerturn 8 ch board  
        | last board == 0 = take 8 board ++ [ ch ]
        | otherwise = computerturn ch board
playerturn n ch board -- ако хода е невалиден, компютъра прави хода автоматично
        | n < 0 || n > 8 = computerturn ch board
        | board !! n == 0 = take n board ++ [ ch ] ++ drop (n+1) board                                        
        | otherwise = computerturn ch board
-- ход на компютър - връща новата дъска
computerturn :: Int -> [ Int ] -> [ Int ]
computerturn ch board
        -- ако може да спечели, поставя там
        -- проверка - първи ред
        | head board == ch && board !! 1 == ch && board !! 2 == 0 = take 2 board ++ [ ch ] ++ drop 3 board
        | head board == ch && board !! 1 == 0 && board !! 2 == ch = [head board] ++ [ ch ] ++ drop 2 board
        | head board == 0 && board !! 1 == ch && board !! 2 == ch = ch:tail board
        -- втори ред
        | board !! 3 == ch && board !! 4 == ch && board !! 5 == 0 = take 5 board ++ [ ch ] ++ drop 6 board
        | board !! 3 == ch && board !! 4 == 0 && board !! 5 == ch = take 4 board ++ [ ch ] ++ drop 5 board
        | board !! 3 == 0 && board !! 4 == ch && board !! 5 == ch = take 3 board ++ [ ch ] ++ drop 4 board
        -- трети ред
        | board !! 6 == ch && board !! 7 == ch && last board == 0 = take 8 board ++ [ ch ]
        | board !! 6 == ch && board !! 7 == 0 && last board == ch = take 7 board ++ [ ch ] ++ drop 8 board
        | board !! 6 == 0 && board !! 7 == ch && last board == ch = take 6 board ++ [ ch ] ++ drop 7 board
        -- проверка - първа колона
        | head board == ch && board !! 3 == ch && board !! 6 == 0 = take 6 board ++ [ ch ] ++ drop 7 board
        | head board == ch && board !! 3 == 0 && board !! 6 == ch = take 3 board ++ [ ch ] ++ drop 4 board
        | head board == 0 && board !! 3 == ch && board !! 6 == ch = ch:tail board
        -- втора колона
        | board !! 1 == ch && board !! 4 == ch && board !! 7 == 0 = take 7 board ++ [ ch ] ++ drop 8 board
        | board !! 1 == ch && board !! 4 == 0 && board !! 7 == ch = take 4 board ++ [ ch ] ++ drop 5 board
        | board !! 1 == 0 && board !! 4 == ch && board !! 7 == ch = [head board] ++ [ ch ] ++ drop 2 board
        -- трета колона
        | board !! 2 == ch && board !! 5 == ch && last board == 0 = take 8 board ++ [ ch ]
        | board !! 2 == ch && board !! 5 == 0 && last board == ch = take 5 board ++ [ ch ] ++ drop 6 board
        | board !! 2 == 0 && board !! 5 == ch && last board == ch = take 2 board ++ [ ch ] ++ drop 3 board
        -- проверка - главен диагонал
        | head board == ch && board !! 4 == ch && last board == 0 = take 8 board ++ [ ch ]
        | head board == ch && board !! 4 == 0 && last board == ch = take 4 board ++ [ ch ] ++ drop 5 board
        | head board == 0 && board !! 4 == ch && last board == ch = ch:tail board
        -- проверка - вторичен диагонал
        | board !! 2 == ch && board !! 4 == ch && board !! 6 == 0 = take 6 board ++ [ ch ] ++ drop 7 board
        | board !! 2 == ch && board !! 4 == 0 && board !! 6 == ch = take 4 board ++ [ ch ] ++ drop 5 board
        | board !! 2 == 0 && board !! 4 == ch && board !! 6 == ch = take 2 board ++ [ ch ] ++ drop 3 board
        -- блокира опонента, ако ще спечели
        -- проверка - първи ред
        | head board == -ch && board !! 1 == -ch && board !! 2 == 0 = take 2 board ++ [ ch ] ++ drop 3 board
        | head board == -ch && board !! 1 == 0 && board !! 2 == -ch = [head board] ++ [ ch ] ++ drop 2 board
        | head board == 0 && board !! 1 == -ch && board !! 2 == -ch = ch:tail board
        -- втори ред
        | board !! 3 == -ch && board !! 4 == -ch && board !! 5 == 0 = take 5 board ++ [ ch ] ++ drop 6 board
        | board !! 3 == -ch && board !! 4 == 0 && board !! 5 == -ch = take 4 board ++ [ ch ] ++ drop 5 board
        | board !! 3 == 0 && board !! 4 == -ch && board !! 5 == -ch = take 3 board ++ [ ch ] ++ drop 4 board
        -- трети ред
        | board !! 6 == -ch && board !! 7 == -ch && last board == 0 = take 8 board ++ [ ch ]
        | board !! 6 == -ch && board !! 7 == 0 && last board == -ch = take 7 board ++ [ ch ] ++ drop 8 board
        | board !! 6 == 0 && board !! 7 == -ch && last board == -ch = take 6 board ++ [ ch ] ++ drop 7 board
        -- проверка - първа колона
        | head board == -ch && board !! 3 == -ch && board !! 6 == 0 = take 6 board ++ [ ch ] ++ drop 7 board
        | head board == -ch && board !! 3 == 0 && board !! 6 == -ch = take 3 board ++ [ ch ] ++ drop 4 board
        | head board == 0 && board !! 3 == -ch && board !! 6 == -ch = ch:tail board
        -- втора колона
        | board !! 1 == -ch && board !! 4 == -ch && board !! 7 == 0 = take 7 board ++ [ ch ] ++ drop 8 board
        | board !! 1 == -ch && board !! 4 == 0 && board !! 7 == -ch = take 4 board ++ [ ch ] ++ drop 5 board
        | board !! 1 == 0 && board !! 4 == -ch && board !! 7 == -ch = [head board] ++ [ ch ] ++ drop 2 board
        -- трета колона
        | board !! 2 == -ch && board !! 5 == -ch && last board == 0 = take 8 board ++ [ ch ]
        | board !! 2 == -ch && board !! 5 == 0 && last board == -ch = take 5 board ++ [ ch ] ++ drop 6 board
        | board !! 2 == 0 && board !! 5 == -ch && last board == -ch = take 2 board ++ [ ch ] ++ drop 3 board
        -- проверка - главен диагонал
        | head board == -ch && board !! 4 == -ch && last board == 0 = take 8 board ++ [ ch ]
        | head board == -ch && board !! 4 == 0 && last board == -ch = take 4 board ++ [ ch ] ++ drop 5 board
        | head board == 0 && board !! 4 == -ch && last board == -ch = ch:tail board
        -- проверка - вторичен диагонал
        | board !! 2 == -ch && board !! 4 == -ch && board !! 6 == 0 = take 6 board ++ [ ch ] ++ drop 7 board
        | board !! 2 == -ch && board !! 4 == 0 && board !! 6 == -ch = take 4 board ++ [ ch ] ++ drop 5 board
        | board !! 2 == 0 && board !! 4 == -ch && board !! 6 == -ch = take 2 board ++ [ ch ] ++ drop 3 board                                
        -- ако центъра е празен, запълни го
        | board !! 4 == 0 = take 4 board ++ [ ch ] ++ drop 5 board
        -- иначе запълни някой от крайщата
        | head board == 0 = ch:tail board
        | board !! 2 == 0 = take 2 board ++ [ ch ] ++ drop 3 board
        | board !! 6 == 0 = take 6 board ++ [ ch ] ++ drop 7 board
        | last board == 0 = take 8 board ++ [ ch ]
        | board !! 1 == 0 = [head board] ++ [ ch ] ++ drop 2 board
        | board !! 3 == 0 = take 3 board ++ [ ch ] ++ drop 4 board
        | board !! 5 == 0 = take 5 board ++ [ ch ] ++ drop 6 board
        | board !! 7 == 0 = take 7 board ++ [ ch ] ++ drop 8 board
        -- ако нищо не е възможно просто върни дъската както си е
        | otherwise = board
-- главната функция за развитие на играта
play :: Int -> [ Int ] -> IO ()
play ch board =   
        if ch == 1 then do
                putStrLn "Твой ход е. Въведи грешна команда, за да оставиш компютъра да играе вместо теб."
                putStrLn "Въведи номера на желаната клетката, за да започнеш. (1..9)"
                pos <- getLine
                -- Прави хода на играча
                let newboard = playerturn (read pos - 1) ch board                                                    
                -- Принтира новата дъска
                putStrLn $ drawgame newboard
                -- проверка дали играта е приключила 
                if checkgameover newboard
                        then 
                        -- ако има победител изпиши кой е.
                                putStrLn $ gameover (if checkwinner ch newboard then ch else 0)
                        -- няма победител
                        else 
                                if checkwinner ch newboard
                                        then 
                                                putStrLn $ gameover ch
                                        else                                                            
                                                play (-ch) newboard                           
        else do
                let newboard = computerturn ch board    
                putStrLn "Компютърът е на ход: "
                putStrLn $ drawgame newboard
                -- проверява дали играта е приключила
                if checkgameover newboard
                        then 
                        -- ако има победител изпиши кой е
                                putStrLn $ gameover (if checkwinner ch newboard then ch else 0)
                        -- няма победител
                        else 
                                if checkwinner ch newboard
                                        then  
                                                putStrLn $ gameover ch
                                        else                                                            
                                                play (-ch) newboard
main = do
        let board = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        putStrLn "Морски Шах"
        putStrLn ""
        putStrLn $ drawgame board
        putStrLn "Ако искате да започнете пръв въведете '1' "
        choice <- getLine
        if choice == "1" 
                then play 1 board
                else play (-1) board
