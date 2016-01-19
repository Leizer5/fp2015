
module Main where
 
import System.Random
import Data.List (intercalate, find, minimumBy)
import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Monad (guard)
import Data.Ord (comparing)

-- Проверява дали има хоризонтална, вертикална или диагонална линия от X или О.
tictactoe :: String -> Bool
tictactoe a = tictactoeFor 'X' a /= tictactoeFor 'O' a

--Проверява дали има хоризонтална, вертикална или диагонална линия за играч "n"
-- check if there is a horizontal, vertical or diagonal line
-- for the given player "n"
tictactoeFor :: Char -> String -> Bool
tictactoeFor n [a,b,c,d,e,f,g,h,i] =
    [n,n,n] `elem` [[a,b,c],[d,e,f],[g,h,i],[a,d,g],
                    [b,e,h],[c,f,i],[a,e,i],[c,e,g]]
--Празна дъска
-- empty game board
start :: String 
start = "         "
--Проверява дали има X или O на посочената позиция
-- check if there is an X or an O at the given position
isPossible :: Int -> String -> Bool
isPossible n game = (game !! n) `notElem` "XO"
-- Поставя X или O на посочената позиция. "Right" + променена дъска = успешно поставяне, "Left" и не поменена дъска = неуспех.
-- try to place an X or an O at a given position.
-- "Right" + modified board means success, "Left" + unmodified board
-- means failure
place :: Int -> Char -> String -> Either String String
place i c game =
    if isPossible i game
    then Right $ take i game ++ [c] ++ drop (i + 1) game
    else Left game 
 
-- COMPUTER AI
-- взима броя ходове, започващи от дадена не-празна дъска и позицията на следжащия ход, докато играчът спечели или няма повече възможни ходове.
-- get the number of movements, starting from a given non-empty board
-- and a position for the next movement, until the specified player
-- wins or no movement is possible
-- the positions are chosen sequentially, so there's not much
-- intelligence here anyway
developGame :: Bool -> Int -> Int -> Char -> String -> (Int, Char, String)
developGame iterateMore moves i player game
    | i > 8 = 
        -- ако i стигне до последната позиция, обхождаме отново от 0, но само веднъж
        -- but do it only once
        if iterateMore
        then developGame False moves 0 player game
        -- Равенство
        -- draw game (after one iteration, still no winning moves)
        else (moves, player, game)
        -- Равенство или победа за играча
        -- draw game (game board full) or a win for the player
    | moves == 9 || tictactoeFor player game = (moves, player, game)
        -- изиграва се ход, ако е възможно и играта продължава
        -- make a move, if possible, and continue playing
    | otherwise = case place i otherPlayer game of
        -- позиция i не е празна. опитай отново със следващата позиция
        -- position i is not empty. try with the next position
        Left _ -> developGame iterateMore moves (i + 1)
                    otherPlayer game
        -- позиция i е празна, значи ходът е валиден, 
        -- следващия играч трябва да направи ход започващ от pos 0
        -- position i was empty, so it was a valid move.
        -- change the player and make a new move, starting at pos 0
        Right newGame -> developGame iterateMore (moves + 1) 0
                    otherPlayer newGame
        where
            otherPlayer = changePlayer player
 
-- COMPUTER AI
-- започващ от дадена не-празна дъска, опитва да познае кой ход ще доведе до най-бърза победа
-- starting from a given non-empty board, try to guess which position
-- could lead the player to the fastest victory.
bestMoveFor :: Char -> String -> Int
bestMoveFor player game = bestMove
    where
        -- докарва играта до край от всяка една стартова позиция
        -- drive the game to its end for each starting position
        continuations = [ (x, developGame True 0 x player game) |
            x <- [0..8] ]
        -- сравнява броя на ходове на играта и взима най-малкия
        -- compare the number of moves of the game and take the
        -- shortest one
        move (_, (m, _, _)) = m
        (bestMove, _) = minimumBy (comparing move) continuations
-- Проверява дали опонента има 2 заети полета в ред и дали 3-тото е свободно и поставя 
-- своят символ там, блокирайки опонента.
-- canBlock checks if the opponent has two pieces in a row and the
-- other cell in the row is empty, and places the player's piece there,
-- blocking the opponent
canBlock :: Char -> String -> Maybe Int
canBlock p [a,b,c,d,e,f,g,h,i] =
    listToMaybe $ mapMaybe blockable [[a,b,c],[d,e,f],[g,h,i],[a,d,g],
                                      [b,e,h],[c,f,i],[a,e,i],[c,e,g]]
    where
        blockable xs = do          
          guard $ length (filter (== otherPlayer) xs) == 2
          x <- find (`elem` "123456789") xs
          return $ digitToInt x
        otherPlayer = changePlayer p
-- сглобява дъска за принтиране на екрана
-- format a game board for on-screen printing
showGame :: String -> String
showGame [a,b,c,d,e,f,g,h,i] =
    topBottom ++
    "|    | 1 | 2 | 3 |\n" ++
    topBottom ++
    row "0" [[a],[b],[c]] ++
    row "3" [[d],[e],[f]] ++
    row "6" [[g],[h],[i]]
    where
        topBottom = "+----+---+---+---+\n"
        row n x = "| " ++ n ++ "+ | " ++
            intercalate " | " x ++ " |\n" ++ topBottom
-- Казва на играча да напише число и го превръша в int
-- ask the user to press a numeric key and convert it to an int
enterNumber :: IO Int
enterNumber = do
    c <- getChar
    if c `elem` "123456789"
    then do
        putStrLn ""
        return $ digitToInt c
    else do
        putStrLn "\nМоля въведете число!"
        enterNumber
-- Ход на играч-човек: взима броя на полета заети на дъската, следващото място което трябва да е заето,
-- и самата дъска и връша новото състояние на играта, проверявайки дали на това място може да бъде заето
-- ако не, играчът трябва да опита отново.
-- a human player's turn: get the number of pieces put on the board,
-- the next piece to be put (X or O) and a game board, and return
-- a new game state, checking if the piece can be placed on the board.
-- if it can't, make the user try again.
turn :: (Int, Char, String) -> IO (Int, Char, String)
turn (count, player, game) = do
    putStr $ "Моля кажете къде искате да поставите " ++
        [player] ++ ": "
    pos <- enterNumber
    case place (pos - 1) player game of
        Left oldGame -> do
            putStrLn "Тази позиция е вече заета!\n"
            turn (count, player, oldGame)
        Right newGame ->
            return (count + 1, changePlayer player, newGame)
-- сменя фигурата, която играчът ползва
-- alternate between X and O players
changePlayer :: Char -> Char
changePlayer 'O' = 'X'
changePlayer 'X' = 'O'
 
-- COMPUTER AI
-- прави автоматичен ход, поставяйки X или O на дъската. Първия ход винаги е случаен.
-- първо, компютърът търси 2 заети позиции на ред и се опитва да ги блокира.
-- ако няма се опитва да познае най-добрата позиция за следжащия си ход.
-- в краен случай поставя на случайна позиция.
-- make an automatic turn, placing an X or an O game board.
-- the first movement is always random.
-- first, the computer looks for two pieces of his opponent in a row
-- and tries to block.
-- otherwise, it tries to guess the best position for the next movement.
-- as a last resort, it places a piece randomly.
autoTurn :: Bool -> (Int, Char, String) -> IO (Int, Char, String)
autoTurn forceRandom (count, player, game) = do
    -- пробва случайна позиция, ако всичко останало не е проработило.
    -- try a random position 'cause everything else failed
    -- count == 0 overrides the value of forceRandom
    i <- if count == 0 || forceRandom
            then randomRIO (0,8)
            else return $
                case canBlock player game of
                    -- ако опонента не може да бъде блокиран, опитва да познае най-добрата позиция
                    -- за следжащ ход.
                    -- opponent can't be blocked. try to guess
                    -- the best movement
                    Nothing -> bestMoveFor player game
                    -- ако може да бъде блокиран го блокира.
                    -- opponent can be blocked, so just do it!
                    Just blockPos -> blockPos
    -- ако не може да играе на изчислената позиция, опитва отново с случайна.
    -- if trying to place a piece at a calculated position doesn't work,
    -- just try again with a random value
    case place i player game of
        Left oldGame -> autoTurn True (count, player, oldGame)
        Right newGame -> do
            putStrLn $ "Играчът " ++ [player] ++ " е на ход."
            return (count + 1, changePlayer player, newGame)
-- играта се играе докато някой спечели или дъската е пълна.
-- според стойността на "auto", казва на играча да направи своя ход, или го прави автоматично 
-- play a game until someone wins or the board becomes full.
-- depending on the value of the variable "auto", ask the user(s) to
-- put some pieces on the board or do it automatically
play :: Int -> (Int, Char, String) -> IO ()
play auto cpg@(_, player, game) = do
    newcpg@(newCount, newPlayer, newGame) <- case auto of
        -- ако и двамата играчи са хора, изписва команда
        -- if both players are human, always ask them
        0 -> turn cpg
        -- ако и двата са компютри, винаги играе автоматично
        -- if both players are computer, always play auto
        1 -> autoTurn False cpg
        -- ако първия играч е компютър, а втория човек.
        -- X is computer, O is human
        2 -> if player == 'X' then autoTurn False cpg else turn cpg
        -- ако пъривя играч е човек, а втория компютър.
        -- X is human, O is computer
        3 -> if player == 'O' then autoTurn False cpg else turn cpg
    putStrLn $ "\n" ++ showGame newGame
    if tictactoe newGame
    then putStrLn $ "Играчът " ++ [changePlayer newPlayer] ++ " печели!\n"
    else
        if newCount == 9
        then putStrLn "Равенство!\n"
        else play auto newcpg
-- main: изписва текст с правилата на играта и изисква да се въведе типът на желаната игра, започвайки 
-- от празна дъска.
-- main program: greet the user, ask for a game type, ask for the
-- player that'll start the game, and play the game beginning with an
-- empty board
main :: IO ()
main = do
    a <- getArgs
    if null a
    then usage
    else do
        let option = head a
        if option `elem` ["0","1","2","3"]
        then do
            putStrLn $ "\n" ++ showGame start
            let m = read option :: Int
            play m (0, 'X', start)
        else usage
 
usage :: IO ()
usage = do
    putStrLn "==|==========|=="
    putStrLn "  |МОРСКИ ШАХ|\n==|==========|==\n"
    putStrLn "Как искате да играете ?"
    putStrLn "Стартирайте програмата с една от посочените опции"
    putStrLn "0 : Човек срещу човек."
    putStrLn "1 : Компютър срещу компютър."
    putStrLn "2 : играчът с X е компютър, а играчът с O е човек"
    putStrLn "3 : играчът с O е компютър, а играчът с X е човек"
