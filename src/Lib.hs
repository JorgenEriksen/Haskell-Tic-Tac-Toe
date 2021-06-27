module Lib
    ( startGame, opponents, aiTurn, availableSlots, rotateLeft, rotateRight, swap1and3, changePlayer, newBoard, blankBoard
    ) where

import Data.Char (digitToInt)
import Data.Typeable
import System.Random (randomRIO)
import System.IO.Unsafe

-- prints out the start message and starts the first turn
startGame :: Int -> IO ()
startGame m = do
  putStrLn("")
  putStrLn("**************************************************************************************")
  putStrLn("welcome to tick tack toe")
  modeMessages m
  putStrLn("if you want to see the arguments for mode, start the game with -h or --help at the end")
  putStrLn("")
  putStrLn("o starts")
  putStrLn("**************************************************************************************")
  putStrLn("")
  turn blankBoard 'o' (opponents m)

-- prints out the mode message
modeMessages :: Int -> IO ()
modeMessages m
  | m == 1 = putStrLn("mode is: player vs player")
  | m == 2 = putStrLn("mode is: player vs computer")
  | m == 3 = putStrLn("mode is: computer vs computer")
  | otherwise = putStrLn("mode is: player vs computer")

-- | returns the mode from number argument
--
-- >>> opponents 1
-- ('p','p')
--
-- >>> opponents 2
-- ('c','p')
--
-- >>> opponents 3
-- ('c','c')
opponents :: Int -> (Char, Char)
opponents m
  | m == 1 = ('p','p')
  | m == 2 = ('c','p')
  | m == 3 = ('c','c')
  | otherwise = ('c','p')

-- when a new turn starts
turn :: [Char] -> Char -> (Char, Char) -> IO ()
turn xs p t = do
  printBoard xs
  putStrLn("")
  command <- aiOrHumanCommand p t (availableSlots xs)
  checkMarkCommand xs command p t

-- checks if it should get input from ai or human
aiOrHumanCommand :: Char -> (Char, Char) -> [Int] -> IO String
aiOrHumanCommand p t xs
  | p == 'x' && snd t == 'p' = humanCommand
  | p == 'x' && snd t == 'c' = aiCommand xs
  | p == 'o' && fst t == 'p' = humanCommand
  | p == 'o' && fst t == 'c' = aiCommand xs

-- gets input with getLine
humanCommand :: IO String
humanCommand = do
  x <- getLine
  return x

-- generates ai command
aiCommand :: [Int] -> IO String
aiCommand xs = do
  mark <- aiMark xs
  turnRnd <- random 5
  putStrLn $ (show ((xs!!mark)+1)) ++ aiTurn turnRnd
  return $ (show ((xs!!mark)+1)) ++ aiTurn turnRnd

-- random mark in the available spaces
aiMark :: [Int] -> IO Int
aiMark xs = random ((length xs)-1)

-- | returns a turn command for AI if randam number equals 0 or 1
--
-- >>> aiTurn 0
-- " left"
--
-- >>> aiTurn 1
-- " right"
--
-- >>> aiTurn 2
-- ""
aiTurn :: Int -> [Char]
aiTurn r
  | r == 0 = " left"
  | r == 1 = " right"
  | otherwise = ""

-- random function
random :: Int -> IO Int
random x = randomRIO (0, x)

-- | returns the available indexes in board
--
-- >>> availableSlots ['_', 'x', '_', '_', '_', '_', 'o', '_', '_']
-- [0,2,3,4,5,7,8]
--
-- >>> availableSlots ['_', '_', 'x', 'x', 'o', '_', 'o', '_', '_']
-- [0,1,5,7,8]
availableSlots :: [Char] -> [Int]
availableSlots xs = [i | (i,x) <- zip [0..] xs, x == '_']

-- checks mark input
checkMarkCommand :: [Char] -> [Char] -> Char -> (Char, Char) ->  IO ()
checkMarkCommand xs x p t
  | 0 < digitToInt (head x) && digitToInt (head x) < 10 && checkIfMarkIsAvailable xs (head x) = setMark xs x p t
  | otherwise = gameOver (changePlayer p)

-- checks rotate input
checkRotateCommand :: [Char] -> [Char] -> Char -> (Char, Char) -> IO ()
checkRotateCommand xs x p t
  | length x == 1 = turn xs (changePlayer p) t
  | x!!1 == ' ' && drop 2 x == "left" = rotateLeftCommand xs x p t
  | x!!1 == ' ' && drop 2 x == "right" = rotateRightCommand xs x p t
  | otherwise = gameOver (changePlayer p)

-- rotate left command
rotateLeftCommand :: [Char] -> [Char] -> Char -> (Char, Char) -> IO ()
rotateLeftCommand xs x p t = turn (rotateLeft (swap1and3 xs)) (changePlayer p) t

-- | rotates the board to the left
--
-- >>> rotateLeft ['_', 'x', '_', 'o', 'x', '_', 'o', '_', '_']
-- "___xx__oo"
-- >>> rotateLeft ['x', '_', 'o', 'o', '_', 'x', 'o', 'x', '_']
-- "ox___xxoo"
rotateLeft :: [Char] -> [Char]
rotateLeft xs = [xs!!2] ++ [xs!!5] ++ [xs!!8] ++ [xs!!1] ++ [xs!!4] ++ [xs!!7] ++ [xs!!0] ++ [xs!!3] ++ [xs!!6]
--rotateLeft xs = [xs!!2] ++ [xs!!5] ++ [xs!!8] ++ [xs!!1] ++ [xs!!4] ++ [xs!!7] ++ [xs!!0] ++ [xs!!3] ++ [xs!!6]

-- rotate right command
rotateRightCommand :: [Char] -> [Char] -> Char -> (Char, Char) -> IO ()
rotateRightCommand xs x p t = turn (rotateRight (swap1and3 xs)) (changePlayer p) t

-- | rotates the board to the right
--
-- >>> rotateRight ['_', 'x', 'x', '_', 'o', '_', 'o', '_', '_']
-- "o___ox__x"
-- >>> rotateRight ['x', '_', 'o', 'o', '_', 'x', 'o', 'x', '_']
-- "ooxx___xo"
rotateRight:: [Char] -> [Char]
rotateRight xs = [xs!!6] ++ [xs!!3] ++ [xs!!0] ++ [xs!!7] ++ [xs!!4] ++ [xs!!1] ++ [xs!!8] ++ [xs!!5] ++ [xs!!2]

-- | swaps position 1 and 3 (index 0 and 2) in board
-- >>> swap1and3 ['x', '_', 'o', 'o', '_', 'x', 'o', 'x', '_']
-- "o_xo_xox_"
swap1and3 :: [Char] -> [Char]
swap1and3 xs = [xs!!2] ++ [xs!!1] ++ [xs!!0] ++ (drop 3 xs)

-- | swaps position 1 and 3 (index 0 and 2) in board
--
-- >>> checkIfMarkIsAvailable ['x', '_', 'o', 'o', '_', 'x', 'o', 'x', '_'] '3'
-- False
--
checkIfMarkIsAvailable :: [Char] -> Char -> Bool
checkIfMarkIsAvailable xs x
  | xs!!((digitToInt x)-1) == '_' = True
  | otherwise = False

-- | change the current player between x and o
-- >>> changePlayer 'x'
-- 'o'
-- >>> changePlayer 'o'
-- 'x'
changePlayer :: Char -> Char
changePlayer x
  | x == 'x' = 'o'
  | otherwise = 'x'

-- checks the board if its a win
checkIfWin :: [Char] -> [Char] -> Char -> (Char, Char) -> IO ()
checkIfWin xs x p t
  | checkRows xs p || checkColumns xs p || checkDiagonal xs p = gameOver p
  | not ('_' `elem` xs) = draw
  | otherwise = checkRotateCommand xs x p t

-- sets mark on board
setMark :: [Char] -> [Char] -> Char -> (Char, Char) -> IO ()
setMark xs x p t = checkIfWin (newBoard xs (digitToInt (head x)-1) p) x p t

-- | creates the new board with the new mark
-- >>> newBoard ['_', 'x', '_', '_', 'x', '_', 'o', '_', '_'] 3 'o'
-- "_x_ox_o__"
newBoard :: [Char] -> Int -> Char -> [Char]
newBoard xs i p = (take i xs) ++ [p] ++ (drop (i + 1) xs)

-- | checks for 3 in a row in rows
-- >>> checkRows ['_', 'o', '_', 'x', 'x', 'x', 'o', 'o', '_'] 'x'
-- True
-- >>> checkRows ['_', 'o', 'x', 'x', 'o', 'x', 'o', 'o', '_'] 'x'
-- False
checkRows :: [Char] -> Char -> Bool
checkRows xs x
  | xs!!0 == x && xs!!1 == x && xs!!2 == x = True
  | xs!!3 == x && xs!!4 == x && xs!!5 == x = True
  | xs!!6 == x && xs!!7 == x && xs!!8 == x = True
  | otherwise = False

-- | checks for 3 in a row in columns
-- >>> checkColumns ['x', 'o', '_', 'x', 'o', 'x', 'o', 'o', 'x'] 'o'
-- True
-- >>> checkColumns ['_', 'o', 'x', 'x', 'o', 'x', 'o', 'x', '_'] 'x'
-- False
checkColumns :: [Char] -> Char -> Bool
checkColumns xs x
  | xs!!0 == x && xs!!3 == x && xs!!6 == x = True
  | xs!!1 == x && xs!!4 == x && xs!!7 == x = True
  | xs!!2 == x && xs!!5 == x && xs!!8 == x = True
  | otherwise = False

-- | checks for 3 in a row in diagonal
-- >>> checkColumns ['x', 'o', 'o', '_', 'o', 'x', 'x', 'o', 'x'] 'o'
-- True
-- >>> checkColumns ['_', 'o', 'x', 'x', 'o', 'x', 'o', 'x', '_'] 'o'
-- False
checkDiagonal :: [Char] -> Char -> Bool
checkDiagonal xs x
  | xs!!0 == x && xs!!4 == x && xs!!8 == x = True
  | xs!!2 == x && xs!!4 == x && xs!!6 == x = True
  | otherwise = False

-- game over message
gameOver :: Char -> IO ()
gameOver p = putStrLn("GAME OVER " ++ [p] ++ " WON")

-- draw message
draw :: IO ()
draw = putStrLn("GAME OVER DRAW")

-- function not used, but can be used if you want to display board after both players have played
checkPrintBoard :: [Char] -> Char -> IO ()
checkPrintBoard xs p
  | p == 'o' || xs == blankBoard = return ()
  | otherwise = printBoard xs

-- | creates a blank board
-- >>> blankBoard
-- "_________"
blankBoard :: [Char]
blankBoard = ['_', '_', '_', '_', '_', '_', '_', '_', '_']

-- prints the board
printBoard :: [Char] -> IO ()
printBoard xs = do
    putStrLn ("# " ++ [xs!!0] ++ " " ++ [xs!!1] ++ " " ++ [xs!!2])
    putStrLn ("# " ++ [xs!!3] ++ " " ++ [xs!!4] ++ " " ++ [xs!!5])
    putStrLn ("# " ++ [xs!!6] ++ " " ++ [xs!!7] ++ " " ++ [xs!!8])