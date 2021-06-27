module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  xs <- getArgs
  checkArgs xs

checkArgs :: [[Char]] -> IO ()
checkArgs xs
  | xs == [] = startGame 0
  | xs!!0 == "--help" || xs!!0 == "-h" = printArgUsage
  | xs!!0 == "1" = startGame 1
  | xs!!0 == "2" = startGame 2
  | xs!!0 == "3" = startGame 3
  | otherwise = startGame 0

printArgUsage :: IO ()
printArgUsage = do
  putStrLn("You can use the following arguments: 1, 2 and 3")
  putStrLn("1 = player vs player")
  putStrLn("2 = player vs computer")
  putStrLn("3 = computer vs computer")
